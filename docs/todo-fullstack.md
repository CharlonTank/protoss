# Full-stack demo: the todo app and the priority patch

`examples/web/todo_app` is the end-to-end full-stack example: a web app with UI, local
storage, a typed Process model, and a structured patch that evolves it to carry a
per-item priority — **without editing the source**. The change is delivered as JSON and
applied to the content-addressed store. This page walks the demo, all verified against
the build (the demo script passed 15/15 here).

## The app

`examples/web/todo_app/protoss.toml`:

```toml
name = "todo-web-alpha"
version = "0.1.0"
entrypoints = ["src/app.protoss"]
stdlib = "../../../stdlib/prelude.protoss"
source_dirs = ["src"]
store_dir = ".protoss/store"
cache_dir = ".protoss/cache"
capabilities = ["Local.storage"]
```

`src/app.protoss` is a Process-architecture web app: `init`, `update`, `view`, with a
model `{ draft : String, items : List String, next : Nat }` and messages `AddTodo` /
`NewTodoChanged`. The `update` for `AddTodo` performs a `Local.save` effect (hence the
`Local.storage` capability) before extending the items list.

It app-checks:

```sh
_build/default/bin/main.exe app check examples/web/todo_app
```

```
App OK model=(Record (draft String) (items (List String)) (next Nat)) msg=(Variant (AddTodo Unit) (NewTodoChanged String)) architecture=process
```

## Building the web bundle

```sh
_build/default/bin/main.exe web build examples/web/todo_app --out /tmp/todo-dist
```

```
Web build p2:35cfd3950b9b8864d0ee9c1daeb6aa593f224e360ca53712232e32bb8c6abdfd
Out /tmp/todo-dist
CompiledArtifact p2:255017c4c4879e2c259aca5d243a32c190e047537786101ef54907591b42a7f6
```

The output directory contains the deterministic bundle (verified):

```
index.html
protoss-runtime.js
protoss-app.json
protoss-graph.json
protoss-canon-graph.json
protoss-host-contract.json
protoss-capabilities.json
protoss-world.json
protoss-compiled-artifact.txt
```

The browser runtime interprets the embedded canonical graph for `view`/`update` and
suspends external effects as typed requests through its ledger/request API. The
`compiledArtifact` ref is `derive(UniverseRoot, target=web, policy=web-default-v1)` — it
depends only on those inputs, so the same app always produces the same artifact ref.

## The priority patch

`examples/web/todo_app/patches/add_priority.json` is a single atomic **5-op batch** that
evolves each item from a `String` into a `{ label : String, priority : Low | High }`
record — across the whole app:

| op | kind | name | effect |
|---|---|---|---|
| 1 | `MigrateType` | `migrate_v1_v2` | pure `(-> v1Model v2Model)`; rewrites each old item `String` into `(Record (label String) (priority (Variant (Low Unit) (High Unit))))`, defaulting priority to `Low` |
| 2 | `ReplaceDef` | `init` | v2 Model shape (empty items list) |
| 3 | `ReplaceDef` | `update` | `AddTodo` now builds an item record with `High` priority |
| 4 | `ReplaceDef` | `view` | renders `(get item label)` for the new item shape |
| 5 | `AddDef` | `samplePrioritized` | `migrate_v1_v2` applied to a one-item v1 model, so `eval` shows a concrete migrated item |

This is the realistic shape of a schema migration: a pure migration function, the
init/update/view replaced to the new shape, and revalidation that the whole program still
typechecks.

## Run the demo (one command)

```sh
examples/web/todo_app/priority_demo.sh
```

It rebuilds a fresh store, applies the patch through the full content-addressed path, and
asserts on pinned hashes. Verified output:

```
== Todo-app add-priority demo (G7 / spec 14.4) ==
ok   build v1 store
ok   patch review lists MigrateType
ok   patch review lists samplePrioritized AddDef
ok   patch check valid
ok   patch apply accepted
ok   patch audit OK
ok   patch audit records 5 ops
ok   patch audit chains to v1 root
ok   store lists v2 init with priority field
ok   migrate_v1_v2 present with declared dep
ok   eval samplePrioritized shows concrete priority
ok   eval init produces v2 Model (empty items)
ok   project audit after apply
ok   graph invariants after apply
ok   duplicate apply rejected (PATCH001) (failed as expected)

== Summary ==
checks run: 15
RESULT: PASS (todo app evolved to carry priority via structured patch)
```

(The script removes the git-ignored `.protoss` store on exit; use `KEEP_STORES=1` to keep
it for inspection.)

## The proof points

The two assertions worth understanding:

**The migration produces concrete priority data.** Evaluating `samplePrioritized` from the
post-patch graph object shows a real migrated item:

```sh
_build/default/bin/main.exe eval --store-graph examples/web/todo_app \
  p2:4e944b11f190fe853cdf858ffcf7164469ce65c8ebce3648937cb092d824cb3c --entry samplePrioritized
```

```
samplePrioritized = {draft = "", items = [{label = "buy milk", priority = Low unit}], next = 1}
```

**Re-applying the batch fails (atomicity).** `samplePrioritized` already exists:

```
PATCH001 ... AddDef target already exists: samplePrioritized
```

## How this maps to the manual flow

The demo is the [patches.md](patches.md) flow applied to a real app:

1. `project build` the app store.
2. `patch review` / `patch check` the 5-op batch.
3. `patch apply` (atomic, audited; `patch audit` shows `ops=5` chained to the v1 root).
4. `store list` shows the evolved Model type carrying `(priority (Variant (High Unit)
   (Low Unit)))`.
5. `eval --store-graph` proves the migrated value; `audit` and `invariants graph` confirm
   whole-program revalidation.

The same change could be driven through the MCP server's `proposePatch` / `checkPatch` /
`applyPatch` tools (which require harness validation on apply) — see `docs/mcp.md`.
