# Structured patches

You do not edit a Protoss store by hand. You change it with a **structured patch**: a
JSON document describing operations on definitions. Every patch goes through whole-program
validation, and a successful apply writes a content-addressed, hash-linked audit record.
This page documents the format and the full mutation flow, grounded in the golden
projects (which carry pinned hashes the test suite verifies).

## Why patches

The store is content-addressed and deterministic. A structured patch:

- is **validated as a whole program** (parse, canonicalize, typecheck, totality,
  capabilities, secrets, policies, affected harnesses) before anything is written;
- is **atomic** — a batch either applies fully or not at all;
- is **audited** — each apply links to the previous one and records the root state, so
  the history is a verifiable chain.

This is the only sanctioned mutation path; `protoss agent guard-write` rejects direct
writes to canonical/store internals.

## The patch ADT

The structural patch operations are:

`AddDef`, `ReplaceDef`, `DeleteDef`, `RenameDef`, `AddField`, `RemoveField`, `Inline`,
`Extract`, `AddHarness`, `AddCapability`, and `MigrateType`. Every variant goes through
whole-program validation and audit recording.

A patch file is either a single op (a JSON object with `"op"`) or a batch
(`{"ops": [...]}`).

## Anatomy of an op

An op names the target definition and supplies its new canonical pieces:

- `op` — the operation kind.
- `name` — target definition name.
- `deps` — the definition's dependencies; these **must exactly match** the canonical
  definition dependencies (mismatch raises `PATCH_DEPS`).
- `type` — the type, either an inline canonical type or `{"source": "..."}`.
- `expr` — the body, either inline canonical JSON or `{"source": "..."}`.

Bodies can be written two ways:

```json
"expr": ["succ", "base"]
```

or as source text that the patch loader parses:

```json
"expr": { "source": "(migrate_v1_v2 (record (title \"first item\")))" }
```

## Worked example 1 — `AddDef` (patch-demo)

`examples/golden/patch-demo` is a two-definition project (`base`, `label`). The patch
`patches/add_total.json` adds `total = (succ base)`:

```json
{
  "op": "AddDef",
  "name": "total",
  "deps": ["base"],
  "type": "Nat",
  "expr": ["succ", "base"]
}
```

### The flow

Run from the repository root, over a fresh store (the scenario is one-shot).

**1. Build the store:**

```sh
_build/default/bin/main.exe project build examples/golden/patch-demo
```

```
Build p2:11010968b8570735a85dbcb4fc55073c34141cf37fbcef943a6731d4c007f59b
UniverseRoot p2:199fb4342c933cae406203eeb111675fbc635164d1e5a37ce799972b443c57aa
Store <REPO>/examples/golden/patch-demo/.protoss/store
```

**2. Review the patch (human-readable, no validation):**

```sh
_build/default/bin/main.exe patch review examples/golden/patch-demo/patches/add_total.json
```

```
Patch review
ops: 1
op 1: AddDef
  name: total
  deps: [base]
  capabilities: []
  type: Nat
  expr:
    (succ base)
```

**3. Check the patch against the store (validate, no mutation):**

```sh
_build/default/bin/main.exe patch check examples/golden/patch-demo/.protoss/store examples/golden/patch-demo/patches/add_total.json
```

```
Patch valid p2:bb87817ece787c88ae2b9578ba95790ecd3e9f2dd0a8d2e78f357550cc50f6a1
```

**4. Apply the patch (atomic, audited):**

```sh
_build/default/bin/main.exe patch apply examples/golden/patch-demo/.protoss/store examples/golden/patch-demo/patches/add_total.json
```

```
Patch accepted p2:2f1183f04d08a51b3934168b9478cf1a202b1e1aaf5a986f1ee502daac76d0a0
```

**5. Verify the audit chain:**

```sh
_build/default/bin/main.exe patch audit examples/golden/patch-demo/.protoss/store
```

```
Patch audit OK p2:2f1183f04d08a51b3934168b9478cf1a202b1e1aaf5a986f1ee502daac76d0a0
protoss-patch-audit-v1
previous-ref=none
previous-root=p2:199fb4342c933cae406203eeb111675fbc635164d1e5a37ce799972b443c57aa
root-ref=p2:fa8564ff7e2b3661ca233ab570cdff59664e755016f0361a2ef723fc94f67163
program-hash=p2:cbccabe746dae87e6e6ae8d7323d7b16a94cc77c7871819d277c1d7d6f6c1eba
result=p2:bb87817ece787c88ae2b9578ba95790ecd3e9f2dd0a8d2e78f357550cc50f6a1
ops=1
op=1 kind=AddDef name=total target=total result=p2:bb87817ece787c88ae2b9578ba95790ecd3e9f2dd0a8d2e78f357550cc50f6a1
```

**6. The patched definition is in the store:**

```sh
_build/default/bin/main.exe store list examples/golden/patch-demo
```

```
base  p2:31f69a65... Nat deps=[]
label p2:cfffd954... String deps=[]
total p2:ccd0be0b... Nat deps=[base]
```

**7. Evaluate the patched program from its post-patch graph object:**

```sh
_build/default/bin/main.exe eval --store-graph examples/golden/patch-demo \
  p2:e2cbaf88897644a1b0f6d1d1fa463c63c66310692f476e0e4f719f3d79077e78 --entry total
```

```
total = 3
```

### Atomicity is a contract, not a bug

Re-applying the same `AddDef` on the already-patched store **fails** — `total` exists:

```sh
_build/default/bin/main.exe patch check examples/golden/patch-demo/.protoss/store examples/golden/patch-demo/patches/add_total.json
```

```
PATCH001 ... AddDef target already exists: total
```

That is the atomicity guarantee. The full scenario lives in
`examples/golden/patch-demo/VALIDATE.md` and is replayed by `examples/golden/run.sh`.

## Worked example 2 — a `MigrateType` batch (migration-demo)

`examples/golden/migration-demo` evolves a record schema. The source ships v1:

```scheme
(def initial (Record (title String)) (record (title "first item")))
(def describe (-> (Record (title String)) String)
  (lambda (item (Record (title String))) (get item title)))
(def headline String (describe initial))     ; NOT in the patch
```

The patch `patches/add_done_field.json` is a 3-op batch that evolves the schema to
`(Record (done Bool) (title String))`:

```json
{
  "ops": [
    {
      "op": "MigrateType",
      "name": "migrate_v1_v2",
      "deps": [],
      "type": { "source": "(-> (Record (title String)) (Record (done Bool) (title String)))" },
      "expr": { "source": "(lambda (old (Record (title String))) (record (done false) (title (get old title))))" }
    },
    {
      "op": "ReplaceDef",
      "name": "initial",
      "deps": ["migrate_v1_v2"],
      "type": { "source": "(Record (done Bool) (title String))" },
      "expr": { "source": "(migrate_v1_v2 (record (title \"first item\")))" }
    },
    {
      "op": "ReplaceDef",
      "name": "describe",
      "deps": [],
      "type": { "source": "(-> (Record (done Bool) (title String)) String)" },
      "expr": { "source": "(lambda (item (Record (done Bool) (title String))) (get item title))" }
    }
  ]
}
```

`patch review` lists the three ops in order (verified):

```
Patch review
ops: 3
op 1: MigrateType
  name: migrate_v1_v2
  ...
op 2: ReplaceDef
  name: initial
  ...
op 3: ReplaceDef
  name: describe
  ...
```

The key behaviors this scenario proves:

- **Whole-program revalidation.** `headline` is *not* in the patch, but it depends on the
  replaced `initial` and `describe`; the patch is accepted only because `headline` still
  typechecks against the new shapes. After apply, `headline` still evaluates.
- **The migration is a real value transform.** Evaluating `initial` from the post-patch
  graph yields the migrated record with the defaulted field:

  ```sh
  _build/default/bin/main.exe eval --store-graph examples/golden/migration-demo \
    p2:b034ad0ad777d67574e35570cc07d032fd258860fc5f3994cff3fae4327ca735 --entry initial
  ```

  ```
  initial = {done = false, title = "first item"}
  ```

The full pinned scenario is in `examples/golden/migration-demo/VALIDATE.md`.

## The audit chain

Each successful `patch apply` writes a content-addressed audit file under
`store/patches/<patch-ref>.patch` that:

- links to the previous audit with `previous-ref`,
- records `previous-root` / `root-ref` (the workspace root state before/after),
- writes content-addressed root-state and patch-provenance records under
  `store/provenance/`, plus a `patch-provenance` event in `store/provenance/world-ledger`,
- updates `store/patches/latest`.

`patch audit` (default `latest`) verifies the chain, and the latest audit must match the
current store program hash and latest root state. Project `audit` also verifies the
latest patch audit when present. Rejected patches write no audit artifacts.

## Deriving patches from diffs

You can produce a patch from two stores or a text diff instead of writing JSON by hand:

```sh
_build/default/bin/main.exe diff <store-a> <store-b>                  # structural store diff
_build/default/bin/main.exe diff --json <store-a> <store-b>
_build/default/bin/main.exe patch from-diff <store-a> <store-b> > patch.json
_build/default/bin/main.exe patch from-text-diff <store> <diff.patch> > patch.json
```

`patch from-text-diff` converts a single unambiguous human text diff into structural
`AddDef` / `ReplaceDef` / `DeleteDef` JSON, and rejects multi-definition or rename-shaped
textual edits with an intent error. Store diffs include JSON-pointer-style definition
paths, field-level `changedPaths`, and aggregate `affected` metadata.

## Diagnostics

Patch errors carry the patch file path, JSON syntax `line:column`, the failing operation
number, operation kind, definition name, field context, and embedded `expr.source`
line/column when a kernel type error maps back to that source. The public code family is
`PATCH001` (and `PATCH_DEPS` for dependency mismatches).

## The agent path

Agents mutate through `protoss agent commit`, which wraps `patch check` with mandatory
harness validation — see [harness.md](harness.md) and the MCP contract in `docs/mcp.md`.
The full end-to-end full-stack example (todo app gaining a per-item priority via a 5-op
batch) is in [todo-fullstack.md](todo-fullstack.md).
