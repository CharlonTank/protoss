# Protoss Web Alpha

Executable OCaml/Dune prototype for graph-first, content-addressed Protoss apps.

What works now:

- The pure core remains total: typed AST, canonical DefIds, stable hashes, deterministic normalization, explicit `Process` effects, capabilities, atomic patches, project stores, diff, and audit.
- Workspaces use `protoss.toml`; `project build` writes `.protoss/store` with canonical defs, types, deps, normal forms, roots, build refs, and web markers.
- Web apps are checked by convention: `init : Process Model`, `update : Msg -> Model -> Process Model`, and `view : Model -> View Msg`.
- Source-level type aliases work with `(type Name Type)` and parametric aliases like `(type Maybe (A) (Variant (None Unit) (Some A)))`. Aliases are expanded before canonical hashing, so alias names do not affect DefIds or program hashes.
- `View msg` is a typed canonical UI type. Supported constructors are `text`, `image`, `button`, `input`, `column`, `row`, `list`, and `when`.
- UI/message mismatches are rejected statically by the typechecker.
- Web bundles are deterministic and include `index.html`, `protoss-runtime.js`, `protoss-app.json`, `protoss-graph.json`, `protoss-capabilities.json`, and `protoss-world.json`.
- `Process` supports `AskHuman`, `HttpGet`, `ReadClock`, `SaveLocal`, `LoadLocal`, and `ServerRequest` request payloads. Typed resume rejects wrong response tags.
- Ledger commands support inspect, replay, and diff over deterministic WorldRefs/EventRefs.
- Web patch validation checks `init/update/view`; Model shape changes require a pure `migrate_v1_v2`.

Main commands:

```sh
dune runtest --force

dune exec protoss -- app check examples/web/todo_app
dune exec protoss -- project build examples/web/todo_app --target web --stats
dune exec protoss -- web build examples/web/todo_app --out dist/
dune exec protoss -- web inspect examples/web/todo_app
dune exec protoss -- audit examples/web/todo_app

dune exec protoss -- patch check examples/web/todo_app/.protoss/store patches/web/change_button_text.json
dune exec protoss -- patch apply examples/web/todo_app/.protoss/store patches/web/change_button_text.json
dune exec protoss -- patch check examples/web/todo_app/.protoss/store patches/web/invalid_msg_view_mismatch.json
dune exec protoss -- patch check examples/web/todo_app/.protoss/store patches/web/model_without_migration.json
dune exec protoss -- patch check examples/web/todo_app/.protoss/store patches/web/model_with_migration.json

dune exec protoss -- diff before.store after.store
dune exec protoss -- diff --json before.store after.store
dune exec protoss -- patch from-diff before.store after.store > patch.json

dune exec protoss -- ledger inspect <WorldRefOrEventRef>
dune exec protoss -- ledger replay <WorldRef>
dune exec protoss -- ledger diff <WorldRefA> <WorldRefB>

dune exec protoss -- fmt examples/web/todo_app/src/app.protoss
dune exec protoss -- fmt --check examples/web/todo_app/src/app.protoss
dune exec protoss -- graph examples/web/todo_app --out graph.json
dune exec protoss -- graph examples/web/todo_app --dot graph.dot
dune exec protoss -- explain WEB007
dune exec protoss -- bench build examples/web/todo_app
```

Compatibility commands from earlier MVPs still work:

```sh
dune exec protoss -- parse examples/basic.protoss
dune exec protoss -- check examples/basic.protoss
dune exec protoss -- nf examples/basic.protoss
dune exec protoss -- hash examples/alpha_a.protoss
dune exec protoss -- hash examples/alpha_b.protoss
dune exec protoss -- canon --version
dune exec protoss -- check examples/app.protoss
dune exec protoss -- project build examples/workspace --stats
```
