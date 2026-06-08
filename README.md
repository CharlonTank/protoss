# Protoss Web Alpha

Executable OCaml/Dune prototype for graph-first, content-addressed Protoss apps.

What works now:

- The pure core remains total: typed AST, canonical DefIds, stable hashes, deterministic normalization, explicit `Process` effects, typed capability descriptors, atomic patches, project stores, diff, and audit.
- Workspaces use `protoss.toml`; `project build` writes `.protoss/store` with canonical defs, `program.canon`, `program.graph.json`, types, deps, normal forms, roots, build refs, and web markers.
- Canonical graph JSON can be round-tripped back to `program.canon` with `canon --from-graph`.
- `defrec` supports only structural Nat/List recursion and desugars to `foldNat` or `foldList`; malformed or self-recursive definitions are rejected.
- Web apps are checked by convention: `init : Process Model`, `update : Msg -> Model -> Process Model`, and `view : Model -> View Msg`.
- Source-level type aliases work with `(type Name Type)` and parametric aliases like `(type Maybe (A) (Variant (None Unit) (Some A)))`. Named records and variants also work as alias syntax: `(record Model (name String))`, `(record Pair (params A B) (first A) (second B))`, and `(variant Maybe (params A) (None Unit) (Some A))`. Aliases are expanded before canonical hashing, so alias names do not affect DefIds or program hashes.
- Polymorphic value definitions work with explicit type application: `(defpoly id (params A) (-> A A) (lambda (x A) x))` and `((inst id Nat) 4)`. Type parameters are canonicalized as indexed variables, so their names do not affect hashes.
- The shipped prelude includes polymorphic `List.map`, `List.length`, `Maybe.map`, `Maybe.withDefault`, and `Result.map`, plus monomorphic Nat/Bool/String helpers.
- Variant constructors can infer their variant type from an expected context, for example `(def value (Maybe Nat) (variant Some 4))`; the inferred form hashes like the explicit `(variant (Maybe Nat) Some 4)`.
- Source-level modules work with `(module Name)` and `(export symbol ...)`. Module-local definitions and type aliases are namespace-qualified, and imports may only reference exported symbols directly.
- `View msg` is a typed canonical UI type. Supported constructors are `text`, `image`, `button`, `input`, `column`, `row`, `list`, and `when`.
- UI/message mismatches are rejected statically by the typechecker.
- Web bundles are deterministic and include `index.html`, `protoss-runtime.js`, `protoss-app.json`, `protoss-graph.json`, `protoss-canon-graph.json`, `protoss-capabilities.json`, and `protoss-world.json`.
- `Process` supports `AskHuman`, `HttpGet`, `ReadClock`, `SaveLocal`, `LoadLocal`, and `ServerRequest` request payloads. Capabilities are checked against the kernel catalog and exported with typed request/response signatures. Typed resume rejects wrong response tags.
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
dune exec protoss -- canon --graph examples/basic.protoss > /tmp/basic.protoss.graph.json
dune exec protoss -- canon --from-graph /tmp/basic.protoss.graph.json
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
dune exec protoss -- check examples/inferred_variants.protoss
dune exec protoss -- check examples/polymorphic_defs.protoss
dune exec protoss -- check examples/stdlib_generics.protoss
dune exec protoss -- check examples/structural_recursion.protoss
dune exec protoss -- check examples/modules/app.protoss
dune exec protoss -- project build examples/workspace --stats
```
