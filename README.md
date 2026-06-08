# Protoss Web Alpha

Executable OCaml/Dune prototype for graph-first, content-addressed Protoss apps.

What works now:

- The pure core remains total: typed AST, canonical DefIds, stable hashes, deterministic normalization, explicit `Process` effects, typed capability descriptors, atomic patches, project stores, diff, and audit.
- Workspaces use `protoss.toml`; `project build` writes `.protoss/store` with canonical defs, `program.canon`, `program.graph.json`, types, deps, normal forms, roots, build refs, and web markers.
- `project lock` writes `.protoss/lock`, a deterministic content-addressed lockfile over the package metadata, canonical format versions, program hash, graph hash, DefIds, source unit hashes, imports, exports, and capabilities. `project lock --check` and `project build --locked` reject drift without rewriting the lockfile or store.
- Canonical graph JSON can be round-tripped back to `program.canon` with `canon --from-graph`. It includes a versioned `nodeGraph` table with content-addressed `Type`/`Term` nodes, `typeRef`/`termRef` roots, deterministic sharing, and audit-time validation. `check --graph`, `hash --graph`, `nf --graph`, `eval --graph`, `run --graph`, and `resume --graph` load this graph directly without reparsing `.protoss` text.
- `defrec` supports only structural Nat/List recursion and desugars to `foldNat` or `foldList`; malformed or self-recursive definitions are rejected.
- Web apps are checked by convention: `init : Process Model`, `update : Msg -> Model -> Process Model`, and `view : Model -> View Msg`.
- Source-level type aliases work with `(type Name Type)` and parametric aliases like `(type Maybe (A) (Variant (None Unit) (Some A)))`. Named records and variants also work as alias syntax: `(record Model (name String))`, `(record Pair (params A B) (first A) (second B))`, and `(variant Maybe (params A) (None Unit) (Some A))`. Aliases are expanded before canonical hashing, so alias names do not affect DefIds or program hashes.
- Records can be destructured with `(letRecord recordExpr (field (source binder) ...) body)`. It elaborates to one record `let` plus canonical `get` field accesses, so destructuring field order and binder names do not affect the graph beyond the body references they bind.
- Named variants may be recursively self-referential when recursive occurrences are guarded by a variant constructor, for example a finite `Tree A` with `Leaf A` and `Node (Tree A) (Tree A)`. Unguarded recursive type aliases are rejected.
- Recursive named variants can be consumed with `foldVariant`; branch-local `recur` is accepted only for direct structural subterms of the current constructor payload, and non-structural recursion is rejected.
- Polymorphic value definitions work with explicit type application, for example `(defpoly id (params A) (-> A A) (lambda (x A) x))` and `((inst id Nat) 4)`. Calls such as `(id 4)`, `(some 9)`, and `((List.map xs) (lambda x (succ x)))` infer type arguments when arguments or the expected result type make them unambiguous. The elaborated canonical graph still uses explicit `inst`, so inferred and explicit sources hash the same.
- Lambdas can omit parameter annotations when an expected function type is available, for example `(def inc (-> Nat Nat) (lambda x (succ x)))`, `foldNat`/`foldList` steps, `bind` continuations, and annotated local lets like `(let (inc (-> Nat Nat) (lambda x (succ x))) (inc 1))`. They elaborate to the same canonical graph as annotated lambdas.
- List constructors can omit their item type under an expected `List A`, for example `(def xs (List Nat) (Cons 1 (Cons 2 Nil)))`. They elaborate to the same canonical graph as `(Cons Nat 1 (Cons Nat 2 (Nil Nat)))`.
- Lists support non-recursive pattern matching with `(caseList xs (Nil nilExpr) (Cons head tail consExpr))`; `head` and `tail` are alpha-stable binders in the `Cons` branch, and the form is represented in the canonical graph.
- The shipped prelude includes polymorphic `List.map`, `List.length`, `Maybe.map`, `Maybe.withDefault`, and `Result.map`, plus monomorphic Nat/Bool/String helpers.
- Variant constructors can infer their variant type from an expected context, for example `(def value (Maybe Nat) (variant Some 4))`; the inferred form hashes like the explicit `(variant (Maybe Nat) Some 4)`.
- Variant `case`/`foldVariant` branches whose payload type is `Unit` can omit the payload binder, for example `(case maybe (None 0) (Some n n))`; non-`Unit` constructors still require a binder.
- Source-level modules work with `(module Name)` and `(export symbol ...)`. Module-local definitions and type aliases are namespace-qualified, and imports may only reference exported symbols directly.
- `View msg` is a typed canonical UI type. Supported constructors are `text`, `image`, `button`, `input`, `column`, `row`, `list`, and `when`.
- UI/message mismatches are rejected statically by the typechecker.
- Web bundles are deterministic and include `index.html`, `protoss-runtime.js`, `protoss-app.json`, `protoss-graph.json`, `protoss-canon-graph.json`, `protoss-capabilities.json`, and `protoss-world.json`. The browser runtime interprets the embedded canonical graph for `view` and `update`; external effects suspend as typed requests exposed through the runtime ledger/request API.
- `Process` supports `AskHuman`, `HttpGet`, `ReadClock`, `SaveLocal`, `LoadLocal`, and `ServerRequest` request payloads. Capabilities are checked against the kernel catalog and exported with typed request/response signatures. Typed resume rejects wrong response tags.
- Ledger commands support inspect, replay, and diff over deterministic WorldRefs/EventRefs. Request events validate that `cap-scope` uses known capabilities and contains the capability required by the recorded request before insertion and during inspection. Resume events record `response-type` and validate the typed host response against the suspended request before insertion and during inspection.
- `invariants` runs executable checks over canonicalization, graph round-trip, graph-first loading, normalization, alpha-stability, and typed `Process` resume.
- Web patch validation checks `init/update/view`; Model shape changes require a pure `migrate_v1_v2`.

Main commands:

```sh
dune runtest --force

dune exec protoss -- app check examples/web/todo_app
dune exec protoss -- project build examples/web/todo_app --target web --stats
dune exec protoss -- project lock examples/web/todo_app
dune exec protoss -- project lock examples/web/todo_app --check
dune exec protoss -- project build examples/web/todo_app --locked
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
dune exec protoss -- check --graph /tmp/basic.protoss.graph.json
dune exec protoss -- hash --graph /tmp/basic.protoss.graph.json
dune exec protoss -- eval --graph /tmp/basic.protoss.graph.json --entry main
dune exec protoss -- canon --graph examples/ask_human.protoss > /tmp/ask_human.protoss.graph.json
dune exec protoss -- run --graph /tmp/ask_human.protoss.graph.json --entry askName --ledger /tmp/protoss-ledger
dune exec protoss -- resume --graph /tmp/ask_human.protoss.graph.json --entry askName --event <EventRef> --response String:Ada --ledger /tmp/protoss-ledger
dune exec protoss -- invariants file examples/basic.protoss
dune exec protoss -- invariants graph /tmp/basic.protoss.graph.json
dune exec protoss -- invariants alpha examples/alpha_a.protoss examples/alpha_b.protoss
dune exec protoss -- invariants process examples/ask_human.protoss --entry askName --response String:Ada
dune exec protoss -- invariants process --graph /tmp/ask_human.protoss.graph.json --entry askName --response String:Ada
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
dune exec protoss -- check examples/polymorphic_inference.protoss
dune exec protoss -- check examples/inferred_lambdas.protoss
dune exec protoss -- check examples/list_case.protoss
dune exec protoss -- check examples/record_destructure.protoss
dune exec protoss -- check examples/recursive_tree.protoss
dune exec protoss -- nf examples/recursive_tree.protoss
dune exec protoss -- check examples/stdlib_generics.protoss
dune exec protoss -- check examples/structural_recursion.protoss
dune exec protoss -- check examples/modules/app.protoss
dune exec protoss -- project build examples/workspace --stats
```
