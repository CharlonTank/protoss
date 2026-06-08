# Protoss Web Alpha

Executable OCaml/Dune prototype for graph-first, content-addressed Protoss apps.

What works now:

- The pure core remains total: typed AST, canonical DefIds, stable hashes, deterministic normalization, explicit `Process` effects, typed capability descriptors, atomic patches, project stores, diff, and audit.
- Workspaces use `protoss.toml`; `project build` writes `.protoss/store` with canonical defs, `program.canon`, `program.graph.json`, types, deps, normal forms, roots, build refs, and web markers. Content refs use the declared `sha256`/`p2:` hash contract.
- `project lock` writes `.protoss/lock`, a deterministic content-addressed lockfile over the package metadata, canonical format versions, hash algorithm/prefix, program hash, graph hash, DefIds, source unit hashes, imports, exported interface constraints, imported contract hashes, and capabilities. `project lock --check` and `project build --locked` reject drift without rewriting the lockfile or store.
- `project package` writes `.protoss/packages/<hash>.package` plus `.protoss/package`, and stores the verified public interface as `.protoss/interfaces/<hash>.interface.json` plus `.protoss/interface`. The package descriptor is deterministic and content-addressed over the lock hash, canonical format versions, hash algorithm/prefix, program and graph hashes, public interface hash, declared `package_imports`, declared `package_interfaces`, declared `package_contracts`, type aliases, DefIds, capability scopes, and source units. Local package imports use `package_imports = ["name=path"]` and are locked by package ref, interface hash, and contract hash. Imported packages are rebuilt for verification and source drift is rejected. The public interface hash includes exported canonical types, type hashes, named type aliases, and capability scopes. `project package --locked` requires the existing lockfile to match before writing; `project package --check` verifies the current package hash, descriptor, interface pointer, and interface JSON artifact against the lock, current canonical build, imported package descriptors, any `package_interfaces = ["name=p2:..."]` constraints, and any imported `package_contracts = ["name=p2:..."]` constraints in `protoss.toml`. `project interface` validates the current package descriptor, then prints its public exports and locked package imports; `project interface --json` prints the same verified contract as a versioned JSON object with exported capability descriptors and a portable `contractHash`; `project interface --check <file>` validates a saved interface contract against the current package.
- Canonical graph JSON can be round-tripped back to `program.canon` with `canon --from-graph`. It includes the `sha256`/`p2:` hash contract and a versioned `nodeGraph` table with content-addressed `Type`/`Term` nodes, `typeRef`/`termRef` roots, deterministic sharing, and audit-time validation. `check --graph`, `hash --graph`, `nf --graph`, `eval --graph`, `run --graph`, and `resume --graph` load this graph directly without reparsing `.protoss` text.
- `defrec` supports only structural Nat/List recursion and desugars to `foldNat` or `foldList`; malformed or self-recursive definitions are rejected.
- Web apps are checked by convention: `init : Process Model`, `update : Msg -> Model -> Process Model`, and `view : Model -> View Msg`.
- Source-level type aliases work with `(type Name Type)` and parametric aliases like `(type Maybe (A) (Variant (None Unit) (Some A)))`. Named records and variants also work as alias syntax: `(record Model (name String))`, `(record Pair (params A B) (first A) (second B))`, and `(variant Maybe (params A) (None Unit) (Some A))`. Aliases are expanded before canonical hashing, so alias names do not affect DefIds or program hashes.
- Records can be destructured with `(letRecord recordExpr (field (source binder) ...) body)`. It elaborates to one record `let` plus canonical `get` field accesses, so destructuring field order and binder names do not affect the graph beyond the body references they bind.
- `match` is surface syntax over existing eliminators: Bool/variant branches elaborate to `case`, list `(Nil ...)`/`(Cons head tail ...)` branches elaborate to `caseList`, and a single `((record field (source binder) ...) body)` branch elaborates to `letRecord`. No canonical `match` node is introduced.
- Named variants may be recursively self-referential when recursive occurrences are guarded by a variant constructor, for example a finite `Tree A` with `Leaf A` and `Node (Tree A) (Tree A)`. Unguarded recursive type aliases are rejected.
- Recursive named variants can be consumed with `foldVariant`; branch-local `recur` is accepted only for direct structural subterms of the current constructor payload, and non-structural recursion is rejected.
- Polymorphic value definitions work with explicit type application, for example `(defpoly id (params A) (-> A A) (lambda (x A) x))` and `((inst id Nat) 4)`. Calls such as `(id 4)`, `(some 9)`, and `((List.map xs) (lambda x (succ x)))` infer type arguments when arguments or the expected result type make them unambiguous. The elaborated canonical graph still uses explicit `inst`, so inferred and explicit sources hash the same.
- Lambdas can omit parameter annotations when an expected function type is available, for example `(def inc (-> Nat Nat) (lambda x (succ x)))`, `foldNat`/`foldList` steps, `bind` continuations, and annotated local lets like `(let (inc (-> Nat Nat) (lambda x (succ x))) (inc 1))`. They elaborate to the same canonical graph as annotated lambdas.
- List constructors can omit their item type under an expected `List A`, for example `(def xs (List Nat) (Cons 1 (Cons 2 Nil)))`. They elaborate to the same canonical graph as `(Cons Nat 1 (Cons Nat 2 (Nil Nat)))`.
- Lists support non-recursive pattern matching with `(caseList xs (Nil nilExpr) (Cons head tail consExpr))`; `head` and `tail` are alpha-stable binders in the `Cons` branch, and the form is represented in the canonical graph.
- The shipped prelude includes polymorphic `List.map`, `List.length`, `List.append`, `List.filter`, `List.reverse`, `List.any`, `List.all`, `List.member`, `List.find`, `Maybe.map`, `Maybe.map2`, `Maybe.withDefault`, `Maybe.isSome`, `Maybe.isNone`, `Maybe.andThen`, `Maybe.toResult`, `Result.map`, `Result.map2`, `Result.withDefault`, `Result.mapError`, `Result.andThen`, `Result.toMaybe`, `Result.isOk`, `Result.isErr`, `Pair.swap`, `Assoc.empty`, `Assoc.insert`, `Assoc.get`, `Assoc.contains`, `Assoc.keys`, `Assoc.values`, `Set.empty`, `Set.contains`, `Set.insert`, `Set.union`, `Set.remove`, `Set.intersect`, `Set.difference`, and a recursive `Json` ADT with constructors, object lookup, field decoding, and typed `Result` validators, plus monomorphic Nat/Bool/String helpers.
- Variant constructors can infer their variant type from an expected context, for example `(def value (Maybe Nat) (variant Some 4))`; the inferred form hashes like the explicit `(variant (Maybe Nat) Some 4)`.
- Variant `case`/`foldVariant` branches whose payload type is `Unit` can omit the payload binder, for example `(case maybe (None 0) (Some n n))`; non-`Unit` constructors still require a binder.
- Source-level modules work with `(module Name)` and `(export symbol ...)`. Module-local definitions and type aliases are namespace-qualified, and imports may only reference exported symbols directly.
- S-expression syntax errors include deterministic `line:column` locations. File loading and project builds preserve them as `path:line:column: message`; type errors are still localized to the definition when loaded from a file.
- `View msg` is a typed canonical UI type. Supported constructors are `text`, `image`, `button`, `input`, `column`, `row`, `list`, and `when`.
- UI/message mismatches are rejected statically by the typechecker.
- Web bundles are deterministic and include `index.html`, `protoss-runtime.js`, `protoss-app.json`, `protoss-graph.json`, `protoss-canon-graph.json`, `protoss-capabilities.json`, and `protoss-world.json`. The browser runtime interprets the embedded canonical graph for `view` and `update`; external effects suspend as typed requests exposed through the runtime ledger/request API.
- `Process` supports `AskHuman`, `HttpGet`, `ReadClock`, `SaveLocal`, `LoadLocal`, and `ServerRequest` request payloads. Capabilities are checked against the kernel catalog and exported with typed request/response signatures plus content-addressed capability/signature refs. Typed resume rejects wrong response tags.
- Ledger commands support inspect, replay, and diff over deterministic WorldRefs/EventRefs. Request events record and validate `capability`, `capability-ref`, `request-tag`, `request-signature-ref`, `request-payload-type`, `response-type`, request/continuation ids, suspended request payload, and `cap-scope` before insertion and during inspection. Resume events record `request-signature-ref` and `response-type`, then validate the typed host response against the suspended request before insertion and during inspection.
- Patch diagnostics include patch file paths, JSON syntax `line:column` locations, the failing operation number, operation kind, definition name, and field context for structural parse errors, dependency conflicts, capability validation, and definition-local kernel errors. Successful `patch apply` writes a deterministic content-addressed audit file under `store/patches/<patch-ref>.patch`, links it to the previous audit with `previous-ref`, and updates `store/patches/latest`; `patch audit` verifies and prints that chain, and the default `latest` audit must match the current store program hash. Project `audit` also verifies the latest patch audit when present. Rejected patches do not write audit artifacts.
- `invariants` runs executable checks over canonicalization, graph round-trip, graph-first loading, normalization, alpha-stability, typed `Process` resume, and typed ledger request/resume events.
- `invariants package <project>` checks package lock consistency, package descriptor freshness, package interface refs, package interface JSON artifacts, exported capability descriptors, `contractHash`, exported canonical type hashes, imported package freshness, package refs, and audit.
- Web patch validation checks `init/update/view`; Model shape changes require a pure `migrate_v1_v2`.

Main commands:

```sh
dune runtest --force

dune exec protoss -- app check examples/web/todo_app
dune exec protoss -- project build examples/web/todo_app --target web --stats
dune exec protoss -- project lock examples/web/todo_app
dune exec protoss -- project lock examples/web/todo_app --check
dune exec protoss -- project package examples/web/todo_app
dune exec protoss -- project package examples/web/todo_app --check
dune exec protoss -- project package examples/web/todo_app --locked
dune exec protoss -- project interface examples/web/todo_app
dune exec protoss -- project interface examples/web/todo_app --json > /tmp/todo_app.interface.json
dune exec protoss -- project interface examples/web/todo_app --check /tmp/todo_app.interface.json
dune exec protoss -- project build examples/web/todo_app --locked
dune exec protoss -- web build examples/web/todo_app --out dist/
dune exec protoss -- web inspect examples/web/todo_app
dune exec protoss -- audit examples/web/todo_app

dune exec protoss -- patch check examples/web/todo_app/.protoss/store patches/web/change_button_text.json
dune exec protoss -- patch apply examples/web/todo_app/.protoss/store patches/web/change_button_text.json
dune exec protoss -- patch audit examples/web/todo_app/.protoss/store
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
dune exec protoss -- invariants ledger examples/ask_human.protoss --entry askName --response String:Ada --ledger /tmp/protoss-ledger-invariant
dune exec protoss -- invariants ledger --graph /tmp/ask_human.protoss.graph.json --entry askName --response String:Ada --ledger /tmp/protoss-ledger-invariant-graph
dune exec protoss -- invariants package examples/workspace
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
dune exec protoss -- check examples/pattern_match.protoss
dune exec protoss -- check examples/record_destructure.protoss
dune exec protoss -- check examples/recursive_tree.protoss
dune exec protoss -- nf examples/recursive_tree.protoss
dune exec protoss -- check examples/stdlib_generics.protoss
dune exec protoss -- check examples/structural_recursion.protoss
dune exec protoss -- check examples/modules/app.protoss
dune exec protoss -- project build examples/workspace --stats
```
