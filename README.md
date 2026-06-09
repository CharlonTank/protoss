# Protoss Web Alpha

Executable OCaml/Dune prototype for graph-first, content-addressed Protoss apps.

What works now:

- The pure core remains total: typed AST, canonical DefIds, stable hashes, deterministic normalization, explicit `Process` effects, typed capability descriptors, atomic patches, project stores, diff, and audit.
- Workspaces use `protoss.toml`; `project build` writes `.protoss/store` with canonical defs, `program.canon`, `program.graph.json`, `host.contract.json`, content-addressed graph objects under `graphs/`, content-addressed host contracts under `host-contracts/`, types, deps, normal forms, roots, build refs, and web markers. Content refs use the declared `sha256`/`p2:` hash contract. Host contracts include typed JSON codec refs for request payloads and responses. Stored graph objects can be inserted with `store graph-put`, listed with `store graphs`, or read and validated with `store graph <graphHash>`. Project audit validates every graph object present under `graphs/` plus the current content-addressed host contract.
- `project lock` writes `.protoss/lock`, a deterministic content-addressed lockfile over the package metadata, canonical format versions, hash algorithm/prefix, program hash, graph hash, host contract hash, DefIds, source unit hashes, imports, exported interface constraints, imported contract hashes, and capabilities. `project lock --check` and `project build --locked` reject drift without rewriting the lockfile or store.
- `project package` writes `.protoss/packages/<hash>.package` plus `.protoss/package`, and stores the verified public interface as `.protoss/interfaces/<hash>.interface.json` plus `.protoss/interface`. The package descriptor is deterministic and content-addressed over the lock hash, canonical format versions, hash algorithm/prefix, program, graph and host contract hashes, public interface hash, declared `package_imports`, declared `package_interfaces`, declared `package_contracts`, type aliases, DefIds, capability scopes, and source units. Local package imports use `package_imports = ["name=path"]` and are locked by package ref, interface hash, and contract hash. Imported packages are rebuilt for verification and source drift is rejected. The public interface hash includes exported canonical types, type hashes, named type aliases, and capability scopes. `project package --locked` requires the existing lockfile to match before writing; `project package --check` verifies the current package hash, descriptor, interface pointer, and interface JSON artifact against the lock, current canonical build, imported package descriptors, any `package_interfaces = ["name=p2:..."]` constraints, and any imported `package_contracts = ["name=p2:..."]` constraints in `protoss.toml`. `project interface` validates the current package descriptor, then prints its public exports and locked package imports; `project interface --json` prints the same verified contract as a versioned JSON object with exported capability descriptors and a portable `contractHash`; `project interface --check <file>` validates a saved interface contract against the current package.
- Canonical graph JSON can be round-tripped back to `program.canon` with `canon --from-graph` and explicitly migrated to the current graph format with `canon --migrate-graph`. Migration first validates the input graph, then re-emits exact current canonical JSON. It includes the `sha256`/`p2:` hash contract, a self-excluding `graphHash`, and a versioned `nodeGraph` table with content-addressed `Type`/`Term` nodes, exact program capability refs, definition `deps`, `capabilityScope` names, per-capability refs, aggregate `capabilityScopeRef`, `typeRef`/`termRef` roots and `edgeRefs`, deterministic sharing, reachability checks that reject dead nodes, and audit-time validation. Graph loading rejects non-canonical JSON serialization, including unknown fields. `check --graph`, `hash --graph`, `nf --graph`, `eval --graph`, `run --graph`, and `resume --graph` load this graph directly without reparsing `.protoss` text; the same commands also accept `--store-graph <project-or-store> <graphHash>` to load a content-addressed graph object from a local store.
- `defrec` supports structural Nat/List/Variant recursion and desugars to `foldNat`, `foldList`, or `foldVariant`; `defrecpoly` does the same for polymorphic structural definitions. Variant recursion can recurse over direct recursive payload fields and recursive fields of record items taken from direct payload lists. Malformed, self-recursive, or non-structural `recur` definitions are rejected.
- Web apps are checked by convention: `init : Process Model`, `update : Msg -> Model -> Process Model`, and `view : Model -> View Msg`.
- Source-level type aliases work with `(type Name Type)` and parametric aliases like `(type Maybe (A) (Variant (None Unit) (Some A)))`. Named records and variants also work as alias syntax: `(record Model (name String))`, `(record Pair (params A B) (first A) (second B))`, and `(variant Maybe (params A) (None Unit) (Some A))`. Aliases are expanded before canonical hashing, so alias names do not affect DefIds or program hashes.
- Files can use a small Elm-like surface syntax for top-level `name : Type` / `name = expr` definitions, `type alias`, indented union types, record type/value literals, lambdas written `\x -> expr`, whitespace application, `let ... in`, `case ... of`, and pipelines such as `value |> f |> g`. This surface is converted to the existing canonical S-expression AST before checking, so equivalent S-expression and Elm-like files hash the same.
- Records can be destructured with `(letRecord recordExpr (field (source binder) ...) body)`. It elaborates to one record `let` plus canonical `get` field accesses, so destructuring field order and binder names do not affect the graph beyond the body references they bind.
- Tuples are surface syntax over records: `(Tuple A B)` is the record type `(_1 A) (_2 B)`, `(tuple a b)` is the matching record value, and `(match pair ((tuple x y) body))` elaborates to `letRecord`.
- `match` is surface syntax over existing eliminators: Bool/variant branches elaborate to `case`, `_` can fill missing Bool/variant/list branches and is rejected when unreachable, list `(Nil ...)`/`(Cons head tail ...)` branches elaborate to `caseList`, record destructuring elaborates to `letRecord`, tuple destructuring elaborates through the same record path, and variant payloads can destructure records or tuples directly with branches like `(Node (record left right) body)` or `(Pair (tuple a b) body)`. No canonical `match` node is introduced.
- Named variants may be recursively self-referential when recursive occurrences are guarded by a variant constructor, for example a finite `Tree A` with `Leaf A` and `Node (Tree A) (Tree A)`. Unguarded recursive type aliases are rejected.
- Recursive named variants can be consumed with `foldVariant`; branch-local `recur` is accepted only for direct structural subterms of the current constructor payload, and non-structural recursion is rejected.
- Polymorphic value definitions work with explicit type application, for example `(defpoly id (params A) (-> A A) (lambda (x A) x))` and `((inst id Nat) 4)`. Calls such as `(id 4)`, `(some 9)`, and `((List.map xs) (lambda x (succ x)))` infer type arguments when arguments or the expected result type make them unambiguous. The elaborated canonical graph still uses explicit `inst`, so inferred and explicit sources hash the same.
- Lambdas can omit parameter annotations when an expected function type is available, for example `(def inc (-> Nat Nat) (lambda x (succ x)))`, `foldNat`/`foldList` steps, `bind` continuations, and annotated local lets like `(let (inc (-> Nat Nat) (lambda x (succ x))) (inc 1))`. They elaborate to the same canonical graph as annotated lambdas.
- List constructors can omit their item type under an expected `List A`, for example `(def xs (List Nat) (Cons 1 (Cons 2 Nil)))`. They elaborate to the same canonical graph as `(Cons Nat 1 (Cons Nat 2 (Nil Nat)))`.
- Lists support non-recursive pattern matching with `(caseList xs (Nil nilExpr) (Cons head tail consExpr))`; `head` and `tail` are alpha-stable binders in the `Cons` branch, and the form is represented in the canonical graph.
- The shipped prelude includes polymorphic `List.map`, `List.length`, `List.append`, `List.fold`, `List.concat`, `List.flatMap`, `List.filter`, `List.reverse`, `List.any`, `List.all`, `List.member`, `List.find`, `Maybe.map`, `Maybe.map2`, `Maybe.withDefault`, `Maybe.isSome`, `Maybe.isNone`, `Maybe.andThen`, `Maybe.toResult`, `Option.none`, `Option.some`, `Option.map`, `Option.map2`, `Option.withDefault`, `Option.isSome`, `Option.isNone`, `Option.andThen`, `Option.toResult`, `Result.map`, `Result.map2`, `Result.withDefault`, `Result.mapError`, `Result.andThen`, `Result.toMaybe`, `Result.isOk`, `Result.isErr`, `Pair.swap`, `Assoc.empty`, `Assoc.insert`, `Assoc.get`, `Assoc.contains`, `Assoc.keys`, `Assoc.values`, `Map.empty`, `Map.insert`, `Map.get`, `Map.contains`, `Map.keys`, `Map.values`, `Map.remove`, `Set.empty`, `Set.contains`, `Set.insert`, `Set.union`, `Set.remove`, `Set.intersect`, `Set.difference`, `String.empty`, `String.concat`, `String.append`, `String.eqString`, `String.length`, `String.slice`, `String.take`, `String.drop`, `String.startsWith`, `String.charAt`, `String.isWhitespace`, `String.isDigit`, `String.isDelimiter`, `String.isAtomChar`, `String.isEmpty`, `String.nonEmpty`, `String.join`, `Nat.toString`, `Nat.pred`, `Nat.sub`, `Nat.lt`, `Nat.lte`, `Nat.gt`, `Nat.gte`, typed `SourceSpan`/`Diagnostic` records with render helpers, a typed `TextCursor` with current/advance/remaining/done/peek helpers, a recursive `Sexp` ADT with constructors, typed `Result` validators, `Sexp.renderFlat`, total recursive `Sexp.render`, `Sexp.lexTokens` for pure tokenization of S-expression text, `Sexp.parseText`/`Sexp.parseTokens` for pure parsing into `Sexp`, `Protoss.parseText` for pure parsing of `module`, `import`, `export`, `capabilities`, `def`, `defpoly`, `defcap`, `defpolycap`, `defrec`, `defrecpoly`, `type`, `alias`, `record`, and `variant` declarations plus lambda/let/case/caseList/foldNat/foldList/foldVariant/recur/Nil/Cons/get/inst/done/bind/request/record/variant expression forms into small surface ASTs, `Protoss.renderType`/`Protoss.renderExpr`/`Protoss.renderDecl` and `Protoss.formatText` for validated AST-based formatting of parsed Protoss declarations, `Protoss.typeNames`/`Protoss.exprTermNames`/`Protoss.exprTypeNames`/`Protoss.declTermNames`/`Protoss.declTypeNames` plus `Protoss.resolveDecls`/`Protoss.resolveText` for pure AST reference extraction and local name-resolution reports, `Protoss.declsTermDepNodes`/`Protoss.termDependencyOrderText` and `Protoss.declsTypeDepNodes`/`Protoss.typeDependencyOrderText` for bounded local term/type dependency ordering with cycle reports and recursive variant self-dependencies ignored, `Protoss.typeEnvReportText`/`Protoss.checkTypeEnvText` for pure named-type environment checks over aliases, records, variants, missing types, duplicate types, duplicate parameters, duplicate record fields, duplicate variant cases, type arity mismatches, and type dependency cycles, and a recursive `Json` ADT with constructors, object lookup, field decoding, and typed `Result` validators, plus monomorphic Nat/Bool helpers.
- Variant constructors can infer their variant type from an expected context, for example `(def value (Maybe Nat) (variant Some 4))`; the inferred form hashes like the explicit `(variant (Maybe Nat) Some 4)`.
- Variant `case`/`foldVariant` branches whose payload type is `Unit` can omit the payload binder, for example `(case maybe (None 0) (Some n n))`; non-`Unit` constructors still require a binder.
- Source-level modules work with `(module Name)` and `(export symbol ...)`. Module-local definitions and type aliases are namespace-qualified, and imports may only reference exported symbols directly.
- S-expression syntax errors include deterministic `line:column` locations. File loading and project builds preserve them as `path:line:column: message`; common type errors loaded from files are localized to the source expression or symbol when it can be recovered deterministically, with definition-level fallback.
- `View msg` is a typed canonical UI type. Supported constructors are `text`, `image`, `button`, `input`, `column`, `row`, `list`, and `when`.
- UI/message mismatches are rejected statically by the typechecker.
- Web bundles are deterministic and include `index.html`, `protoss-runtime.js`, `protoss-app.json`, `protoss-graph.json`, `protoss-canon-graph.json`, `protoss-host-contract.json`, `protoss-capabilities.json`, and `protoss-world.json`. The browser runtime interprets the embedded canonical graph for `view` and `update`; external effects suspend as typed requests exposed through the runtime ledger/request API with capability refs, request signature refs, host codec refs, and response type metadata.
- `Process` supports `AskHuman`, `HttpGet`, `ReadClock`, `SaveLocal`, `LoadLocal`, and `ServerRequest` request payloads. Capabilities are checked against the kernel catalog and exported with typed request/response signatures plus content-addressed capability/signature refs; canonical request nodes also carry and validate those refs. Typed resume rejects wrong response tags.
- Source definitions can declare an exact allowed capability scope with `(defcap name (capabilities Cap.name ...) Type expr)` or `(defpolycap name (params A ...) (capabilities Cap.name ...) Type expr)`. The checker compares the declaration with the inferred direct and inherited capability scope; mismatches are rejected before canonical graph/store insertion. The declaration does not affect the canonical DefId.
- Ledger commands support inspect, replay, and diff over deterministic WorldRefs/EventRefs. Request events record and validate `capability`, `capability-ref`, `request-tag`, `request-signature-ref`, `request-payload-type`, `response-type`, `host-codec-version`, `request-codec-ref`, `response-codec-ref`, request/continuation ids, suspended request payload, `cap-scope`, and `cap-scope-ref` before insertion and during inspection. Resume events record `request-signature-ref`, `response-type`, `host-codec-version`, and `response-codec-ref`, then validate the typed host response against the suspended request before insertion and during inspection.
- Patch diagnostics include patch file paths, JSON syntax `line:column` locations, the failing operation number, operation kind, definition name, field context, and embedded `expr.source` line/column when kernel type errors can be mapped back to that source. Successful `patch apply` writes a deterministic content-addressed audit file under `store/patches/<patch-ref>.patch`, links it to the previous audit with `previous-ref`, and updates `store/patches/latest`; `patch audit` verifies and prints that chain, and the default `latest` audit must match the current store program hash. Project `audit` also verifies the latest patch audit when present. Rejected patches do not write audit artifacts.
- `invariants` runs executable checks over canonicalization, graph round-trip, graph-first loading, canonical graph migration, normalization, alpha-stability, typed `Process` resume, and typed ledger request/resume events.
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
dune exec protoss -- graph --stats graph.json
dune exec protoss -- graph --roots graph.json
dune exec protoss -- graph --deps graph.json
dune exec protoss -- graph --deps graph.json <nameOrDefId>
dune exec protoss -- graph --capabilities graph.json
dune exec protoss -- graph --capability graph.json <nameOrCapRef>
dune exec protoss -- graph --capability-scopes graph.json
dune exec protoss -- graph --capability-scopes graph.json <nameOrCapRef>
dune exec protoss -- graph --host-contract graph.json
dune exec protoss -- graph --check-host-contract graph.json host-contract.json
dune exec protoss -- graph --node graph.json <nodeRef>
dune exec protoss -- graph --def graph.json <nameOrDefId>
dune exec protoss -- graph --store-graph examples/workspace <graphHash> --out graph.json
dune exec protoss -- graph --store-graph examples/workspace <graphHash> --dot graph.dot
dune exec protoss -- graph --store-graph examples/workspace <graphHash> --stats
dune exec protoss -- graph --store-graph examples/workspace <graphHash> --roots
dune exec protoss -- graph --store-graph examples/workspace <graphHash> --deps
dune exec protoss -- graph --store-graph examples/workspace <graphHash> --deps <nameOrDefId>
dune exec protoss -- graph --store-graph examples/workspace <graphHash> --capabilities
dune exec protoss -- graph --store-graph examples/workspace <graphHash> --capability <nameOrCapRef>
dune exec protoss -- graph --store-graph examples/workspace <graphHash> --capability-scopes
dune exec protoss -- graph --store-graph examples/workspace <graphHash> --capability-scopes <nameOrCapRef>
dune exec protoss -- graph --store-graph examples/workspace <graphHash> --host-contract
dune exec protoss -- graph --store-graph examples/workspace <graphHash> --check-host-contract host-contract.json
dune exec protoss -- graph --store-graph examples/workspace <graphHash> --node <nodeRef>
dune exec protoss -- graph --store-graph examples/workspace <graphHash> --def <nameOrDefId>
dune exec protoss -- canon --graph examples/basic.protoss > /tmp/basic.protoss.graph.json
dune exec protoss -- canon --from-graph /tmp/basic.protoss.graph.json
dune exec protoss -- canon --migrate-graph /tmp/basic.protoss.graph.json
dune exec protoss -- check --graph /tmp/basic.protoss.graph.json
dune exec protoss -- hash --graph /tmp/basic.protoss.graph.json
dune exec protoss -- eval --graph /tmp/basic.protoss.graph.json --entry main
dune exec protoss -- canon --graph examples/ask_human.protoss > /tmp/ask_human.protoss.graph.json
dune exec protoss -- run --graph /tmp/ask_human.protoss.graph.json --entry askName --ledger /tmp/protoss-ledger
dune exec protoss -- resume --graph /tmp/ask_human.protoss.graph.json --entry askName --event <EventRef> --response String:Ada --ledger /tmp/protoss-ledger
dune exec protoss -- invariants file examples/basic.protoss
dune exec protoss -- invariants graph /tmp/basic.protoss.graph.json
dune exec protoss -- invariants graph --store-graph examples/workspace <graphHash>
dune exec protoss -- invariants alpha examples/alpha_a.protoss examples/alpha_b.protoss
dune exec protoss -- invariants process examples/ask_human.protoss --entry askName --response String:Ada
dune exec protoss -- invariants process --graph /tmp/ask_human.protoss.graph.json --entry askName --response String:Ada
dune exec protoss -- invariants process --store-graph examples/workspace <graphHash> --entry askName --response String:Ada
dune exec protoss -- invariants ledger examples/ask_human.protoss --entry askName --response String:Ada --ledger /tmp/protoss-ledger-invariant
dune exec protoss -- invariants ledger --graph /tmp/ask_human.protoss.graph.json --entry askName --response String:Ada --ledger /tmp/protoss-ledger-invariant-graph
dune exec protoss -- invariants ledger --store-graph examples/workspace <graphHash> --entry askName --response String:Ada --ledger /tmp/protoss-ledger-invariant-store-graph
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
dune exec protoss -- check examples/polymorphic_structural_recursion.protoss
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
dune exec protoss -- store graphs examples/workspace
dune exec protoss -- store graph examples/workspace <graphHash>
dune exec protoss -- store graph-put target/store graph.json
dune exec protoss -- store host-contracts examples/workspace
dune exec protoss -- store host-contract examples/workspace current
dune exec protoss -- store host-contract examples/workspace <contractHash>
dune exec protoss -- check --store-graph examples/workspace <graphHash>
dune exec protoss -- eval --store-graph examples/workspace <graphHash> --entry appMain
```
