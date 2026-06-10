# Protoss Web Alpha

Executable OCaml/Dune prototype for graph-first, content-addressed Protoss apps.

What works now:

- The pure core remains total: typed AST, canonical DefIds, stable hashes, deterministic normalization, explicit `Process` effects, typed capability descriptors, atomic patches, project stores, diff, and audit.
- Workspaces use `protoss.toml`; `project build` writes `.protoss/store` with canonical defs, `program.canon`, `program.graph.json`, `host.contract.json`, content-addressed graph objects under `graphs/`, content-addressed host contracts under `host-contracts/`, types, deps, normal forms, roots, build refs, `universe.root`, `universe.root.content`, and web markers. Content refs use the declared `sha256`/`p2:` hash contract. `UniverseRoot` is `H("universe-root-v1", package metadata/import constraints, defs, types, harness slot, policies, world refs)` and is also recorded in lock and package descriptors. `project audit`, `project lock --check`, `project build --locked`, and `project package --check` reject stale or mismatched `UniverseRoot` state. Host contracts include typed JSON codec refs for request payloads and responses. Stored graph objects can be inserted with `store graph-put`, listed with `store graphs`, or read and validated with `store graph <graphHash>`. Project audit validates every graph object present under `graphs/` plus the current content-addressed host contract.
- `project lock` writes `.protoss/lock`, a deterministic content-addressed lockfile over the package metadata, canonical format versions, hash algorithm/prefix, program hash, graph hash, host contract hash, DefIds, source unit hashes, imports, exported interface constraints, imported contract hashes, capabilities, and package policies. `project lock --check` and `project build --locked` reject drift without rewriting the lockfile or store.
- `project package` writes `.protoss/packages/<hash>.package` plus `.protoss/package`, and stores the verified public interface as `.protoss/interfaces/<hash>.interface.json` plus `.protoss/interface`. The package descriptor is deterministic and content-addressed over the lock hash, canonical format versions, hash algorithm/prefix, program, graph and host contract hashes, public interface hash, declared `package_imports`, declared `package_aliases`, declared `package_policy_aliases`, declared `package_interfaces`, declared `package_contracts`, declared package `policies`, type aliases, DefIds, capability scopes, and source units. Local package imports use `package_imports = ["name=path"]` and are locked by package ref, interface hash, and contract hash; `package_aliases = ["name@semver=path"]` lets imports use `package_imports = ["name=name@semver"]`, with name/version verified before the alias is resolved to the package hash. `package_policy_aliases = ["name@policy=path"]` does the same for policy-based imports and requires the imported package manifest to advertise that policy. The importing manifest must declare every capability required by the imported public interface. Imported packages are rebuilt for verification and source drift is rejected. The public interface hash includes exported canonical types, type hashes, named type aliases, and capability scopes. `project package --locked` requires the existing lockfile to match before writing; `project package --check` verifies the current package hash, descriptor, interface pointer, and interface JSON artifact against the lock, current canonical build, imported package descriptors, any `package_interfaces = ["name=p2:..."]` constraints, and any imported `package_contracts = ["name=p2:..."]` constraints in `protoss.toml`. `project interface` validates the current package descriptor, then prints its public exports and locked package imports; `project interface --json` prints the same verified contract as a versioned JSON object with exported capability descriptors and a portable `contractHash`; `project interface --check <file>` validates a saved interface contract against the current package.
- `project export-layout [project] [--out <dir>]` writes a portable tree with `/protoss.lock`, `/views/**/*.pt`, `/cache/program.ptb`, and `/harness/_empty.pth` layout paths.
- `git map [project]` builds the project and writes `.protoss/git.map`, mapping the current Git commit to the current `UniverseRoot` and the current Git branch to a content-derived Universe branch ref. `git blame [project] <file>` writes a `.protoss/provenance/git-blame/*.ledger` file that maps source lines and Git commits to that same root.
- Package policy `NoNetworkExceptDeclared` rejects workspace builds where source declares `Http.*` or `Server.*` capabilities that are not explicitly listed in the manifest `capabilities`.
- Canonical graph JSON can be round-tripped back to `program.canon` with `canon --from-graph` and explicitly migrated to the current graph format with `canon --migrate-graph`. Migration first validates the input graph, then re-emits exact current canonical JSON. It includes the `sha256`/`p2:` hash contract, a self-excluding `graphHash`, and a versioned `nodeGraph` table with content-addressed `Type`/`Term` nodes, exact program capability refs, definition `deps`, `capabilityScope` names, per-capability refs, aggregate `capabilityScopeRef`, `typeRef`/`termRef` roots and `edgeRefs`, deterministic sharing, reachability checks that reject dead nodes, and audit-time validation. Graph loading rejects non-canonical JSON serialization, including unknown fields. `check --graph`, `hash --graph`, `nf --graph`, `eval --graph`, `run --graph`, and `resume --graph` load this graph directly without reparsing `.protoss` text; the same commands also accept `--store-graph <project-or-store> <graphHash>` to load a content-addressed graph object from a local store. Agents can query graph files or store graph objects through `protoss agent graph`, which returns versioned JSON envelopes for summaries, stats, definitions, dependencies, nodes, capabilities, capability scopes, host contracts, and definition explanations.
- `defrec` supports structural Nat/List/Variant recursion and desugars to `foldNat`, `foldList`, or `foldVariant`; `defrecpoly` does the same for polymorphic structural definitions. Variant recursion can recurse over direct recursive payload fields and recursive fields of record items taken from direct payload lists. Malformed, self-recursive, or non-structural `recur` definitions are rejected.
- Web apps are checked by convention: `init : Process Model`, `update : Msg -> Model -> Process Model`, and `view : Model -> View Msg`.
- Web builds write `protoss-compiled-artifact.txt` and embed `compiledArtifact` in `protoss-app.json`; the artifact ref is `derive(UniverseRoot, Target, OptimizationPolicy)` using target `web` and policy `web-default-v1`.
- Source-level type aliases work with `(type Name Type)` and parametric aliases like `(type Maybe (A) (Variant (None Unit) (Some A)))`. Named records and variants also work as alias syntax: `(record Model (name String))`, `(record Pair (params A B) (first A) (second B))`, and `(variant Maybe (params A) (None Unit) (Some A))`. Aliases are expanded before canonical hashing, so alias names do not affect DefIds or program hashes.
- Files can use either the prototype `.protoss` extension or the official human-source `.pt` extension; workspaces discover both under `source_dirs`. Source imports can pin the imported file with a hash suffix, for example `import "math.protoss#p2:..."`; mismatches are rejected during workspace loading. Canonical text can be saved as `.ptc` and canonical binary can be saved as `.ptb`; both load directly through `check`/`hash`/`nf`/`eval` without reparsing human syntax. The `.ptb` v1 container is `PROTOSS-PTB\0\1`, a 32-bit big-endian payload length, and the canonical text payload. A small Elm-like surface syntax supports top-level `name : Type` / `name = expr` / `name arg = expr` definitions, `module Name exposing (...)`, `import "path" exposing (...)`, top-level `capabilities Cap.name ...`, process scopes in signatures like `Process { Human.ask } String`, typed local `let` bindings, `type alias`, indented union types, record type/value literals, record updates like `{ model | count = next }`, list literals when an expected `List A` is available, field access like `model.count`, lambdas written `\x y -> expr`, whitespace application, Nat addition with `a + b`, Nat comparisons `==`, `/=`, `<`, `<=`, `>`, `>=`, boolean `not`, `&&`, `||`, signature-free Nat additions such as `add a b = a + b`, `if ... then ... else ...`, `let ... in`, `case ... of`, and pipelines such as `value |> f |> g`. This surface is converted to the existing canonical S-expression AST before checking, so equivalent S-expression and Elm-like files hash the same.
- See `docs/canonical-formats.md` for the `.pt`/`.ptc`/`.ptb` contract and validation rules.
- Cursor/VS Code syntax highlighting and Ctrl+Click go-to-definition for `.protoss` and `.pt` files is available in `editors/cursor/protoss-syntax`; copy it to `~/.cursor/extensions/protoss.protoss-syntax-0.3.2`, and to `~/.cursor-server/extensions/protoss.protoss-syntax-0.3.2` when using Cursor Remote/WSL, then restart Cursor.
- Records can be destructured with `(letRecord recordExpr (field (source binder) ...) body)`. It elaborates to one record `let` plus canonical `get` field accesses, so destructuring field order and binder names do not affect the graph beyond the body references they bind.
- Tuples are surface syntax over records: `(Tuple A B)` is the record type `(_1 A) (_2 B)`, `(tuple a b)` is the matching record value, and `(match pair ((tuple x y) body))` elaborates to `letRecord`.
- `match` is surface syntax over existing eliminators: Bool/variant branches elaborate to `case`, `_` can fill missing Bool/variant/list branches and is rejected when unreachable, list `(Nil ...)`/`(Cons head tail ...)` branches elaborate to `caseList`, record destructuring elaborates to `letRecord`, tuple destructuring elaborates through the same record path, and variant payloads can destructure records or tuples directly with branches like `(Node (record left right) body)` or `(Pair (tuple a b) body)`. No canonical `match` node is introduced.
- Named variants may be recursively self-referential when recursive occurrences are guarded by a variant constructor, for example a finite `Tree A` with `Leaf A` and `Node (Tree A) (Tree A)`. Unguarded recursive type aliases are rejected.
- Recursive named variants can be consumed with `foldVariant`; branch-local `recur` is accepted only for direct structural subterms of the current constructor payload, and non-structural recursion is rejected.
- Polymorphic value definitions work with explicit type application, for example `(defpoly id (params A) (-> A A) (lambda (x A) x))` and `((inst id Nat) 4)`. Calls such as `(id 4)`, `(some 9)`, and `((List.map xs) (lambda x (succ x)))` infer type arguments when arguments or the expected result type make them unambiguous. The elaborated canonical graph still uses explicit `inst`, so inferred and explicit sources hash the same.
- Lambdas can omit parameter annotations when an expected function type is available, for example `(def inc (-> Nat Nat) (lambda x (succ x)))`, `foldNat`/`foldList` steps, `bind` continuations, and annotated local lets like `(let (inc (-> Nat Nat) (lambda x (succ x))) (inc 1))`. They elaborate to the same canonical graph as annotated lambdas.
- List constructors can omit their item type under an expected `List A`, for example `(def xs (List Nat) (Cons 1 (Cons 2 Nil)))`. They elaborate to the same canonical graph as `(Cons Nat 1 (Cons Nat 2 (Nil Nat)))`.
- Lists support non-recursive pattern matching with `(caseList xs (Nil nilExpr) (Cons head tail consExpr))`; `head` and `tail` are alpha-stable binders in the `Cons` branch, and the form is represented in the canonical graph.
- The shipped prelude includes polymorphic `List.map`, `List.length`, `List.append`, `List.fold`, `List.concat`, `List.flatMap`, `List.filter`, `List.reverse`, `List.any`, `List.all`, `List.member`, `List.find`, `Maybe.map`, `Maybe.map2`, `Maybe.withDefault`, `Maybe.isSome`, `Maybe.isNone`, `Maybe.andThen`, `Maybe.toResult`, `Option.none`, `Option.some`, `Option.map`, `Option.map2`, `Option.withDefault`, `Option.isSome`, `Option.isNone`, `Option.andThen`, `Option.toResult`, `Result.map`, `Result.map2`, `Result.withDefault`, `Result.mapError`, `Result.andThen`, `Result.toMaybe`, `Result.isOk`, `Result.isErr`, `Pair.swap`, `Assoc.empty`, `Assoc.insert`, `Assoc.get`, `Assoc.contains`, `Assoc.keys`, `Assoc.values`, `Map.empty`, `Map.insert`, `Map.get`, `Map.contains`, `Map.keys`, `Map.values`, `Map.remove`, `Set.empty`, `Set.contains`, `Set.insert`, `Set.union`, `Set.remove`, `Set.intersect`, `Set.difference`, `String.empty`, `String.concat`, `String.append`, `String.eqString`, `String.length`, `String.slice`, `String.take`, `String.drop`, `String.startsWith`, `String.charAt`, `String.isWhitespace`, `String.isDigit`, `String.isDelimiter`, `String.isAtomChar`, `String.isEmpty`, `String.nonEmpty`, `String.join`, `Nat.toString`, `Nat.pred`, `Nat.sub`, `Nat.lt`, `Nat.lte`, `Nat.gt`, `Nat.gte`, typed `SourceSpan`/`Diagnostic` records with render helpers, a typed `TextCursor` with current/advance/remaining/done/peek helpers, a recursive `Sexp` ADT with constructors, typed `Result` validators, `Sexp.renderFlat`, total recursive `Sexp.render`, `Sexp.lexTokens` for pure tokenization of S-expression text, `Sexp.parseText`/`Sexp.parseTokens` for pure parsing into `Sexp`, `Protoss.parseText` for pure parsing of `module`, `import`, `export`, `capabilities`, `def`, `defpoly`, `defcap`, `defpolycap`, `defrec`, `defrecpoly`, `type`, `alias`, `record`, and `variant` declarations plus lambda/let/case/caseList/foldNat/foldList/foldVariant/recur/Nil/Cons/get/inst/done/bind/request/record/variant expression forms into small surface ASTs, `Protoss.renderType`/`Protoss.renderExpr`/`Protoss.renderDecl` and `Protoss.formatText` for validated AST-based formatting of parsed Protoss declarations, `Protoss.typeNames`/`Protoss.exprTermNames`/`Protoss.exprTypeNames`/`Protoss.declTermNames`/`Protoss.declTypeNames` plus `Protoss.resolveDecls`/`Protoss.resolveText` for pure AST reference extraction and local name-resolution reports, `Protoss.declsTermDepNodes`/`Protoss.termDependencyOrderText` and `Protoss.declsTypeDepNodes`/`Protoss.typeDependencyOrderText` for bounded local term/type dependency ordering with cycle reports and recursive variant self-dependencies ignored, `Protoss.typeEnvReportText`/`Protoss.checkTypeEnvText` for pure named-type environment checks over aliases, records, variants, missing types, duplicate types, duplicate parameters, duplicate record fields, duplicate variant cases, type arity mismatches, and type dependency cycles, `Protoss.capabilityReportText`/`Protoss.checkCapabilityText` for pure capability catalog, direct request, module declaration, and declared scope checks, `Protoss.staticReportText`/`Protoss.checkStaticText` for a combined pure parse/resolution/dependency/type-env/capability report, and a recursive `Json` ADT with constructors, object lookup, field decoding, typed `Result` validators, and deterministic `Json.render` text encoding plus `Json.lexTokens`/`Json.parseText` for JSON text decoding, plus monomorphic Nat/Bool helpers.
- The self-hosted frontend is reachable from the CLI with `protoss self parse|resolve|deps|capabilities|static <file>` (add `--json` to `static`) and `protoss self fmt [--check] <file>`. A kernel-checked self-hosted typecheck report is available with `protoss self typecheck <file> [--json]`, `protoss self type-of <file> --entry <name>`, and `protoss self compare-typecheck <file>`. It structurally checks `def`/`defcap`/`defpoly`/`defpolycap` bodies, Nat/List `defrec` and `defrecpoly`, direct-payload Variant `defrec`, explicit polymorphic `inst`, expected-context implicit polymorphic instantiation for direct variables/applications, direct `Nil`/`Cons` arguments that contribute type argument constraints under expected `List A`, nested spines with inferable or annotated prefix arguments, current spine suffix arguments checked in expected context, already-applied prefix `Nil` and `Cons ... Nil` list constructors checked from inferred `List A` context while outer suffix arguments are checked in expected context, parameterized record/variant type substitution, expected-context inferred variants, expected-context `Nil`/`Cons`, and direct expression subterms for lambdas, lets, folds including `foldVariant`, fields, caseList, records, application arguments, Bool case branches, exhaustive cases over named variants, and direct `Process` terms (`done`, requests, annotated `bind`) including rejection of `Process`-typed `let` values in pure expected contexts. Each command splices the target source into a driver definition, checks the combined program with the trusted OCaml kernel, and evaluates the matching `stdlib/prelude.protoss` function through the normal evaluator — so the report is produced by Protoss code, rendered with `Json.render`, while canonical DefIds still come from the kernel. `self static --json` includes the kernel-computed `frontendDefId` of the frontend implementation. The formatter is deterministic and idempotent (`fmt(fmt(x)) = fmt(x)`) and preserves DefIds after canonicalization. `PROTOSS_STDLIB` overrides the prelude path. See `docs/self-hosting.md` and `docs/self-hosted-typechecker.md` for the trusted-kernel boundary, and `conformance/self_host/` for golden parse/resolve/deps/capabilities/static/format/diagnostic outputs.
- Variant constructors can infer their variant type from an expected context, for example `(def value (Maybe Nat) (variant Some 4))`; the inferred form hashes like the explicit `(variant (Maybe Nat) Some 4)`.
- Variant `case`/`foldVariant` branches whose payload type is `Unit` can omit the payload binder, for example `(case maybe (None 0) (Some n n))`; non-`Unit` constructors still require a binder.
- Source-level modules work with `(module Name)` and `(export symbol ...)`. Module-local definitions and type aliases are namespace-qualified, and imports may only reference exported symbols directly.
- S-expression syntax errors include deterministic `line:column` locations. File loading and project builds preserve them as `path:line:column: message`; common type errors loaded from files are localized to the source expression or symbol when it can be recovered deterministically, with definition-level fallback.
- Public CLI failures are prefixed with stable error codes from the `Public_error` catalog, including the formal taxonomy categories `TypeMismatch`, `UnknownReference`, `CapabilityDenied`, `NonTerminatingRecursion`, `NonProductiveProcess`, `HarnessRegression`, `AmbiguousHumanSyntax`, `UnsafeMigration`, `PolicyViolation`, and `SecretLeakRisk`. Use `protoss explain <code>` or `protoss explain --list` for the current catalog.
- `protoss grammar kernel` prints the versioned executable grammar for trusted core declarations, types, expressions, requests, branches, and binders. `protoss grammar human` prints the versioned Protoss/H grammar covering the accepted S-expression and Elm-like human-source views.
- `View msg` is a typed canonical UI type. Supported constructors are `text`, `image`, `button`, `input`, `column`, `row`, `list`, `when`, and `node`.
- For HTML beyond the declarative constructors, `node`, `attr`, and `on` are an Elm-`Html`-style escape hatch and compose with the existing `View msg` tree: `node : String -> List (Attr msg) -> List (View msg) -> View msg` builds an arbitrary element, `attr : String -> String -> Attr msg` is a static attribute, and `on : String -> msg -> Attr msg` is an event handler. They introduce a new canonical type `Attr a` (mirrored on `View a`): `attr` is message-agnostic (`Attr Unit`) and unifies into any `List (Attr msg)`, exactly as `text : View Unit` composes into any `List (View msg)`. Attribute order follows the source and is never sorted, so equivalent S-expression and Elm-like forms hash identically. The web runtime renders nodes with `createElement`/`setAttribute`/`addEventListener` (never `innerHTML`) and dispatches the typed `msg`, so there is no script injection. The shipped prelude adds `defpoly` helpers over these primitives: `Html.div`/`Html.span`/`Html.p`/`Html.ul`/`Html.li`/`Html.a`, `Html.class`/`Html.id`/`Html.href`/`Html.style`, and `Html.onClick`/`Html.onInput`.
- UI/message mismatches are rejected statically by the typechecker.
- Web bundles are deterministic and include `index.html`, `protoss-runtime.js`, `protoss-app.json`, `protoss-graph.json`, `protoss-canon-graph.json`, `protoss-host-contract.json`, `protoss-capabilities.json`, and `protoss-world.json`. The browser runtime interprets the embedded canonical graph for `view` and `update`; external effects suspend as typed requests exposed through the runtime ledger/request API with capability refs, request signature refs, host codec refs, and response type metadata.
- `Process` supports `AskHuman`, `HttpGet`, `ReadClock`, `SaveLocal`, `LoadLocal`, and `ServerRequest` request payloads. Capabilities are checked against the kernel catalog and exported with typed request/response signatures plus content-addressed capability/signature refs; canonical request nodes also carry and validate those refs. Typed resume rejects wrong response tags. `examples/effect_sensors.protoss` is a host-effect fixture covering clock, HTTP, and server-request style sensors.
- Source definitions can declare an exact allowed capability scope with `(defcap name (capabilities Cap.name ...) Type expr)` or `(defpolycap name (params A ...) (capabilities Cap.name ...) Type expr)`. The checker compares the declaration with the inferred direct and inherited capability scope; mismatches are rejected before canonical graph/store insertion. The declaration does not affect the canonical DefId. Capability audit output includes a conservative `SecretLeakRisk` report when local storage capability is combined with outbound request capabilities; `examples/secret_leak_risk.protoss` is the dedicated regression fixture.
- Ledger commands support inspect, replay, diff, fork, merge, and typed negative external responses over deterministic WorldRefs/EventRefs. World objects and event objects form a content-addressed Merkle-DAG: request/resume/error/merge events are hashed, worlds point to previous or merged parent worlds, and merge worlds record both parents. Inspection validates event content hashes, world content hashes, previous-world links, merge parent links, and the rule that every non-initial world points at an explicit event. Request events record and validate `capability`, `capability-ref`, `request-tag`, `request-signature-ref`, `request-payload-type`, `response-type`, `host-codec-version`, `request-codec-ref`, `response-codec-ref`, request/continuation ids, suspended request payload, `cap-scope`, and `cap-scope-ref` before insertion and during inspection. Resume events record `request-signature-ref`, `response-type`, `host-codec-version`, and `response-codec-ref`, then validate the typed host response against the suspended request before insertion and during inspection. `external-error` events link to a request and validate the same typed response metadata while recording `error-code` and `error-message`. Merge worlds replay both branches with shared ancestors de-duplicated.
- Store diffs include JSON-pointer-style structural definition paths, field-level `changedPaths`, and aggregate `affected` metadata for definitions plus the reserved harness surface.
- Store objects are interned through a global content store (`PROTOSS_GLOBAL_STORE`, or `$HOME/.protoss/global-store` by default) and project object files hardlink to the shared payload when the filesystem allows it, physically deduplicating identical nodes across projects.
- `store gc <store>` reports unreferenced content-addressed objects, and `store gc --sweep --yes <store>` removes those unreachable objects while keeping current definition objects.
- Patch diagnostics include patch file paths, JSON syntax `line:column` locations, the failing operation number, operation kind, definition name, field context, and embedded `expr.source` line/column when kernel type errors can be mapped back to that source. `patch review` renders a human-readable operation summary before validation or apply. Successful `patch apply` writes a deterministic content-addressed audit file under `store/patches/<patch-ref>.patch`, links it to the previous audit with `previous-ref`, records `previous-root`/`root-ref`, writes content-addressed root-state and patch-provenance records under `store/provenance/`, and updates `store/patches/latest`; `patch audit` verifies and prints that chain, and the default `latest` audit must match the current store program hash and latest root state. Project `audit` also verifies the latest patch audit when present. Rejected patches do not write audit artifacts.
- The native agent protocol is `AI -> PatchCandidate -> Validator -> Harness -> Commit`. `protoss agent protocol` prints the versioned contract, `protoss agent guard-write <path>` rejects direct writes to canonical/store internals, `protoss agent factor-identical <project-or-store> [--out <patch.json>]` proposes a structural `DeleteDef` patch for duplicate DefIds whose source-level dependents do not need rewriting, and `protoss agent commit <store> <patch.json>` is the agent mutation path through patch validation and content-addressed audit.
- `protoss mcp serve` starts a JSON-RPC stdio MCP server with `initialize`, `tools/list`, and `tools/call`. It exposes `protoss.query`, `protoss.readNode`, `protoss.renderView`, `protoss.proposePatch`, `protoss.checkPatch`, `protoss.applyPatch`, `protoss.runHarness`, `protoss.explain`, `protoss.normalize`, `protoss.diff`, and `protoss.rollback`.
- `invariants` runs executable checks over canonicalization, graph round-trip, graph-first loading, canonical graph migration, normalization, alpha-stability, typed `Process` resume, and typed ledger request/resume events.
- `invariants package <project>` checks package lock consistency, package descriptor freshness, package interface refs, package interface JSON artifacts, exported capability descriptors, `contractHash`, exported canonical type hashes, imported package freshness, package refs, and audit.
- The core test suite includes preservation/progression-style fixtures: well-typed pure definitions in `examples/preservation_progression.protoss` normalize to values matching their declared types, while paired type-error and recursion-error fixtures are rejected before evaluation.
- Runtime `let` bindings are evaluated as memoized thunks: an unused RHS is not forced, and repeated uses share the first forced value. The S-expression surface and canonical graph support `(strict expr)`; a strict `let` RHS is forced at binding time and is recorded as a `Strict` graph term.
- Pure persistent evaluation cache entries use `EvalKey = H("protoss.eval.v1", DefId, ArgsHash, RuntimePolicy)` for nullary definitions; `ArgsHash` is the canonical no-args hash and `RuntimePolicy` records the runtime version, cache scope, stdlib fast-path setting, and active capability scope. Definition and application cache keys include that policy, so different runtime or capability policies write separate content-addressed entries.
- Process evaluation keys use `H("protoss.process.eval.v1", DefId, WorldRef, CapScope, RuntimePolicy)` and `protoss run` prints the `ProcessEvalKey` for completed or suspended process entries.
- `protoss spec check [protoss-spec.md]` audits checked TODO items and fails when a checked item lacks local or section-level proof markers.
- Web patch validation checks `init/update/view`; Model shape changes require a pure `migrate_v1_v2`.
- `dune runtest` runs a fast smoke suite for parser/checker/hash/normalization/cache/process/patch rollback. Longer suites are explicit: `dune build @coretest`, `dune build @integrationtest`, `dune build @stdlibtest`, `dune build @selftest`, or `dune build @fulltest` (aggregates the per-section aliases — the whole workspace part plus the three web slices `@integrationtest-web-app`/`-patches`/`-audit` — and runs them as parallel processes; per-slice workspace aliases `@integrationtest-workspace-project`/`-consumer`/`-corruption` remain for targeted reruns). Test rules declare their fixture dependencies, so plain `dune build @fulltest` without `--force` is correct and a no-op when nothing changed.

Main commands:

```sh
dune runtest

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
dune exec protoss -- project export-layout examples/web/todo_app --out /tmp/todo-layout
dune exec protoss -- git map examples/web/todo_app
dune exec protoss -- git blame examples/web/todo_app src/app.protoss
dune exec protoss -- web build examples/web/todo_app --out dist/
dune exec protoss -- web inspect examples/web/todo_app
dune exec protoss -- audit examples/web/todo_app

dune exec protoss -- patch check examples/web/todo_app/.protoss/store patches/web/change_button_text.json
dune exec protoss -- patch review patches/web/change_button_text.json
dune exec protoss -- patch apply examples/web/todo_app/.protoss/store patches/web/change_button_text.json
dune exec protoss -- patch audit examples/web/todo_app/.protoss/store
dune exec protoss -- patch check examples/web/todo_app/.protoss/store patches/web/invalid_msg_view_mismatch.json
dune exec protoss -- patch check examples/web/todo_app/.protoss/store patches/web/model_without_migration.json
dune exec protoss -- patch check examples/web/todo_app/.protoss/store patches/web/model_with_migration.json

dune exec protoss -- diff before.store after.store
dune exec protoss -- diff --json before.store after.store
dune exec protoss -- patch from-diff before.store after.store > patch.json
dune exec protoss -- compare examples/basic.pt examples/basic.ptc
dune exec protoss -- compare --project examples/web/todo_app examples/web/todo_app
dune exec protoss -- capabilities examples/ask_human.protoss
dune exec protoss -- capabilities --project examples/web/todo_app
dune exec protoss -- duplicates examples/basic.protoss
dune exec protoss -- duplicates --project examples/web/todo_app
dune exec protoss -- termination examples/basic.protoss main
dune exec protoss -- grammar kernel
dune exec protoss -- grammar human
dune exec protoss -- spec check protoss-spec.md

dune exec protoss -- ledger inspect <WorldRefOrEventRef>
dune exec protoss -- ledger replay <WorldRef>
dune exec protoss -- ledger diff <WorldRefA> <WorldRefB>
dune exec protoss -- ledger fork feature <WorldRef>
dune exec protoss -- ledger merge <ledger-root> <WorldRefA> <WorldRefB>
dune exec protoss -- ledger reject <ledger-root> <WorldRef> <EventRef> HOST_TIMEOUT "host timed out"

dune exec protoss -- fmt examples/web/todo_app/src/app.protoss
dune exec protoss -- fmt --check examples/web/todo_app/src/app.protoss
dune exec protoss -- convert --to pt examples/basic.ptc > /tmp/basic.pt
dune exec protoss -- convert --to ptc examples/basic.pt > /tmp/basic.ptc
dune exec protoss -- convert --to ptb examples/basic.ptc > /tmp/basic.ptb
dune exec protoss -- convert --from-graph --to pt /tmp/basic.protoss.graph.json > /tmp/basic-from-graph.pt
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
dune exec protoss -- agent graph graph.json --summary
dune exec protoss -- agent graph graph.json --def <nameOrDefId>
dune exec protoss -- agent graph graph.json --node <nodeRef>
dune exec protoss -- agent graph graph.json --deps <nameOrDefId>
dune exec protoss -- agent graph graph.json --explain <nameOrDefId>
dune exec protoss -- agent explain graph.json <nameOrDefId>
dune exec protoss -- agent protocol
dune exec protoss -- agent guard-write .protoss/store/program.canon
dune exec protoss -- agent factor-identical examples/web/todo_app/.protoss/store --out /tmp/factor-identical.json
dune exec protoss -- agent commit examples/web/todo_app/.protoss/store patches/web/change_button_text.json
dune exec protoss -- mcp serve
dune exec protoss -- agent graph --store-graph examples/workspace <graphHash> --capabilities
dune exec protoss -- agent graph --store-graph examples/workspace <graphHash> --host-contract
dune exec protoss -- canon --ptb examples/basic.protoss > /tmp/basic.ptb
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
dune exec protoss -- explain --list
dune exec protoss -- bench build examples/web/todo_app
dune exec protoss -- cache stats .protoss/cache
dune exec protoss -- cache list .protoss/cache
dune exec protoss -- store gc examples/web/todo_app/.protoss/store
dune exec protoss -- store gc --sweep --yes examples/web/todo_app/.protoss/store
```

Compatibility commands from earlier MVPs still work:

```sh
dune exec protoss -- parse examples/basic.protoss
dune exec protoss -- parse examples/basic.ptc
dune exec protoss -- check examples/basic.pt
dune exec protoss -- check examples/basic.ptc
dune exec protoss -- check examples/basic.ptb
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
dune exec protoss -- check examples/preservation_progression.protoss
dune exec protoss -- check examples/list_case.protoss
dune exec protoss -- check examples/pattern_match.protoss
dune exec protoss -- check examples/record_destructure.protoss
dune exec protoss -- check examples/recursive_tree.protoss
dune exec protoss -- nf examples/recursive_tree.protoss
dune exec protoss -- check examples/stdlib_generics.protoss
dune exec protoss -- check examples/result_errors.protoss
dune exec protoss -- check examples/structural_recursion.protoss
dune exec protoss -- check examples/effect_sensors.protoss
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
