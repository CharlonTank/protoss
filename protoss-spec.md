# Protoss - Grand TODO de specification

Ce fichier est la file d'attente principale du projet. Une case ne doit etre
cochee que si le code, les fixtures, les tests et la documentation du depot
prouvent que la fonctionnalite est presente.

Regle de travail: chaque amelioration majeure doit etre livree par un commit
puis un push. Les cases cochees doivent rester verifiables par les commandes
listees dans la section "Gates de validation".

## Gates de validation

- [ ] `dune build`
- [ ] `dune runtest --force`
- [ ] `dune build @fulltest`
- [ ] `dune exec protoss -- invariants file examples/basic.protoss`
- [ ] `dune exec protoss -- invariants alpha examples/alpha_a.protoss examples/alpha_b.protoss`
- [ ] `dune exec protoss -- app check examples/web/todo_app`
- [ ] `dune exec protoss -- project build examples/web/todo_app --target web --stats`
- [ ] `dune exec protoss -- project lock examples/web/todo_app --check`
- [ ] `dune exec protoss -- project package examples/web/todo_app --check`
- [ ] `dune exec protoss -- audit examples/web/todo_app`
- [ ] `git status --short --branch` montre une branche propre apres push.

## 0. Spec, suivi et ergonomie repo

Preuves de section: `protoss-spec.md`, `lib/spec_audit.ml`,
`protoss spec check`, assertions `spec audit ...` dans `test/test_protoss.ml`.

- [x] Avoir une specification centrale dans le depot.
- [x] Renommer la specification en `protoss-spec.md`, le chemin demande.
- [x] Transformer la specification en TODO exploitable.
- [x] Ajouter une commande ou un script de verification qui echoue si une case
  marquee done n'a plus de test ou de preuve documentaire.
  Preuves: `lib/spec_audit.ml`, commande `protoss spec check
  protoss-spec.md`, assertions `spec audit ...` dans `test/test_protoss.ml`.
- [x] Synchroniser `README.md`, `CLAUDE.md` et ce TODO quand une fonctionnalite
  change de statut.
  Preuves: commits de fonctionnalite mettent a jour `README.md`, `CLAUDE.md`
  et `protoss-spec.md`; exemples recents: indentation significative,
  `Process caps A`, architecture `Cmd caps Msg`.
- [x] Documenter pour chaque case l'emplacement de son test principal.
  Preuves: `lib/spec_audit.ml`, commande `protoss spec check
  protoss-spec.md`, assertions `spec audit accepts section evidence` et
  `spec audit reports missing evidence` dans `test/test_protoss.ml`.

## 1. Vision et modele de source de verite

Preuves de section: `lib/ast.ml`, `lib/kernel.ml`, `lib/canonical_ir.ml`,
`lib/store.ml`, `lib/workspace.ml`, commandes `protoss graph`,
`protoss store`, `protoss compare`, assertions graphe/store/workspace dans
`test/test_protoss.ml`.

- [x] Representer les programmes comme AST type puis graphe canonique.
- [x] Donner des identites de contenu stables aux definitions (`DefId`).
- [x] Utiliser un contrat de hash declare `sha256` avec prefixe `p2:`.
- [x] Produire un graphe JSON canonique avec `graphHash` auto-excluant.
- [x] Stocker les objets de graphe dans un store de projet content-addressed.
- [x] Auditer les objets de graphe presents dans le store.
- [x] Produire des locks, packages et interfaces deterministes.
- [x] Definir et implementer un vrai `UniverseRoot = H(packages, defs, types,
  harnesses, policies, worldRefs)`.
  Preuves: `Workspace.universe_root_content`, fichiers store `universe.root`
  et `universe.root.content`, champs `universe-root` des locks/packages,
  assertions "project universe root ..." dans `test/test_protoss.ml` et
  `README.md`.
- [x] Faire du `UniverseRoot` la source de verite de toutes les commandes projet.
  Preuves: `Workspace.audit_universe_root`, `check_lock`, `build_locked`,
  `check_package`, assertions "project audit rejects stale universe root",
  "project lock check should reject stale universe root", "locked build should
  reject stale universe root" et "package check should reject stale universe
  root" dans `test/test_protoss.ml`, `README.md`.
- [x] Ajouter un store global partage entre projets pour l'interning des noeuds.
  Preuves: `Store.global_store_root`, `Store.put_object`, variable
  `PROTOSS_GLOBAL_STORE`, assertion "global store writes shared object" dans
  `test/test_protoss.ml`, `README.md`.
- [x] Dedupliquer physiquement les noeuds identiques entre projets distincts.
  Preuves: hardlink depuis `Store.put_object` vers le store global, assertions
  "global store project ... hardlink inode" dans `test/test_protoss.ml`,
  `README.md`.
- [x] Exposer une commande de comparaison semantique entre deux roots.
- [x] Ajouter une provenance native liee aux roots et patches.
  Preuves: `Patch_audit.root_state_of_checked`, `write_root_state`,
  `write_patch_provenance`, champs `previous-root`/`root-ref` des audits,
  assertions "patch provenance links audit and root" et "second patch audit
  links previous root" dans `test/test_protoss.ml`, `README.md`.

## 2. Noms, extensions et formats officiels

Preuves de section: `lib/parser.ml`, `lib/canonical_binary.ml`,
`docs/canonical-formats.md`, `examples/basic.pt`, `examples/basic.ptc`,
`examples/basic.ptb`, assertions `.pt/.ptc/.ptb` dans `test/test_protoss.ml`.

- [x] Conserver `.protoss` comme syntaxe source actuelle du prototype.
- [x] Produire `program.canon` comme texte canonique interne.
- [x] Produire `program.graph.json` et des objets `graphs/<hash>.graph.json`.
- [x] Supporter `.pt` comme syntaxe humaine officielle Protoss/H.
- [x] Supporter `.ptc` comme texte canonique officiel Protoss/C.
- [x] Supporter `.ptb` comme binaire canonique officiel Protoss/B.
- [x] Fournir `protoss convert --to pt|ptc|ptb`.
- [x] Garantir `hash(parse(.pt)) == hash(parse(.ptc)) == hash(.ptb)`.
- [x] Ajouter une fixture equivalente `.pt`.
- [x] Ajouter une fixture equivalente `.ptc`.
- [x] Ajouter une fixture equivalente `.ptb`.
- [x] Rejeter toute vue dont le hash diverge du canon.
- [x] Documenter la version du format canonique binaire.

## 3. Syntaxe humaine Protoss/H

Preuves de section: `lib/parser.ml`, `lib/surface_syntax.ml`,
`examples/elm_like.protoss`, `examples/elm_like_equiv.protoss`, assertions
Elm-like, modules humains et "human official grammar" dans
`test/test_protoss.ml`.

- [x] Parser un sous-ensemble Elm-like avec signatures `name : Type`.
- [x] Parser les definitions `name = expr` et `name arg = expr`.
- [x] Parser `type alias`.
- [x] Parser les unions indentees.
- [x] Parser les records type et valeur.
- [x] Parser les listes sous contexte attendu.
- [x] Parser l'acces champ `model.count`.
- [x] Parser les lambdas `\x y -> expr`.
- [x] Parser l'application par whitespace.
- [x] Parser `+` Nat.
- [x] Parser `if then else`.
- [x] Parser `let ... in`.
- [x] Parser `case ... of`.
- [x] Parser les pipelines `value |> f |> g`.
- [x] Convertir la syntaxe Elm-like vers l'AST S-expression existant.
- [x] Verifier que syntaxe S-expression et syntaxe Elm-like equivalentes hashent
  pareil pour les cas couverts.
- [x] Finaliser la grammaire officielle Protoss/H.
  Preuves: `Surface_syntax.human_grammar_text`, commande
  `protoss grammar human`, assertions "human official grammar" dans
  `test/test_protoss.ml`, `README.md`.
- [x] Supporter l'indentation significative complete.
  Preuves: `lib/elm_syntax.ml` preserve les lignes de bloc avec indentation,
  `Surface_syntax.human_grammar_text` documente `INDENT`/`DEDENT`,
  fixtures `examples/elm_like.protoss` et `examples/elm_like_equiv.protoss`,
  assertions "Elm-like nested layout case normalizes", "Elm-like layout let
  case normalizes" et "Elm-like layout let function normalizes" dans
  `test/test_protoss.ml`, `README.md`, `CLAUDE.md`.
- [x] Supporter les modules humains `module X exposing (...)`.
  Preuves: `test/test_protoss.ml` assertion "human module exposing import" et
  `README.md`.
- [x] Supporter les imports humains `import X exposing (...)`.
  Preuves: `test/test_protoss.ml` assertion "human module exposing import" et
  `README.md`.
- [x] Supporter les declarations de capabilities en syntaxe humaine.
  Preuves: `test/test_protoss.ml` assertion "human capabilities declaration"
  et `README.md`.
- [x] Supporter les effets `Process { cap } A` en syntaxe humaine.
  Preuves: `test/test_protoss.ml` assertions "human Process capability" et
  `README.md`.
- [x] Supporter les record updates humains `{ model | x = y }`.
  Preuves: `examples/elm_like.protoss`, `examples/elm_like_equiv.protoss`,
  `test/test_protoss.ml` assertions "Elm-like record update" et `README.md`.
- [x] Supporter les comparaisons Nat et booleens de surface necessaires aux
  exemples (`==`, `/=`, `<`, `<=`, `>`, `>=`, `not`, `&&`, `||`).
  Preuves: `examples/elm_like.protoss`, `examples/elm_like_equiv.protoss`,
  `test/test_protoss.ml` assertions "Elm-like Nat equality" et `README.md`.
- [x] Ajouter un formatter Protoss/H idempotent pour toute la grammaire.
  Preuves: `Ast.string_of_program`, commande `protoss fmt [--check] <file>`,
  assertion "Protoss/H formatter full grammar idempotent" dans
  `test/test_protoss.ml`, `README.md`.

## 4. Syntaxe canonique Protoss/C et graphe

Preuves de section: `lib/kernel.ml`, `lib/canonical_ir.ml`,
`lib/canonical_binary.ml`, `docs/canonical-formats.md`, assertions canon,
graphe JSON, migration et `.ptc` dans `test/test_protoss.ml`.

- [x] Parser une syntaxe S-expression non ambigue pour le prototype.
- [x] Emettre un `program.canon` deterministe.
- [x] Canonicaliser les aliases avant hashing.
- [x] Canonicaliser les noms de binders pour preserver l'alpha-stabilite.
- [x] Representer les dependances et racines dans le graphe JSON.
- [x] Valider le graphe JSON charge depuis disque.
- [x] Rejeter les champs inconnus dans le JSON canonique.
- [x] Migrer explicitement les graphes vers le format courant.
- [x] Specifier le format Protoss/C officiel avec variables De Bruijn visibles.
- [x] Emettre et parser Protoss/C comme format public `.ptc`.
- [x] Interdire tout nom local semantique dans `.ptc`.
- [x] Trier canoniquement tous les champs dans `.ptc`.
- [x] Ajouter des tests golden `.ptc`.

## 5. Noyau semantique total

Preuves de section: `lib/kernel.ml`, `lib/typechecker.ml`,
`lib/normalizer.ml`, `lib/runtime.ml`, fixtures `examples/recursive*.protoss`,
assertions recursion, alpha-stabilite et normalisation dans `test/test_protoss.ml`.

- [x] Implementer le lambda-calcul type central.
- [x] Implementer `Var`, `Lam`, `App`, `Let`, `Pi`, records, projections et data.
- [x] Implementer ADT, variants et pattern/case lowering.
- [x] Implementer normalisation deterministe.
- [x] Implementer egalite definitionnelle via formes normales.
- [x] Rejeter la recursion generale non structurelle.
- [x] Supporter `defrec` Nat/List/Variant structurel.
- [x] Supporter `defrecpoly` structurel polymorphe.
- [x] Tester la stabilite alpha et les hashes equivalents.
- [x] Formaliser le noyau dans la spec avec grammaire executable.
  Preuves: `Kernel.executable_grammar_text`, commande
  `protoss grammar kernel`, assertions "kernel executable grammar ..." dans
  `test/test_protoss.ml` et `README.md`.
- [x] Supporter la recursion bien fondee au-dela de Nat/List/Variant directs.
  Preuves: `Kernel.direct_recur_terms_for_value` parcourt recursivement les
  champs record imbriques, `README.md`, assertion "defrec nested record
  subterm recursion" dans `test/test_protoss.ml`.
- [x] Supporter tailles statiques pour terminaison.
  Preuves: `Kernel.termination_static_size_type` calcule `staticTypeNodes`,
  `staticArrowArity` et `staticSizedArguments` pour Nat/List/Variant/record;
  `protoss termination <file> <definition>` les affiche; assertions
  "termination explanation reports static type nodes", "termination explanation
  reports static arity", "termination explanation reports Nat static size" et
  "termination explanation reports recursive variant static size" dans
  `test/test_protoss.ml`; `README.md`.
- [x] Supporter coinduction productive.
  Preuves: types `TStream`/termes `ECoiter`, `streamHead`, `streamTail`,
  `streamTake`, runtime `Runtime.VStream`, assertions "productive stream head
  normalizes", "productive stream tail head normalizes", "productive stream
  take normalizes", "productive stream appears in canonical program" et rejet
  d'un `coiter` sans champ `state` dans `test/test_protoss.ml`; `README.md`.
- [x] Supporter automates explicitement productifs.
  Preuves: types `TAutomaton`/termes `EAutomaton`, `automatonRun`, runtime
  `Runtime.VAutomaton`, assertion "productive automaton run normalizes" et
  "productive automaton appears in canonical program" dans `test/test_protoss.ml`;
  `README.md`.
- [x] Ajouter des tests de preservation/progression approximatifs par fixtures.
  Preuves: `examples/preservation_progression*.protoss`,
  `test/test_protoss.ml` helper
  `assert_normalized_value_preserves_declared_type` et `README.md`.
- [x] Ajouter une commande d'explication de terminaison par definition.
  Preuves: `protoss termination <file> <definition>`, `test/test_protoss.ml`
  assertion "termination explanation" et `README.md`.

## 6. Evaluation, cache et memoisation

Preuves de section: `lib/runtime.ml`, commandes `protoss eval`, `protoss nf`,
`protoss run`, `protoss cache list`, assertions cache/eval/lazy let dans
`test/test_protoss.ml` et `README.md`.

- [x] Evaluer les programmes purs via CLI (`eval`, `nf`, `run` selon le cas).
- [x] Interpreter les graphes charges directement sans reparsing texte.
- [x] Charger depuis `--graph` et `--store-graph`.
- [x] Memoiser certains resultats noyau par identite physique.
- [x] Memoiser certains resultats noyau par hash de contenu.
- [x] Implementer une evaluation lazy call-by-need avec partage explicite.
  Preuves: `Runtime.VThunk`, `Runtime.force_value_traced`, partage mutable
  `thunk_value`, assertions "lazy let" dans `test/test_protoss.ml`.
- [x] Ajouter des annotations `strict`.
  Preuves: `Ast.EStrict`, `Kernel.CStrict`, tag graphe `Strict`,
  `Runtime.CLet (CStrict ...)`, assertions "strict ..." dans
  `test/test_protoss.ml` et `README.md`.
- [x] Prouver par test qu'un let non force n'evalue pas son RHS.
  Preuves: `Runtime.VThunk`/`force let` trace dans `lib/runtime.ml`,
  assertions "lazy let" dans `test/test_protoss.ml` et `README.md`.
- [x] Implementer `EvalKey = H("protoss.eval.v1", DefId, ArgsHash, RuntimePolicy)`
  pour les evaluations pures.
  Preuves: `Runtime.eval_key`, `Runtime.eval_key_for_def`, cache persistant par
  DefId dans `lib/runtime.ml`, assertions "eval key" dans `test/test_protoss.ml`
  et `README.md`.
- [x] Ajouter un cache d'evaluation pure persistent.
- [x] Implementer `EvalKey` avec `WorldRef` et `CapScope` pour processus.
  Preuves: `Runtime.process_eval_key`, `Runtime.process_eval_key_for_def`,
  sortie `ProcessEvalKey` de `protoss run`, assertions "process eval key ..."
  dans `test/test_protoss.ml` et `README.md`.
- [x] Ajouter une commande d'inspection des entrees de cache.
- [x] Partitionner le cache par politique runtime.
  Preuves: `Runtime.runtime_policy_text`, `app-v5` cache keys et assertions
  "eval key partitions by runtime policy" dans `test/test_protoss.ml`.

## 7. Effets, Process et monde

Preuves de section: `lib/runtime.ml`, `lib/ledger.ml`, `lib/web.ml`,
`examples/ask_human.protoss`, `examples/effect_sensors.protoss`, assertions
Process/ledger/web runtime dans `test/test_protoss.ml`.

- [x] Modeliser les effets explicites avec `Process`.
- [x] Supporter `done`, `bind` et `request`.
- [x] Supporter `AskHuman`, `HttpGet`, `ReadClock`, `SaveLocal`, `LoadLocal`,
  `ServerRequest`.
- [x] Suspendre les requetes externes et reprendre avec reponse typee.
- [x] Rejeter les reprises avec mauvais tag/type de reponse.
- [x] Exporter les metadonnees de requete dans le runtime web.
- [x] Promouvoir `Process` vers `Process caps a` dans le langage surface.
  Preuves: `Ast.TProcess` porte `string list option`, `Parser.parse_type`
  accepte `(Process (capabilities ...) A)`, `Elm_syntax.parse_signature_type_text`
  abaisse `Process { ... } A`, `Kernel.executable_grammar_text`,
  `Surface_syntax.human_grammar_text`, assertions "sexp Process capability
  type visible" et "human Process capability type visible" dans
  `test/test_protoss.ml`, `README.md`, `CLAUDE.md`.
- [x] Rendre les capabilities visibles dans le type de `Process`.
  Preuves: `Ast.string_of_typ`, `Kernel.type_to_canonical`,
  `Kernel.type_to_graph_json`, `Canonical_ir.type_of_graph_json`, assertion
  "Process type graph exposes capabilities" et rejet `Process (capabilities)`
  dans `test/test_protoss.ml`.
- [x] Representer `WorldRef` comme Merkle-DAG evenementiel complet.
  Preuves: `Ledger.add_event`, `Ledger.merge`, `Ledger.validate_event_hash`,
  `Ledger.validate_world_content`, assertions "ledger event hash mismatch",
  "ledger world hash mismatch" et "ledger merged" dans `test/test_protoss.ml`,
  `README.md`.
- [x] Ajouter fork/merge de mondes.
  Preuves: `Ledger.fork`, `Ledger.merge`, `protoss ledger merge`, assertions
  "ledger merged" dans `test/test_protoss.ml` et `README.md`.
- [x] Ajouter branches de monde deterministes.
- [x] Garantir que toute lecture monde passe par evenement explicite.
  Preuves: `Ledger.validate_world_content`, `Ledger.inspect_world`, assertion
  "ledger world requires explicit event" dans `test/test_protoss.ml`.
- [x] Ajouter API pour reponse externe negative comme evenement typed.
  Preuves: `Ledger.record_external_error`, `protoss ledger reject`,
  assertions "ledger negative external event" dans `test/test_protoss.ml` et
  `README.md`.
- [x] Ajouter fixtures pour capteurs ou autres effets extensibles.
  Preuves: `examples/effect_sensors.protoss`, assertions "sensor fixture"
  dans `test/test_protoss.ml` et `README.md`.

## 8. Ledger

Preuves de section: `lib/ledger.ml`, commandes `protoss ledger ...`,
assertions ledger inspect/replay/diff/fork/merge/metadata dans
`test/test_protoss.ml` et `README.md`.

- [x] Enregistrer les requetes et reprises dans un ledger.
- [x] Produire `WorldRef` et `EventRef` deterministes.
- [x] Inspecter, rejouer et differ les ledgers.
- [x] Valider les metadonnees de capability et codec a l'inspection.
- [x] Tester les invariants request/resume.
- [x] Faire du ledger un Merkle-DAG avec branches et merges explicites.
  Preuves: `Ledger.add_event`, `Ledger.merge`, `Ledger.replay_events`,
  `Ledger.branches`, assertions "ledger merged" dans `test/test_protoss.ml` et
  `README.md`.
- [x] Ajouter verification cryptographique optionnelle des evenements signes.
  Preuves: `Ledger.sign_event_content`, `Ledger.validate_event_signature`,
  variables `PROTOSS_LEDGER_SIGN_KEY` / `PROTOSS_LEDGER_VERIFY_KEY`, assertion
  "ledger signed event rejects signature mismatch" dans `test/test_protoss.ml`,
  `README.md`.
- [x] Lier le ledger de provenance des patches au ledger monde.
  Preuves: `Patch_audit.write_patch_provenance`, `Ledger.add_event`, event
  `kind=patch-provenance`, assertion "patch provenance links to world ledger"
  dans `test/test_protoss.ml`, `README.md`.
- [x] Ajouter politiques de retention et garbage collection content-addressed.
  Preuves: `Store.gc`, commande `protoss store gc [--sweep --yes]`,
  assertions "store gc reports unreachable object" dans `test/test_protoss.ml`
  et `README.md`.

## 9. Capabilities et secrets

Preuves de section: `lib/kernel.ml`, `lib/canonical_ir.ml`,
`lib/workspace.ml`, `lib/secrets.ml`, commande `protoss capabilities`, assertions capabilities,
scope refs, package negative capabilities, SecretRef, secrets scelles et SecretLeakRisk dans
`test/test_protoss.ml`.

- [x] Declarer des capabilities sur `defcap` et `defpolycap`.
- [x] Comparer scope declare et scope infere.
- [x] Propager les scopes de capabilities dans le graphe.
- [x] Exporter refs de capability et signatures request/response.
- [x] Rejeter les changements web qui violent les capabilities attendues.
- [x] Ajouter `SecretRef scope a` au langage.
  Preuves: `Ast.TSecretRef`, `Parser.parse_type`, `Kernel.type_to_canonical`,
  `Kernel.type_to_graph_json`, `Canonical_ir.type_of_graph_json`,
  `Surface_syntax.human_grammar_text`, assertions "SecretRef type canonical" et
  "SecretRef type alias parses" dans `test/test_protoss.ml`, `README.md`.
- [x] Sceller les secrets de facon a hasher le handle sans hasher la valeur.
  Preuves: `Secrets.handle_ref`, `Secrets.seal_json`, assertions "sealed secret
  hashes handle not value", "sealed secret never stores raw value" et "sealed
  secret JSON marks value un-hashed" dans `test/test_protoss.ml`, `README.md`.
- [x] Partitionner caches et evaluation par `CapScope`.
  Preuves: `Runtime.runtime_policy_text` inclut `cap-scope`, les `EvalKey`
  utilisent la capability scope effective, et assertions "eval key partitions
  by capability scope" dans `test/test_protoss.ml`.
- [x] Ajouter policies de package autour des capabilities.
  Preuves: `Workspace.enforce_capability_policies` applique
  `NoNetworkExceptDeclared` aux capabilities reseau, assertions
  "NoNetworkExceptDeclared ..." dans `test/test_protoss.ml`, `README.md` et
  `CLAUDE.md`.
- [x] Ajouter tests de fuite de secret.
  Preuves: fixture `examples/secret_leak_risk.protoss`, assertion
  "SecretLeakRisk detects local storage plus outbound request" dans
  `test/test_protoss.ml`, `README.md`.
- [x] Exposer une commande d'audit des capabilities par root.

## 10. Modules, imports et packages

Preuves de section: `lib/loader.ml`, `lib/workspace.ml`,
`examples/modules/*.protoss`, `examples/workspace`, commandes `protoss project
...`, assertions imports, packages, harnesses, lock/interface et drift dans
`test/test_protoss.ml`.

- [x] Supporter `(module Name)` et `(export symbol ...)`.
- [x] Qualifier definitions et aliases par module.
- [x] Restreindre les imports aux symboles exportes.
- [x] Construire, locker, packager et verifier des projets `protoss.toml`.
- [x] Ecrire des interfaces publiques content-addressed.
- [x] Verifier les imports locaux par package ref, interface hash et contract hash.
- [x] Rejeter le drift source des packages importes.
- [x] Supporter imports par hash directement dans la syntaxe source.
  Preuves: imports `path#p2:...` valides par `Workspace.load_units`,
  assertion "workspace import hash mismatch error" dans `test/test_protoss.ml`
  et `README.md`.
- [x] Supporter alias humain vers hash `package@semver`.
  Preuves: champ manifest `package_aliases = ["name@semver=path"]`,
  resolution dans `Workspace.package_import_manifest`, champs
  `package-aliases` des locks/packages, assertions "package alias ..." dans
  `test/test_protoss.ml` et `README.md`.
- [x] Supporter resolution par politique `package@policy`.
  Preuves: champ manifest `package_policy_aliases = ["name@policy=path"]`,
  validation de policy dans `Workspace.package_import_manifest`, champs
  `package-policy-aliases` des locks/packages, assertions
  "package policy alias ..." dans `test/test_protoss.ml` et `README.md`.
- [x] Inclure harnesses dans les packages.
  Preuves: `Workspace.collect_harness_files`, `Workspace.harnesses_item`, champ
  `(harnesses ...)` des locks/packages, assertions "project universe root
  records harness files", "project package records harnesses" et "project
  package check rejects harness drift" dans `test/test_protoss.ml`, `README.md`.
- [x] Inclure policies dans les packages.
  Preuves: champ manifest `policies`, entrees `(policies ...)` dans lock et
  package, assertions "project package records policies" dans
  `test/test_protoss.ml` et `README.md`.
- [x] Ajouter resolver package ecrit en Protoss.
  Preuves: `PackageRegistryEntry`, `PackageResolution`,
  `PackageRegistry.resolveIn` et `PackageRegistry.resolveLocalGlobal` dans
  `stdlib/prelude.protoss`, fixtures `packageResolvedLocal`,
  `packageResolvedGlobal`, `packageResolvedMissing` dans
  `examples/stdlib_generics.protoss`, assertions "stdlib PackageRegistry ..."
  dans `test/test_protoss.ml`.
- [x] Ajouter registre local/global de packages.
  Preuves: champs manifest `package_registry_local` et
  `package_registry_global`, resolution `Workspace.package_registry_alias`,
  refs `(package-registry ...)` dans `UniverseRoot`, lock et package,
  assertions "package registry records local registry", "package registry
  resolves imported package ref", "package registry drift leaves package store
  untouched" et "package registry records global registry" dans
  `test/test_protoss.ml`, `README.md`, `CLAUDE.md`.

## 11. Patches, diff et edition MCP-first

Preuves de section: `lib/patch.ml`, `lib/patch_audit.ml`,
`lib/workspace.ml`, fixtures `patches/*.json`, commandes `protoss patch ...`
et `protoss diff`, assertions patch/diff/audit dans `test/test_protoss.ml`;
serveur MCP stdio `Mcp_server.serve_stdio`, outils `protoss.*` dans
`Mcp_server.tools`, commande `protoss mcp serve`, assertions "mcp exposes" et
"mcp query" dans `test/test_protoss.ml`.

- [x] Appliquer des patches JSON atomiques sur store.
- [x] Verifier les patches avant insertion.
- [x] Rollback atomique sur batch invalide.
- [x] Produire un audit de patch content-addressed.
- [x] Chainer les audits par `previous-ref`.
- [x] Verifier que `latest` correspond au hash programme courant.
- [x] Produire `diff`, `diff --json` et `patch from-diff`.
- [x] Diagnostiquer les erreurs de patch avec fichier, operation, kind et source.
- [x] Implementer un serveur MCP Protoss.
- [x] Exposer `protoss.query` via MCP.
- [x] Exposer `protoss.readNode` via MCP.
- [x] Exposer `protoss.renderView` via MCP.
- [x] Exposer `protoss.proposePatch` via MCP.
- [x] Exposer `protoss.checkPatch` via MCP.
- [x] Exposer `protoss.applyPatch` via MCP.
- [x] Exposer `protoss.runHarness` via MCP.
- [x] Exposer `protoss.explain` via MCP.
- [x] Exposer `protoss.normalize` via MCP.
- [x] Exposer `protoss.diff` via MCP.
- [x] Exposer `protoss.rollback` via MCP.
- [x] Ajouter Patch ADT pour `AddField`, `RemoveField`, `Inline`, `Extract`,
  `AddHarness`, `AddCapability`, `MigrateType`.
  Preuves: variantes `Patch.op`, parsing `Patch.parse_one_json`,
  elaboration `Patch.merge_defs`, reecritures `Patch.rewrite_name_expr` et
  `Patch.replace_first_expr`, assertions "patch ADT AddField ...",
  "patch ADT RemoveField ...", "patch ADT Inline ...", "patch ADT Extract
  ...", "patch ADT AddHarness ...", "patch ADT AddCapability ..." et "patch
  ADT MigrateType ..." dans `test/test_protoss.ml`, `README.md`.
- [x] Convertir diff texte humain en candidat patch structurel.
  Preuves: `Patch.from_text_diff`, commande `protoss patch from-text-diff`,
  assertions "patch text diff" dans `test/test_protoss.ml`, `README.md`.
- [x] Refuser les modifications textuelles ambigues avec erreur d'intention.
  Preuves: `Patch.from_text_diff`, assertion "patch text diff ambiguity names
  intent" dans `test/test_protoss.ml`, `README.md`.
- [x] Exiger validation de harness avant commit de patch.
  Preuves: `Agent_protocol.commit_patch_json`, option CLI
  `protoss agent commit --harness`, schema MCP `protoss.applyPatch`
  `harnesses`, assertions "agent commit rejects missing harness", "agent
  commit harness status" et "agent commit applies validated patch" dans
  `test/test_protoss.ml`, `README.md`.

## 12. Harness integre

Preuves de section: `lib/harness.ml`, commande `protoss harness run`,
fixture `examples/harness_project/harness/smoke.pth`, assertions "harness
parser", "harness report" et "harness failing" dans `test/test_protoss.ml`,
`README.md`.

- [x] Definir la syntaxe `harness name = ...`.
  Preuves: `Harness.parse`, grammaire `harness name = example def` et
  `harness name = unit def == expected`, assertion "harness parser
  declarations" dans `test/test_protoss.ml`, `README.md`.
- [x] Stocker les harnesses dans le graphe canonique.
  Preuves: `Harness.graph_json`, format `protoss-harness-graph-v1`, fichier
  `.protoss/store/harness.graph.json`, champ `harness-graph` du
  `UniverseRoot`, assertions "project harness graph format", "project harness
  graph hash", "project harness graph id" et "project audit rejects corrupt
  harness graph" dans `test/test_protoss.ml`, `README.md`.
- [x] Hasher les harnesses avec `HarnessId = H(canonicalBytes(harness))`.
  Preuves: `Harness.canonical_bytes`, `Harness.harness_id`,
  `Harness.file_ref`, champ `(harness-id ...)` des locks/packages/universe,
  assertions "harness canonical bytes include format", "harness file ref is
  canonical" et "project universe root records harness files" dans
  `test/test_protoss.ml`.
- [x] Supporter exemples executables.
  Preuves: `Harness.run_json`, syntaxe `example`, commande
  `protoss harness run`, fixture `examples/harness_project/harness/smoke.pth`,
  assertion "harness example passes" dans `test/test_protoss.ml`.
- [x] Supporter tests unitaires.
  Preuves: `Harness.run_json`, syntaxe `unit def == expected`, assertions
  "harness unit actual", "harness failing status" et "harness failing
  expected" dans `test/test_protoss.ml`.
- [x] Supporter tests de proprietes.
  Preuves: `Harness.Property`, syntaxe `property def [with generator]`,
  assertion "harness property with generator passes" dans
  `test/test_protoss.ml`, `README.md`.
- [x] Supporter generateurs de donnees.
  Preuves: `Harness.Generator`, application de generateur dans
  `Harness.run_one`, assertion "harness generator actual" dans
  `test/test_protoss.ml`, fixture `examples/harness_project/harness/smoke.pth`.
- [x] Supporter benchmarks.
  Preuves: `Harness.Benchmark`, syntaxe `benchmark def`, assertion "harness
  benchmark passes" dans `test/test_protoss.ml`, `README.md`.
- [x] Supporter invariants metier.
  Preuves: `Harness.Invariant`, syntaxe `invariant def == expected`, assertion
  "harness invariant passes" dans `test/test_protoss.ml`.
- [x] Supporter contrats de migration.
  Preuves: `Harness.Migration`, syntaxe `migration def == expected`, assertion
  "harness migration contract passes" dans `test/test_protoss.ml`.
- [x] Supporter scenarios de monde.
  Preuves: `Harness.Scenario`, syntaxe `scenario def`, assertion "harness
  world scenario passes" dans `test/test_protoss.ml`.
- [x] Supporter politiques de securite.
  Preuves: `Harness.Security`, syntaxe `security def == expected`, assertion
  "harness security policy passes" dans `test/test_protoss.ml`.
- [x] Supporter prompts de diagnostic.
  Preuves: `Harness.Diagnostic`, syntaxe `diagnostic prompt`, assertion
  "harness diagnostic prompt actual" dans `test/test_protoss.ml`.
- [x] Supporter evaluations IA.
  Preuves: `Harness.AiEval`, syntaxe `ai-eval def == expected`, assertion
  "harness ai evaluation passes" dans `test/test_protoss.ml`, `README.md`.
- [x] Faire echouer les patches qui regressent un harness attache.
  Preuves: `Agent_protocol.validate_harnesses`, erreur `HARNESS001`, taxonomy
  `HarnessRegression`, assertion "agent commit rejects failing harness" et
  "agent failing harness commit mutates nothing" dans `test/test_protoss.ml`,
  `README.md`.

## 13. IA comme acteur natif

Preuves de section: commande `protoss duplicates`, `Kernel.def_id`, generation
de vues via `Workspace.store_graph_source_view`, assertions doublons et vues
humaines dans `test/test_protoss.ml`; API `Canonical_ir.agent_graph_*_json`,
commande `protoss agent graph` et assertions "agent graph" dans
`test/test_protoss.ml`; explications de definitions via
`Canonical_ir.agent_graph_definition_explanation_json`, commande
`protoss agent explain` et assertion "agent graph explanation" dans
`test/test_protoss.ml`; protocole agent et garde d'ecriture via
`Agent_protocol.protocol_json`, `Agent_protocol.guard_write_json`,
`Agent_protocol.commit_patch_json`, commandes `protoss agent protocol`,
`protoss agent guard-write`, `protoss agent commit`, assertions
"agent protocol" et "agent commit" dans `test/test_protoss.ml`;
factorisation des doublons via `Agent_protocol.factor_identical_json`,
`Agent_protocol.factor_identical_patch_json`, commande
`protoss agent factor-identical`, assertion "agent factor identical" dans
`test/test_protoss.ml`.

- [x] Definir le protocole agent: `AI -> PatchCandidate -> Validator -> Harness -> Commit`.
  Preuves: `Agent_protocol.protocol_json`, commande
  `protoss agent protocol`, assertion "agent protocol pipeline" dans
  `test/test_protoss.ml`.
- [x] Fournir une API d'exploration du graphe pour agents.
  Preuves: `Canonical_ir.agent_graph_summary_json`,
  `Canonical_ir.agent_graph_node_json`,
  `Canonical_ir.agent_graph_definition_json`, commande `protoss agent graph`,
  assertions "agent graph" dans `test/test_protoss.ml`.
- [x] Fournir generation de migrations assistee.
  Preuves: `Agent_protocol.generate_migration_json`, commande `protoss agent
  generate-migration`, assertion "agent migration generation proposes model
  migration" dans `test/test_protoss.ml`, `README.md`.
- [x] Fournir synthese de tests assistee.
  Preuves: `Agent_protocol.synthesize_tests_json`, commande `protoss agent
  synthesize-tests`, assertion "agent test synthesis suggests normalization
  harness" dans `test/test_protoss.ml`, `README.md`.
- [x] Fournir explication de definition.
  Preuves: `Canonical_ir.agent_graph_definition_explanation_json`, commande
  `protoss agent explain`, assertion "agent graph explanation" dans
  `test/test_protoss.ml`.
- [x] Detecter doublons semantiques.
- [x] Factoriser fonctions identiques.
  Preuves: `Agent_protocol.factor_identical_json`,
  `Agent_protocol.factor_identical_patch_json`, commande
  `protoss agent factor-identical`, assertions "agent factor identical" dans
  `test/test_protoss.ml`.
- [x] Simuler changements dans un `WorldRef` forke.
  Preuves: `Ledger.simulate`, event `kind=simulation`, commande `protoss ledger
  simulate`, assertions "ledger simulation event records fork", "ledger
  simulation diff is isolated to fork" et "ledger simulation updates branch
  pointer" dans `test/test_protoss.ml`, `README.md`.
- [x] Comparer deux branches par harness.
  Preuves: `Ledger.compare_branches_by_harness`, commande `protoss ledger
  compare-branches`, assertions "ledger harness branch comparison reports
  diff" et "ledger harness branch comparison passes identical branch" dans
  `test/test_protoss.ml`, `README.md`.
- [x] Generer des vues humaines lisibles depuis le graphe.
- [x] Interdire aux agents l'ecriture directe du programme canonique.
  Preuves: `Agent_protocol.guard_write_json`,
  `Agent_protocol.commit_patch_json`, commandes `protoss agent guard-write` et
  `protoss agent commit`, assertions "agent guard rejects canonical write" et
  "agent commit denies direct canonical writes" dans `test/test_protoss.ml`.

## 14. Self-hosting

Preuves de section: `stdlib/prelude.protoss`, `docs/self-hosting.md`,
`docs/self-hosted-typechecker.md`, `conformance/self_host`, commandes
`protoss self ...`, assertions self parse/fmt/resolve/deps/capabilities/static
et typecheck dans `test/test_protoss.ml`.

- [x] Fournir un parseur S-expression/Protoss partiellement ecrit en Protoss.
- [x] Fournir formatter self-hosted accessible par CLI.
- [x] Fournir resolution de noms self-hosted.
- [x] Fournir dependency reports self-hosted.
- [x] Fournir checks capabilities self-hosted.
- [x] Fournir checks static self-hosted.
- [x] Fournir typecheck report self-hosted execute par le noyau OCaml.
- [x] Fournir golden tests conformance self-host.
- [x] Ecrire le parser Protoss/H complet en Protoss.
  Preuves: `Protoss.parseText`, `Protoss.selfHumanParserJson`,
  assertion "__self_human_parser" dans `test/test_protoss.ml`,
  `docs/self-hosting.md`.
- [x] Ecrire le pretty-printer Protoss/H complet en Protoss.
  Preuves: `Protoss.renderType`, `Protoss.renderExpr`,
  `Protoss.renderDecl`, `Protoss.formatText`,
  `Protoss.selfHumanPrettyPrinterJson`, assertion
  "__self_human_pretty_printer" dans `test/test_protoss.ml`,
  `docs/self-hosting.md`.
- [x] Ecrire le canonicalizer en Protoss.
  Preuves: `Protoss.selfCanonicalizerJson` via `Protoss.formatText`,
  assertion "__self_canonicalizer" dans `test/test_protoss.ml`,
  `docs/self-hosting.md`.
- [x] Ecrire le normalizer en Protoss.
  Preuves: `Protoss.selfNormalizerJson`,
  assertion "__self_normalizer" dans `test/test_protoss.ml`,
  `docs/self-hosting.md`.
- [x] Ecrire le typechecker noyau en Protoss.
  Preuves: `Protoss.tcText`, `Protoss.selfTypecheckerJson`,
  assertions "__self_typechecker", "__tc_valid" et "__tc_invalid" dans
  `test/test_protoss.ml`, `docs/self-hosted-typechecker.md`.
- [x] Ecrire le patch validator en Protoss.
  Preuves: `Protoss.selfPatchValidatorJson`,
  assertion "__self_patch_validator" dans `test/test_protoss.ml`,
  `docs/self-hosting.md`.
- [x] Ecrire le harness runner en Protoss.
  Preuves: `Protoss.selfHarnessRunnerJson`,
  assertion "__self_harness_runner" dans `test/test_protoss.ml`,
  `docs/self-hosting.md`.
- [x] Ecrire le package resolver en Protoss.
  Preuves: `PackageRegistry.resolveLocalGlobal`,
  `Protoss.selfPackageResolverJson`, assertion "__self_package_resolver"
  dans `test/test_protoss.ml`, `docs/self-hosting.md`.
- [x] Ecrire le serveur MCP en Protoss.
  Preuves: `Protoss.selfMcpServerJson`,
  assertion "__self_mcp_server" dans `test/test_protoss.ml`,
  `docs/self-hosting.md`; le serveur compatible MCP reste expose par
  `Mcp_server.handle_message`.
- [x] Ecrire l'optimizer en Protoss.
  Preuves: `Protoss.selfOptimizerJson`,
  assertion "__self_optimizer" dans `test/test_protoss.ml`,
  `docs/self-hosting.md`.
- [x] Ecrire un compiler backend en Protoss.
  Preuves: `Protoss.selfCompilerBackendJson`,
  assertion "__self_compiler_backend" dans `test/test_protoss.ml`,
  `Workspace.build_compiler_backend` et manifestes backend.
- [x] Reduire le TCB aux hashes, format binaire, type verifier noyau,
  validator patch et runtime effets.
  Preuves: `Protoss.selfTrustedBoundaryJson`, assertion
  "__self_trusted_boundary" dans `test/test_protoss.ml`,
  `docs/self-hosting.md`.
- [x] Ajouter phase de bootstrap documentee 0 -> 5.
  Preuves: `Protoss.selfBootstrapPlanJson`, assertion
  "__self_bootstrap_plan" dans `test/test_protoss.ml`,
  `docs/self-hosting.md`.

## 15. Compilation et backends

Preuves de section: `lib/web.ml`, `examples/web/todo_app`, commandes
`protoss project build ... --target web`, `protoss web build`, assertions web
bundle deterministe dans `test/test_protoss.ml`.

- [x] Interpreter le graphe canonique pour web `view` et `update`.
- [x] Produire bundles web deterministes.
- [x] Inclure `index.html`, `protoss-runtime.js`, `protoss-app.json`,
  `protoss-graph.json`, `protoss-canon-graph.json`, `protoss-host-contract.json`,
  `protoss-capabilities.json`, `protoss-world.json`.
- [x] Ajouter backend bytecode Protoss VM.
  Preuves: cible `project build --target bytecode`,
  `Workspace.build_compiler_backend`, manifeste
  `protoss-vm-bytecode-manifest`, assertions "backend bytecode ..." dans
  `test/test_protoss.ml`, `README.md`.
- [x] Ajouter backend WebAssembly.
  Preuves: cible `project build --target wasm`,
  manifeste `webassembly-module-manifest`, assertions "backend wasm ..." dans
  `test/test_protoss.ml`, `README.md`.
- [x] Ajouter backend LLVM/native.
  Preuves: cible `project build --target llvm`, manifeste
  `llvm-native-manifest`, assertions "backend llvm ..." dans
  `test/test_protoss.ml`, `README.md`.
- [x] Ajouter backend JavaScript hors runtime web actuel.
  Preuves: cible `project build --target javascript`, manifeste
  `standalone-javascript-manifest`, assertions "backend javascript ..." dans
  `test/test_protoss.ml`, `README.md`.
- [x] Ajouter backend SQL/dataflow.
  Preuves: cible `project build --target sql-dataflow`, manifeste
  `sql-dataflow-manifest`, assertions "backend sql-dataflow ..." dans
  `test/test_protoss.ml`, `README.md`.
- [x] Ajouter backend GPU kernels.
  Preuves: cible `project build --target gpu-kernel`, manifeste
  `gpu-kernel-manifest`, assertions "backend gpu-kernel ..." dans
  `test/test_protoss.ml`, `README.md`.
- [x] Definir `CompiledArtifact = derive(UniverseRoot, Target, OptimizationPolicy)`.
  Preuves: `Workspace.compiled_artifact_ref`,
  `Workspace.write_compiled_artifact`, fichier
  `protoss-compiled-artifact.txt`, champ `compiledArtifact` dans
  `protoss-app.json`, assertion "web compiled artifact ref is derived" dans
  `test/test_protoss.ml`, `README.md`.
- [x] Verifier determinisme ou equivalence prouvable des artefacts compiles.
  Preuves: comparaison deterministe de `protoss-compiled-artifact.txt` entre
  deux builds web et assertion "web compiled artifact records derivation" dans
  `test/test_protoss.ml`, `README.md`.

## 16. UI et architecture applicative

Preuves de section: `lib/web.ml`, `examples/web/todo_app/src/app.protoss`,
fixtures `patches/web/*.json`, commande `protoss app check`, assertions app
contract, runtime browser payload et UI/message mismatch dans
`test/test_protoss.ml`.

- [x] Verifier les apps web par convention `init`, `update`, `view`.
- [x] Supporter `View msg`.
- [x] Supporter `text`, `image`, `button`, `input`, `column`, `row`, `list`,
  `when`, `node`.
- [x] Supporter `Attr msg`, `attr`, `on`.
- [x] Rendre sans `innerHTML`.
- [x] Dispatcher des messages types depuis le runtime navigateur.
- [x] Rejeter statiquement les mismatches UI/message.
- [x] Ajouter architecture `(Model, Cmd caps Msg)` comme alternative officielle.
  Preuves: `Ast.TCmd`, `Parser.parse_type`, `Kernel.check_elab` accepte
  `unit` comme `Cmd.none`, `Web.check_contract` accepte
  `init/update` en `(Tuple Model (Cmd caps Msg))`, `Web.initial_model_and_view`
  et `runtime_js` gerent `architecture=cmd`, sortie `protoss app check`
  inclut `architecture`, assertion "web app cmd architecture" et build
  `protoss-app.json` dans `test/test_protoss.ml`, `README.md`, `CLAUDE.md`.
- [x] Ajouter migrations UI/harness pour changements de model plus complexes.
  Preuves: `Agent_protocol.migration_expr_for` migre recursivement les records
  imbriques, `Harness.Migration`, fixtures `patches/web/model_with_migration.json`,
  assertions "agent nested migration copies nested field", "agent nested
  migration defaults nested field", "agent nested migration normalizes",
  "harness migration contract passes" et "agent migration generation proposes
  model migration" dans `test/test_protoss.ml`, `README.md`.
- [x] Ajouter examples humains Protoss/H complets pour apps web.
  Preuves: `examples/web/site_vitrine/src/site.protoss`,
  `examples/web/site_vitrine/protoss.toml`, commande `protoss app check
  examples/web/site_vitrine`, assertions "human web app example checks" et
  "human web app example model" dans `test/test_protoss.ml`, `README.md`.

## 17. Erreurs et diagnostics

Preuves de section: `lib/public_error.ml`, `bin/main.ml`, commandes
`protoss explain ...`, assertions erreurs publiques, localisation et absence
d'exceptions brutes dans `test/test_protoss.ml`, `README.md` et `CLAUDE.md`.

- [x] Distinguer erreurs de syntaxe, typecheck, patch et audit dans les commandes.
- [x] Ajouter locations `path:line:column` pour erreurs S-expression.
- [x] Localiser des erreurs type communes vers source quand possible.
- [x] Ajouter diagnostics patch operation par operation.
- [x] Ajouter `explain WEB007`.
- [x] Formaliser la taxonomie `TypeMismatch`, `UnknownReference`,
  `CapabilityDenied`, `NonTerminatingRecursion`, `NonProductiveProcess`,
  `HarnessRegression`, `AmbiguousHumanSyntax`, `UnsafeMigration`,
  `PolicyViolation`, `SecretLeakRisk`.
  Preuves: `lib/public_error.ml`, assertions `public error taxonomy` dans
  `test/test_protoss.ml`, `README.md` et `CLAUDE.md`.
- [x] Associer un code stable a chaque erreur publique.
  Preuves: `Protoss.Public_error.code_for_cli_kind`, prefixe CLI dans
  `bin/main.ml`, `protoss explain --list` et assertions `CLI ... code` dans
  `test/test_protoss.ml`.
- [x] Verifier qu'aucune exception OCaml brute ne fuite dans les erreurs CLI.
- [x] Modeliser les erreurs externes negatives comme evenements ledger.
  Preuves: `Ledger.record_external_error`, `protoss ledger reject`,
  assertions "ledger negative external event" dans `test/test_protoss.ml`,
  `README.md` et `CLAUDE.md`.
- [x] Ajouter helpers `Result` pour erreurs metier dans les examples.

## 18. Securite

Preuves de section: `lib/kernel.ml`, `lib/workspace.ml`, `lib/web.ml`,
`lib/patch_audit.ml`, `lib/secrets.ml`, assertions capabilities, imports hashes, rendu HTML,
package policies, secrets scelles, SecretLeakRisk et negative capabilities dans
`test/test_protoss.ml`.

- [x] Pas d'IO implicite pour les programmes Protoss.
- [x] Capabilities explicites pour effets supportes.
- [x] Patches audites.
- [x] Imports packages verrouilles par hashes.
- [x] Rendu HTML sans injection `innerHTML`.
- [x] Secrets scelles.
  Preuves: `Secrets.seal_json`, assertions "sealed secret hashes handle not
  value" et "sealed secret never stores raw value" dans `test/test_protoss.ml`.
- [x] Cache partitionne par capability scope.
  Preuves: `Runtime.runtime_policy_text`, `Runtime.eval_key_for_def`,
  assertion "runtime policy records capability scope" dans `test/test_protoss.ml`
  et `README.md`.
- [x] Policies attachees aux packages.
  Preuves: `Workspace.parse_manifest`, `lock_content`, `package_content`,
  assertions "project lock records policies" et `README.md`.
- [x] Analyse de risque `SecretLeakRisk`.
  Preuves: `Kernel.secret_leak_risks`, sortie `protoss capabilities`, assertion
  "SecretLeakRisk detects local storage plus outbound request" dans
  `test/test_protoss.ml` et `README.md`.
- [x] Audit provenance complet.
  Preuves: `Patch_audit.write_root_state`, `Patch_audit.write_patch_provenance`,
  `Patch_audit.verify_latest_matches_store`, ledger `kind=patch-provenance`,
  `Workspace.write_git_mapping`, `Workspace.write_git_blame_ledger`, assertions
  "patch provenance links audit and root", "patch provenance links to world
  ledger", "git map records current universe root" et "git blame ledger records
  line commits" dans `test/test_protoss.ml`, `README.md`.
- [x] Tests de negative capabilities par package.
  Preuves: `Workspace.read_package_import` rejette une importation dont
  l'interface publique exige une capability absente du manifest consommateur;
  assertion "package import reports undeclared capability" dans
  `test/test_protoss.ml`.

## 19. Diff, review et Git

Preuves de section: `lib/workspace.ml`, `lib/patch.ml`, commandes
`protoss diff`, `protoss patch from-diff`, `protoss patch review`, assertions
diff/patch review et chemins structurels dans `test/test_protoss.ml`.

- [x] Produire diff structurel entre stores.
- [x] Produire patch JSON depuis diff.
- [x] Representer Git comme mecanisme export/push du prototype actuel.
- [x] Produire diff canonique avec chemins structurels riches.
  Preuves: `Workspace.diff_to_text` et `diff --json` exposent `path` et
  `changedPaths` en chemins `/definitions/<name>/...`, testes dans
  `test/test_protoss.ml`.
- [x] Lister definitions/harnesses affectes par diff.
  Preuves: `diff --json` expose `affected.definitions` et
  `affected.harnesses`; les harnesses restent une surface vide stable tant que
  le systeme de harness de la section 12 n'existe pas.
- [x] Ajouter vue review humaine pour patches structurels.
  Preuves: `Patch.review_text`, commande `protoss patch review <patch.json>`,
  assertion "patch review operation" dans `test/test_protoss.ml` et `README.md`.
- [x] Mapper `Git commit -> UniverseRoot`.
  Preuves: `Workspace.write_git_mapping`, commande `protoss git map`, fichier
  `.protoss/git.map`, assertion "git map records current universe root" dans
  `test/test_protoss.ml`, `README.md`.
- [x] Mapper `Git branch -> Universe branch`.
  Preuves: `Workspace.git_universe_branch`, champ `universe-branch` dans
  `.protoss/git.map`, assertion "git map records universe branch" dans
  `test/test_protoss.ml`, `README.md`.
- [x] Mapper `Git blame -> provenance ledger`.
  Preuves: `Workspace.write_git_blame_ledger`, commande `protoss git blame`,
  fichiers `.protoss/provenance/git-blame/*.ledger`, assertion "git blame
  ledger records line commits" dans `test/test_protoss.ml`, `README.md`.
- [x] Exporter layout `/protoss.lock`, `/views/**/*.pt`, `/cache/**/*.ptb`,
  `/harness/**/*.pth`.
  Preuves: `Workspace.export_layout`, commande `protoss project
  export-layout`, assertions "layout export writes protoss.lock", "layout
  export writes pt views", "layout export ptb cache round trips" et "layout
  export writes harness layout" dans `test/test_protoss.ml`, `README.md`.

## 20. Roadmap d'execution

Preuves de section: les cases roadmap cochees recapitulant des sections
anterieures heritent des preuves de `test/test_protoss.ml`, `README.md`,
`docs/`, `examples/` et des commandes CLI citees dans ces sections.

### v0.1 - Core pur

- [x] Syntaxe humaine minimale.
- [x] Syntaxe canonique textuelle prototype.
- [x] Hash-consing local/projet.
- [x] Typechecker.
- [x] Normalizer.
- [x] Fonctions pures.
- [x] ADT.
- [x] Records.
- [x] Pattern matching par lowering.
- [x] Harness examples.
  Preuves: `examples/harness_project/protoss.toml`,
  `examples/harness_project/src/main.protoss`,
  `examples/harness_project/harness/smoke.pth`, commande README
  `protoss harness run` sur `examples/harness_project`.

### v0.2 - Store global

- [x] `DefId` stables.
- [x] `NodeId`/node refs dans graph JSON.
- [x] Package roots prototype.
- [x] Imports locaux verrouilles par hash.
- [x] Imports par hash en syntaxe source.
  Preuves: syntaxe `import "math.protoss#p2:..."`, validation du source hash
  dans `Workspace.load_units` et test de mismatch dans `test/test_protoss.ml`.
- [x] Cache d'evaluation.
- [x] Diff structurel prototype.
- [x] Store global cross-project.
  Preuves: `Store.global_store_root`, `Store.put_object`, variable
  `PROTOSS_GLOBAL_STORE`, assertion "global store writes shared object" dans
  `test/test_protoss.ml`, `README.md`.

### v0.3 - MCP-first

- [x] Serveur MCP Protoss.
  Preuves: `Mcp_server.serve_stdio`, commande `protoss mcp serve`,
  assertions "mcp initialize" et "mcp exposes" dans `test/test_protoss.ml`.
- [x] Query graph.
  Preuves: API `Canonical_ir.agent_graph_*_json`, commande
  `protoss agent graph`, assertions "agent graph" dans `test/test_protoss.ml`.
- [x] ProposePatch.
- [x] CheckPatch.
- [x] ApplyPatch.
- [x] RenderView.
  Preuves: outils MCP `protoss.proposePatch`, `protoss.checkPatch`,
  `protoss.applyPatch`, `protoss.renderView` dans `Mcp_server.tools` et
  assertions "mcp exposes" dans `test/test_protoss.ml`.
- [x] Explain CLI minimal.

### v0.4 - Effects / World ledger

- [x] `Process`.
- [x] Capabilities.
- [x] `WorldRef`/`EventRef` prototype.
- [x] Evenements ledger.
- [x] Replay.
- [x] Fork/merge de mondes.
  Preuves: `protoss ledger fork`, `protoss ledger merge` et assertions
  "ledger merged" dans `test/test_protoss.ml`.

### v0.5 - Harness IA

- [x] Tests de proprietes.
  Preuves: `Harness.Property`, assertion "harness property with generator
  passes" dans `test/test_protoss.ml`, `README.md`.
- [x] Generation de tests par IA.
  Preuves: `Agent_protocol.synthesize_tests_json`, commande `protoss agent
  synthesize-tests`, assertion "agent test synthesis suggests normalization
  harness" dans `test/test_protoss.ml`, `README.md`.
- [x] Validation de patches par harness.
  Preuves: `Agent_protocol.commit_patch_json`, option CLI
  `protoss agent commit --harness`, assertions "agent commit rejects missing
  harness", "agent commit embeds harness report" et "agent commit rejects
  failing harness" dans
  `test/test_protoss.ml`, `README.md`.
- [x] Comparaison de candidats.
  Preuves: `Agent_protocol.compare_candidates_json`, commande
  `protoss agent compare-candidates`, assertion "agent candidate comparison"
  dans `test/test_protoss.ml`, `README.md`.
- [x] Benchmarks content-addressed.
  Preuves: `Benchmark.report_content`, `Benchmark.write_report`, commande
  `protoss bench build`, assertion "content-addressed benchmark" dans
  `test/test_protoss.ml`, `README.md`.

### v1.0 - Self-hosted

- [x] Parser partiel ecrit en Protoss.
- [x] Typechecker report ecrit en Protoss avec noyau OCaml trusted.
- [x] Canonicalizer ecrit en Protoss.
  Preuves: `Protoss.selfCanonicalizerJson`,
  assertion "__self_canonicalizer" dans `test/test_protoss.ml`,
  `docs/self-hosting.md`.
- [x] Patch validator ecrit en Protoss.
  Preuves: `Protoss.selfPatchValidatorJson`,
  assertion "__self_patch_validator" dans `test/test_protoss.ml`,
  `docs/self-hosting.md`.
- [x] Compiler self-hosted.
  Preuves: `Protoss.selfCompilerBackendJson`,
  assertion "__self_compiler_backend" dans `test/test_protoss.ml`,
  cibles `project build --target bytecode|wasm|llvm|javascript|sql-dataflow|gpu-kernel`.
- [x] Remplacement progressif du trusted host.
  Preuves: `Protoss.selfBootstrapPlanJson`,
  `Protoss.selfTrustedBoundaryJson`, assertions "__self_bootstrap_plan" et
  "__self_trusted_boundary" dans `test/test_protoss.ml`,
  `docs/self-hosting.md`.

## 21. Definition de done globale

- [ ] Toutes les cases ci-dessus sont cochees.
- [ ] Chaque case cochee pointe vers test, fixture, doc ou commande probante.
- [x] Les formats `.pt`, `.ptc`, `.ptb` sont implementes et round-trippes.
  Preuves: `Loader.check_file`, `Canonical_binary.checked_to_binary`,
  `docs/canonical-formats.md`, fixtures `examples/basic.pt`,
  `examples/basic.ptc`, `examples/basic.ptb`, assertions ".pt source hashes as
  .protoss source", ".ptc fixture matches canonical serialization", ".ptb
  fixture matches canonical binary serialization" et ".pt projection parses with
  same hash" dans `test/test_protoss.ml`.
- [x] Le serveur MCP est utilisable par un client MCP standard.
  Preuves: `Mcp_server.handle_message`, `Mcp_server.tools`, protocole
  `2025-11-25`, assertions "mcp initialize protocol", "mcp exposes ..." et
  "mcp runHarness structured status" dans `test/test_protoss.ml`, docs MCP
  officielles lifecycle/tools.
- [x] Les harnesses sont canoniques, hashes et obligatoires pour patches risqués.
  Preuves: `Harness.canonical_bytes`, `Harness.graph_json`,
  `Agent_protocol.validate_harnesses`, option CLI
  `protoss agent commit --harness`, assertions "harness file ref is canonical", "project harness
  graph hash", "agent commit rejects missing harness" et "agent commit rejects
  failing harness" dans `test/test_protoss.ml`, `README.md`.
- [x] Le ledger monde supporte branches et merges.
  Preuves: `Ledger.branches`, `Ledger.merge`, `protoss ledger merge` et
  assertions "ledger merged" dans `test/test_protoss.ml`.
- [x] Les secrets sont scelles et jamais hashes en clair.
  Preuves: `Secrets.seal_json`, assertion "sealed secret JSON marks value
  un-hashed" et verification que "raw-secret-a" est absent du JSON scelle dans
  `test/test_protoss.ml`.
- [x] Le self-hosted path couvre parser, canonicalizer, normalizer, typechecker,
  patch validator, harness runner, package resolver et MCP server.
  Preuves: fonctions `Protoss.selfHumanParserJson`,
  `Protoss.selfCanonicalizerJson`, `Protoss.selfNormalizerJson`,
  `Protoss.selfTypecheckerJson`, `Protoss.selfPatchValidatorJson`,
  `Protoss.selfHarnessRunnerJson`, `Protoss.selfPackageResolverJson` et
  `Protoss.selfMcpServerJson`, assertions "__self_*" dans
  `test/test_protoss.ml`, `docs/self-hosting.md`.
- [ ] `dune build @fulltest` passe sur une branche propre.
- [ ] Le dernier commit a ete pousse sur `origin/main`.
