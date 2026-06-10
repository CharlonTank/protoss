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
- [ ] Synchroniser `README.md`, `CLAUDE.md` et ce TODO quand une fonctionnalite
  change de statut.
- [ ] Documenter pour chaque case l'emplacement de son test principal.

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
- [ ] Faire du `UniverseRoot` la source de verite de toutes les commandes projet.
- [x] Ajouter un store global partage entre projets pour l'interning des noeuds.
  Preuves: `Store.global_store_root`, `Store.put_object`, variable
  `PROTOSS_GLOBAL_STORE`, assertion "global store writes shared object" dans
  `test/test_protoss.ml`, `README.md`.
- [x] Dedupliquer physiquement les noeuds identiques entre projets distincts.
  Preuves: hardlink depuis `Store.put_object` vers le store global, assertions
  "global store project ... hardlink inode" dans `test/test_protoss.ml`,
  `README.md`.
- [x] Exposer une commande de comparaison semantique entre deux roots.
- [ ] Ajouter une provenance native liee aux roots et patches.

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
Elm-like et modules humains dans `test/test_protoss.ml`.

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
- [ ] Finaliser la grammaire officielle Protoss/H.
- [ ] Supporter l'indentation significative complete.
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
- [ ] Ajouter un formatter Protoss/H idempotent pour toute la grammaire.

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
- [ ] Supporter la recursion bien fondee au-dela de Nat/List/Variant directs.
- [ ] Supporter tailles statiques pour terminaison.
- [ ] Supporter coinduction productive.
- [ ] Supporter automates explicitement productifs.
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
- [ ] Implementer une evaluation lazy call-by-need avec partage explicite.
- [ ] Ajouter des annotations `strict`.
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
- [ ] Promouvoir `Process` vers `Process caps a` dans le langage surface.
- [ ] Rendre les capabilities visibles dans le type de `Process`.
- [ ] Representer `WorldRef` comme Merkle-DAG evenementiel complet.
- [x] Ajouter fork/merge de mondes.
  Preuves: `Ledger.fork`, `Ledger.merge`, `protoss ledger merge`, assertions
  "ledger merged" dans `test/test_protoss.ml` et `README.md`.
- [x] Ajouter branches de monde deterministes.
- [ ] Garantir que toute lecture monde passe par evenement explicite.
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
- [ ] Ajouter verification cryptographique optionnelle des evenements signes.
- [ ] Lier le ledger de provenance des patches au ledger monde.
- [x] Ajouter politiques de retention et garbage collection content-addressed.
  Preuves: `Store.gc`, commande `protoss store gc [--sweep --yes]`,
  assertions "store gc reports unreachable object" dans `test/test_protoss.ml`
  et `README.md`.

## 9. Capabilities et secrets

Preuves de section: `lib/kernel.ml`, `lib/canonical_ir.ml`,
`lib/workspace.ml`, commande `protoss capabilities`, assertions capabilities,
scope refs, package negative capabilities et SecretLeakRisk dans
`test/test_protoss.ml`.

- [x] Declarer des capabilities sur `defcap` et `defpolycap`.
- [x] Comparer scope declare et scope infere.
- [x] Propager les scopes de capabilities dans le graphe.
- [x] Exporter refs de capability et signatures request/response.
- [x] Rejeter les changements web qui violent les capabilities attendues.
- [ ] Ajouter `SecretRef scope a` au langage.
- [ ] Sceller les secrets de facon a hasher le handle sans hasher la valeur.
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
...`, assertions imports, packages, lock/interface et drift dans
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
- [ ] Inclure harnesses dans les packages.
- [x] Inclure policies dans les packages.
  Preuves: champ manifest `policies`, entrees `(policies ...)` dans lock et
  package, assertions "project package records policies" dans
  `test/test_protoss.ml` et `README.md`.
- [ ] Ajouter resolver package ecrit en Protoss.
- [ ] Ajouter registre local/global de packages.

## 11. Patches, diff et edition MCP-first

Preuves de section: `lib/patch.ml`, `lib/patch_audit.ml`,
`lib/workspace.ml`, fixtures `patches/*.json`, commandes `protoss patch ...`
et `protoss diff`, assertions patch/diff/audit dans `test/test_protoss.ml`.

- [x] Appliquer des patches JSON atomiques sur store.
- [x] Verifier les patches avant insertion.
- [x] Rollback atomique sur batch invalide.
- [x] Produire un audit de patch content-addressed.
- [x] Chainer les audits par `previous-ref`.
- [x] Verifier que `latest` correspond au hash programme courant.
- [x] Produire `diff`, `diff --json` et `patch from-diff`.
- [x] Diagnostiquer les erreurs de patch avec fichier, operation, kind et source.
- [ ] Implementer un serveur MCP Protoss.
- [ ] Exposer `protoss.query` via MCP.
- [ ] Exposer `protoss.readNode` via MCP.
- [ ] Exposer `protoss.renderView` via MCP.
- [ ] Exposer `protoss.proposePatch` via MCP.
- [ ] Exposer `protoss.checkPatch` via MCP.
- [ ] Exposer `protoss.applyPatch` via MCP.
- [ ] Exposer `protoss.runHarness` via MCP.
- [ ] Exposer `protoss.explain` via MCP.
- [ ] Exposer `protoss.normalize` via MCP.
- [ ] Exposer `protoss.diff` via MCP.
- [ ] Exposer `protoss.rollback` via MCP.
- [ ] Ajouter Patch ADT pour `AddField`, `RemoveField`, `Inline`, `Extract`,
  `AddHarness`, `AddCapability`, `MigrateType`.
- [ ] Convertir diff texte humain en candidat patch structurel.
- [ ] Refuser les modifications textuelles ambigues avec erreur d'intention.
- [ ] Exiger validation de harness avant commit de patch.

## 12. Harness integre

- [ ] Definir la syntaxe `harness name = ...`.
- [ ] Stocker les harnesses dans le graphe canonique.
- [ ] Hasher les harnesses avec `HarnessId = H(canonicalBytes(harness))`.
- [ ] Supporter exemples executables.
- [ ] Supporter tests unitaires.
- [ ] Supporter tests de proprietes.
- [ ] Supporter generateurs de donnees.
- [ ] Supporter benchmarks.
- [ ] Supporter invariants metier.
- [ ] Supporter contrats de migration.
- [ ] Supporter scenarios de monde.
- [ ] Supporter politiques de securite.
- [ ] Supporter prompts de diagnostic.
- [ ] Supporter evaluations IA.
- [ ] Faire echouer les patches qui regressent un harness attache.

## 13. IA comme acteur natif

Preuves de section: commande `protoss duplicates`, `Kernel.def_id`, generation
de vues via `Workspace.store_graph_source_view`, assertions doublons et vues
humaines dans `test/test_protoss.ml`.

- [ ] Definir le protocole agent: `AI -> PatchCandidate -> Validator -> Harness -> Commit`.
- [ ] Fournir une API d'exploration du graphe pour agents.
- [ ] Fournir generation de migrations assistee.
- [ ] Fournir synthese de tests assistee.
- [ ] Fournir explication de definition.
- [x] Detecter doublons semantiques.
- [ ] Factoriser fonctions identiques.
- [ ] Simuler changements dans un `WorldRef` forke.
- [ ] Comparer deux branches par harness.
- [x] Generer des vues humaines lisibles depuis le graphe.
- [ ] Interdire aux agents l'ecriture directe du programme canonique.

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
- [ ] Ecrire le parser Protoss/H complet en Protoss.
- [ ] Ecrire le pretty-printer Protoss/H complet en Protoss.
- [ ] Ecrire le canonicalizer en Protoss.
- [ ] Ecrire le normalizer en Protoss.
- [ ] Ecrire le typechecker noyau en Protoss.
- [ ] Ecrire le patch validator en Protoss.
- [ ] Ecrire le harness runner en Protoss.
- [ ] Ecrire le package resolver en Protoss.
- [ ] Ecrire le serveur MCP en Protoss.
- [ ] Ecrire l'optimizer en Protoss.
- [ ] Ecrire un compiler backend en Protoss.
- [ ] Reduire le TCB aux hashes, format binaire, type verifier noyau,
  validator patch et runtime effets.
- [ ] Ajouter phase de bootstrap documentee 0 -> 5.

## 15. Compilation et backends

Preuves de section: `lib/web.ml`, `examples/web/todo_app`, commandes
`protoss project build ... --target web`, `protoss web build`, assertions web
bundle deterministe dans `test/test_protoss.ml`.

- [x] Interpreter le graphe canonique pour web `view` et `update`.
- [x] Produire bundles web deterministes.
- [x] Inclure `index.html`, `protoss-runtime.js`, `protoss-app.json`,
  `protoss-graph.json`, `protoss-canon-graph.json`, `protoss-host-contract.json`,
  `protoss-capabilities.json`, `protoss-world.json`.
- [ ] Ajouter backend bytecode Protoss VM.
- [ ] Ajouter backend WebAssembly.
- [ ] Ajouter backend LLVM/native.
- [ ] Ajouter backend JavaScript hors runtime web actuel.
- [ ] Ajouter backend SQL/dataflow.
- [ ] Ajouter backend GPU kernels.
- [ ] Definir `CompiledArtifact = derive(UniverseRoot, Target, OptimizationPolicy)`.
- [ ] Verifier determinisme ou equivalence prouvable des artefacts compiles.

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
- [ ] Ajouter architecture `(Model, Cmd caps Msg)` comme alternative officielle.
- [ ] Ajouter migrations UI/harness pour changements de model plus complexes.
- [ ] Ajouter examples humains Protoss/H complets pour apps web.

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
`lib/patch_audit.ml`, assertions capabilities, imports hashes, rendu HTML,
package policies, SecretLeakRisk et negative capabilities dans
`test/test_protoss.ml`.

- [x] Pas d'IO implicite pour les programmes Protoss.
- [x] Capabilities explicites pour effets supportes.
- [x] Patches audites.
- [x] Imports packages verrouilles par hashes.
- [x] Rendu HTML sans injection `innerHTML`.
- [ ] Secrets scelles.
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
- [ ] Audit provenance complet.
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
- [ ] Mapper `Git commit -> UniverseRoot`.
- [ ] Mapper `Git branch -> Universe branch`.
- [ ] Mapper `Git blame -> provenance ledger`.
- [ ] Exporter layout `/protoss.lock`, `/views/**/*.pt`, `/cache/**/*.ptb`,
  `/harness/**/*.pth`.

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
- [ ] Harness examples.

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
- [ ] Store global cross-project.

### v0.3 - MCP-first

- [ ] Serveur MCP Protoss.
- [ ] Query graph.
- [ ] ProposePatch.
- [ ] CheckPatch.
- [ ] ApplyPatch.
- [ ] RenderView.
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

- [ ] Tests de proprietes.
- [ ] Generation de tests par IA.
- [ ] Validation de patches par harness.
- [ ] Comparaison de candidats.
- [ ] Benchmarks content-addressed.

### v1.0 - Self-hosted

- [x] Parser partiel ecrit en Protoss.
- [x] Typechecker report ecrit en Protoss avec noyau OCaml trusted.
- [ ] Canonicalizer ecrit en Protoss.
- [ ] Patch validator ecrit en Protoss.
- [ ] Compiler self-hosted.
- [ ] Remplacement progressif du trusted host.

## 21. Definition de done globale

- [ ] Toutes les cases ci-dessus sont cochees.
- [ ] Chaque case cochee pointe vers test, fixture, doc ou commande probante.
- [ ] Les formats `.pt`, `.ptc`, `.ptb` sont implementes et round-trippes.
- [ ] Le serveur MCP est utilisable par un client MCP standard.
- [ ] Les harnesses sont canoniques, hashes et obligatoires pour patches risqués.
- [x] Le ledger monde supporte branches et merges.
  Preuves: `Ledger.branches`, `Ledger.merge`, `protoss ledger merge` et
  assertions "ledger merged" dans `test/test_protoss.ml`.
- [ ] Les secrets sont scelles et jamais hashes en clair.
- [ ] Le self-hosted path couvre parser, canonicalizer, normalizer, typechecker,
  patch validator, harness runner, package resolver et MCP server.
- [ ] `dune build @fulltest` passe sur une branche propre.
- [ ] Le dernier commit a ete pousse sur `origin/main`.
