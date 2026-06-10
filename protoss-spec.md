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

- [x] Avoir une specification centrale dans le depot.
- [x] Renommer la specification en `protoss-spec.md`, le chemin demande.
- [x] Transformer la specification en TODO exploitable.
- [ ] Ajouter une commande ou un script de verification qui echoue si une case
  marquee done n'a plus de test ou de preuve documentaire.
- [ ] Synchroniser `README.md`, `CLAUDE.md` et ce TODO quand une fonctionnalite
  change de statut.
- [ ] Documenter pour chaque case l'emplacement de son test principal.

## 1. Vision et modele de source de verite

- [x] Representer les programmes comme AST type puis graphe canonique.
- [x] Donner des identites de contenu stables aux definitions (`DefId`).
- [x] Utiliser un contrat de hash declare `sha256` avec prefixe `p2:`.
- [x] Produire un graphe JSON canonique avec `graphHash` auto-excluant.
- [x] Stocker les objets de graphe dans un store de projet content-addressed.
- [x] Auditer les objets de graphe presents dans le store.
- [x] Produire des locks, packages et interfaces deterministes.
- [ ] Definir et implementer un vrai `UniverseRoot = H(packages, defs, types,
  harnesses, policies, worldRefs)`.
- [ ] Faire du `UniverseRoot` la source de verite de toutes les commandes projet.
- [ ] Ajouter un store global partage entre projets pour l'interning des noeuds.
- [ ] Dedupliquer physiquement les noeuds identiques entre projets distincts.
- [x] Exposer une commande de comparaison semantique entre deux roots.
- [ ] Ajouter une provenance native liee aux roots et patches.

## 2. Noms, extensions et formats officiels

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
- [ ] Rejeter toute vue dont le hash diverge du canon.
- [x] Documenter la version du format canonique binaire.

## 3. Syntaxe humaine Protoss/H

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
- [ ] Supporter les modules humains `module X exposing (...)`.
- [ ] Supporter les imports humains `import X exposing (...)`.
- [ ] Supporter les declarations de capabilities en syntaxe humaine.
- [ ] Supporter les effets `Process { cap } A` en syntaxe humaine.
- [ ] Supporter les record updates humains `{ model | x = y }`.
- [ ] Supporter les comparaisons et booleens de surface necessaires aux exemples.
- [ ] Ajouter un formatter Protoss/H idempotent pour toute la grammaire.

## 4. Syntaxe canonique Protoss/C et graphe

- [x] Parser une syntaxe S-expression non ambigue pour le prototype.
- [x] Emettre un `program.canon` deterministe.
- [x] Canonicaliser les aliases avant hashing.
- [x] Canonicaliser les noms de binders pour preserver l'alpha-stabilite.
- [x] Representer les dependances et racines dans le graphe JSON.
- [x] Valider le graphe JSON charge depuis disque.
- [x] Rejeter les champs inconnus dans le JSON canonique.
- [x] Migrer explicitement les graphes vers le format courant.
- [ ] Specifier le format Protoss/C officiel avec variables De Bruijn visibles.
- [x] Emettre et parser Protoss/C comme format public `.ptc`.
- [ ] Interdire tout nom local semantique dans `.ptc`.
- [ ] Trier canoniquement tous les champs dans `.ptc`.
- [x] Ajouter des tests golden `.ptc`.

## 5. Noyau semantique total

- [x] Implementer le lambda-calcul type central.
- [x] Implementer `Var`, `Lam`, `App`, `Let`, `Pi`, records, projections et data.
- [x] Implementer ADT, variants et pattern/case lowering.
- [x] Implementer normalisation deterministe.
- [x] Implementer egalite definitionnelle via formes normales.
- [x] Rejeter la recursion generale non structurelle.
- [x] Supporter `defrec` Nat/List/Variant structurel.
- [x] Supporter `defrecpoly` structurel polymorphe.
- [x] Tester la stabilite alpha et les hashes equivalents.
- [ ] Formaliser le noyau dans la spec avec grammaire executable.
- [ ] Supporter la recursion bien fondee au-dela de Nat/List/Variant directs.
- [ ] Supporter tailles statiques pour terminaison.
- [ ] Supporter coinduction productive.
- [ ] Supporter automates explicitement productifs.
- [ ] Ajouter des tests de preservation/progression approximatifs par fixtures.
- [ ] Ajouter une commande d'explication de terminaison par definition.

## 6. Evaluation, cache et memoisation

- [x] Evaluer les programmes purs via CLI (`eval`, `nf`, `run` selon le cas).
- [x] Interpreter les graphes charges directement sans reparsing texte.
- [x] Charger depuis `--graph` et `--store-graph`.
- [x] Memoiser certains resultats noyau par identite physique.
- [x] Memoiser certains resultats noyau par hash de contenu.
- [ ] Implementer une evaluation lazy call-by-need avec partage explicite.
- [ ] Ajouter des annotations `strict`.
- [ ] Prouver par test qu'un let non force n'evalue pas son RHS.
- [ ] Implementer `EvalKey = H("protoss.eval.v1", DefId, ArgsHash, RuntimePolicy)`
  pour les evaluations pures.
- [x] Ajouter un cache d'evaluation pure persistent.
- [ ] Implementer `EvalKey` avec `WorldRef` et `CapScope` pour processus.
- [x] Ajouter une commande d'inspection des entrees de cache.
- [ ] Partitionner le cache par politique runtime.

## 7. Effets, Process et monde

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
- [ ] Ajouter fork/merge de mondes.
- [ ] Ajouter branches de monde deterministes.
- [ ] Garantir que toute lecture monde passe par evenement explicite.
- [ ] Ajouter API pour reponse externe negative comme evenement typed.
- [ ] Ajouter fixtures pour capteurs ou autres effets extensibles.

## 8. Ledger

- [x] Enregistrer les requetes et reprises dans un ledger.
- [x] Produire `WorldRef` et `EventRef` deterministes.
- [x] Inspecter, rejouer et differ les ledgers.
- [x] Valider les metadonnees de capability et codec a l'inspection.
- [x] Tester les invariants request/resume.
- [ ] Faire du ledger un Merkle-DAG avec branches et merges explicites.
- [ ] Ajouter verification cryptographique optionnelle des evenements signes.
- [ ] Lier le ledger de provenance des patches au ledger monde.
- [ ] Ajouter politiques de retention et garbage collection content-addressed.

## 9. Capabilities et secrets

- [x] Declarer des capabilities sur `defcap` et `defpolycap`.
- [x] Comparer scope declare et scope infere.
- [x] Propager les scopes de capabilities dans le graphe.
- [x] Exporter refs de capability et signatures request/response.
- [x] Rejeter les changements web qui violent les capabilities attendues.
- [ ] Ajouter `SecretRef scope a` au langage.
- [ ] Sceller les secrets de facon a hasher le handle sans hasher la valeur.
- [ ] Partitionner caches et evaluation par `CapScope`.
- [ ] Ajouter policies de package autour des capabilities.
- [ ] Ajouter tests de fuite de secret.
- [x] Exposer une commande d'audit des capabilities par root.

## 10. Modules, imports et packages

- [x] Supporter `(module Name)` et `(export symbol ...)`.
- [x] Qualifier definitions et aliases par module.
- [x] Restreindre les imports aux symboles exportes.
- [x] Construire, locker, packager et verifier des projets `protoss.toml`.
- [x] Ecrire des interfaces publiques content-addressed.
- [x] Verifier les imports locaux par package ref, interface hash et contract hash.
- [x] Rejeter le drift source des packages importes.
- [ ] Supporter imports par hash directement dans la syntaxe source.
- [ ] Supporter alias humain vers hash `package@semver`.
- [ ] Supporter resolution par politique `package@policy`.
- [ ] Inclure harnesses dans les packages.
- [ ] Inclure policies dans les packages.
- [ ] Ajouter resolver package ecrit en Protoss.
- [ ] Ajouter registre local/global de packages.

## 11. Patches, diff et edition MCP-first

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

- [x] Distinguer erreurs de syntaxe, typecheck, patch et audit dans les commandes.
- [x] Ajouter locations `path:line:column` pour erreurs S-expression.
- [x] Localiser des erreurs type communes vers source quand possible.
- [x] Ajouter diagnostics patch operation par operation.
- [x] Ajouter `explain WEB007`.
- [ ] Formaliser la taxonomie `TypeMismatch`, `UnknownReference`,
  `CapabilityDenied`, `NonTerminatingRecursion`, `NonProductiveProcess`,
  `HarnessRegression`, `AmbiguousHumanSyntax`, `UnsafeMigration`,
  `PolicyViolation`, `SecretLeakRisk`.
- [ ] Associer un code stable a chaque erreur publique.
- [ ] Verifier qu'aucune exception OCaml brute ne fuite dans les erreurs CLI.
- [ ] Modeliser les erreurs externes negatives comme evenements ledger.
- [ ] Ajouter helpers `Result` pour erreurs metier dans les examples.

## 18. Securite

- [x] Pas d'IO implicite pour les programmes Protoss.
- [x] Capabilities explicites pour effets supportes.
- [x] Patches audites.
- [x] Imports packages verrouilles par hashes.
- [x] Rendu HTML sans injection `innerHTML`.
- [ ] Secrets scelles.
- [ ] Cache partitionne par capability scope.
- [ ] Policies attachees aux packages.
- [ ] Analyse de risque `SecretLeakRisk`.
- [ ] Audit provenance complet.
- [ ] Tests de negative capabilities par package.

## 19. Diff, review et Git

- [x] Produire diff structurel entre stores.
- [x] Produire patch JSON depuis diff.
- [x] Representer Git comme mecanisme export/push du prototype actuel.
- [ ] Produire diff canonique avec chemins structurels riches.
- [ ] Lister definitions/harnesses affectes par diff.
- [ ] Ajouter vue review humaine pour patches structurels.
- [ ] Mapper `Git commit -> UniverseRoot`.
- [ ] Mapper `Git branch -> Universe branch`.
- [ ] Mapper `Git blame -> provenance ledger`.
- [ ] Exporter layout `/protoss.lock`, `/views/**/*.pt`, `/cache/**/*.ptb`,
  `/harness/**/*.pth`.

## 20. Roadmap d'execution

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
- [ ] Imports par hash en syntaxe source.
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
- [ ] Fork/merge de mondes.

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
- [ ] Le ledger monde supporte branches et merges.
- [ ] Les secrets sont scelles et jamais hashes en clair.
- [ ] Le self-hosted path couvre parser, canonicalizer, normalizer, typechecker,
  patch validator, harness runner, package resolver et MCP server.
- [ ] `dune build @fulltest` passe sur une branche propre.
- [ ] Le dernier commit a ete pousse sur `origin/main`.
