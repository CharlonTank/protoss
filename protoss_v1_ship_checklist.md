# PROTOSS V1.0 — Ship Checklist

Cette checklist est la définition opérationnelle de livraison de **Protoss V1.0**,
alignée sur le noyau réel du dépôt et sur les décisions de design de la section 1.

La règle est stricte : **Protoss V1.0 est SHIPPED uniquement quand chaque case est cochée**,
et **aucune case n'est cochée à la main**. Une case se coche uniquement quand sa preuve
mécanique a été exécutée — à terme, c'est `protoss doctor --v1` qui exécute toutes les
preuves et qui fait foi. Les annotations `Preuve:` désignent la preuve attendue ;
quand l'infrastructure existe déjà dans le dépôt, elle est nommée explicitement.

Chaque item doit correspondre à au moins un des éléments suivants :

- du code mergé ;
- un test automatisé ;
- une commande reproductible ;
- une démo exécutable ;
- un artefact vérifiable ;
- une preuve ou validation mécanique ;
- une documentation opérationnelle testée.

---

## 1. Décisions de design V1.0 (arbitrages tranchés)

Ces décisions sont normatives pour toute la checklist. Les remettre en cause = nouvelle
version de ce document, pas une interprétation locale.

1. **Noyau** : calcul **total et fortement normalisant à types simples pragmatique** —
   lambda/app/let, records, variants (récursion gardée par constructeur), listes,
   polymorphisme prénexe (`Forall`), éliminateurs contrôlés (`foldNat`, `foldList`,
   `foldVariant` + `recur` structurel), coinduction productive (`Stream`, `Automaton`,
   `coiter`). **Pas de Pi-types, pas d'univers, pas d'inductifs généraux en V1.0**
   (différé V2 : rupture de hash assumée et planifiée le jour où).
2. **Primitives** : `Nat`, `Bool`, `String`, `Unit` (+ structures). **`Int`, `Float`,
   `Bytes` différés** — un `Float` canonique exige une politique de déterminisme
   complète qui n'est pas un sous-projet de V1.0.
3. **Stratégie d'évaluation** : la totalité rend la stratégie non observable dans les
   valeurs ; l'identité canonique n'encode pas la stratégie. Implémentation de
   référence **stricte** + annotation `strict` explicite. Le partage à grande échelle
   est fourni par la **mémoïsation content-addressed**, pas par call-by-need.
   Laziness/strictness analysis = optimisation post-V1.0, prouvée équivalente.
4. **Identité** : `DefId` = hash de la **forme canonique** (alpha-normalisation par
   indices De Bruijn, désucrage déterministe, ordonnancement canonique des champs,
   capabilities normalisées). L'**égalité définitionnelle par normalisation** est
   l'outil de comparaison (`protoss nf`, `protoss compare`), pas l'identité primaire —
   les hashes existants restent stables.
5. **Effets** : tout effet est médié — `Process caps a` = `Done` | `Request` +
   continuation typée. Catalogue de requêtes V1.0 : `Human.ask`, `Http.get`,
   `Clock.read`, `Local.storage` (save/load), `Server.request`.
   `fs.*`, `net.post`, `entropy.*` **différés** (chaque requête nouvelle = extension
   du monde répliquable, pas une API ad hoc).
6. **Monde** : ledger **branchable et mergeable**, merge canonique déterministe,
   politiques explicites (`reject` / `manual` / convergence pour états compatibles).
   `WorldRef` entre dans la clé de toute mémoïsation effectful.
7. **Égalité des processus** : canonicité **structurelle** (streams/automates productifs
   canoniques). Minimisation par **bisimulation différée** post-V1.0.
8. **MCP-first** : la modification normale d'un programme est un patch structuré validé
   avant insertion (`patch check` → `patch apply`, chaîne d'audit). Le texte est une
   vue (projection humaine), jamais la source de vérité.
9. **Self-hosting V1.0** : composants frontend réécrits en Protoss et **vérifiés par
   parité contre le kernel** (pattern « kernel-verified candidate » du canonicalizer).
   Le TCB V1.0 est le kernel OCaml, assumé et documenté. Bootstrap binaire
   auto-reproductible **différé**.
10. **Release** : l'identité d'une release est son **hash canonique** (`UniverseRoot`).
    Signatures cryptographiques, clés et canal de publication **différés**.

---

## 2. Définition de “shipped”

- [ ] Une commande unique `protoss doctor --v1` existe et exécute l'intégralité des
  preuves de cette checklist. Preuve: la commande elle-même + son rapport.
- [ ] `protoss doctor --v1` échoue si un seul invariant V1.0 est cassé.
  Preuve: test d'injection de panne (corruption volontaire → doctor échoue).
- [ ] Protoss peut créer un projet vide, ajouter du code, le typer, l'exécuter, le
  tester, le packager et le compiler sans intervention manuelle hors commandes
  documentées. Preuve: script de bout en bout sur `project init|check|build|lock|package`.
- [ ] Protoss peut modifier un projet existant via patch structuré sans édition directe
  de fichiers source. Preuve: démo `patch check`/`patch apply`/`agent commit`.
- [ ] Tous les artefacts V1.0 sont dérivables depuis un `UniverseRoot` canonique.
  Preuve: `universe.root` + rejets d'état périmé (audit/locked build/lock/package).
- [ ] Aucun programme invalide ne peut être commité dans un `UniverseRoot`.
  Preuve: tests de rejet (typage, capabilities, harness) avant insertion.
- [ ] Tous les tests de conformité V1.0 passent depuis un checkout propre.
  Preuve: `dune build @fulltest --force` vert sur clone frais.
- [ ] La release V1.0 est identifiée par hash canonique, pas seulement par nom.
  Preuve: tag de release portant le hash `UniverseRoot` + vérification.

---

## 3. Noyau canonique

### 3.1 Termes

- [ ] Le noyau couvre : `unit`/`true`/`false`/littéraux `Nat`/`String`, variables,
  `lambda`, application, `let`, records + projection, variants + `case`, listes
  (`Nil`/`Cons`/`caseList`), `foldNat`/`foldList`/`foldVariant`/`recur`,
  `Stream`/`coiter`/`streamHead`/`streamTail`/`streamTake`,
  `Automaton`/`automatonRun`, formes View, `Process`/`done`/`bind`/requêtes,
  `inst`/`Forall`. Preuve: `Kernel.executable_grammar_text` + `protoss grammar kernel`
  + suite core.
- [ ] La séparation nœuds sémantiques / métadonnées humaines est stricte : binders,
  commentaires, noms locaux et mise en forme n'entrent pas dans le canonique.
  Preuve: tests d'alpha-stabilité et d'équivalence de syntaxes.
- [ ] Les chemins internes de termes ont une représentation canonique exploitable par
  les diffs et les patches. Preuve: diff structurel + patch ciblé sur sous-terme.

### 3.2 Normalisation et canonicalisation

- [ ] Alpha-normalisation par indices De Bruijn. Preuve: `invariants` (alpha-stability).
- [ ] Désucrage déterministe (sucre ⇒ mêmes nœuds canoniques, jamais de nouveaux
  kinds). Preuve: tests « sucré et désucré hashent identique ».
- [ ] Ordonnancement canonique des champs de records/variants et des capabilities.
  Preuve: suite core (tri canonique).
- [ ] Canonicalisation des imports et des noms locaux (qualification module).
  Preuve: fixtures import/module + équivalence de hash.
- [ ] Normalisation forte du fragment pur, déterministe, forme normale unique.
  Preuve: `protoss nf` + `invariants` (canonicalization, graph round-trip).
- [ ] Deux termes alpha-équivalents produisent le même hash ; deux définitions
  désucrées identiques produisent le même hash. Preuve: tests dédiés existants.
- [ ] Suite de tests de confluence observable et de forme normale unique sur le
  fragment supporté. Preuve: section `invariants` + tests `nf`.

### 3.3 Totalité et productivité

- [ ] La récursion générale est refusée ; seules les éliminations structurelles
  (`defrec` nat/list/variant, `recur` sur sous-termes directs) sont acceptées.
  Preuve: tests de rejet (`recur` non structurel) + acceptation (`sum`-like).
- [ ] Les définitions mutuellement récursives non décroissantes sont refusées.
  Preuve: `reject_cycles` + test.
- [ ] La coinduction est productive uniquement (`coiter`, automates).
  Preuve: tests streams/automates productifs.
- [ ] Erreur structurée et explicable pour une définition non prouvée terminante.
  Preuve: code public stable + `protoss explain`.

---

## 4. Typechecker

- [ ] Typage bidirectionnel complet du noyau (variables, lambdas, applications, let,
  records, projections, variants, cases exhaustifs, folds, listes, streams,
  automates, View, Process). Preuve: suite core + erreurs localisées `path:line:col`.
- [ ] Vérification d'exhaustivité des branches et rejet des branches inconnues ;
  wildcard générant les branches manquantes. Preuve: tests case/foldVariant.
- [ ] Inférence locale (lambdas non annotées en contexte attendu, variants inférés,
  `Nil`/`Cons` inférés, instanciation polymorphe par unification en contexte).
  Preuve: fixtures d'inférence + équivalence de hash avec les formes explicites.
- [ ] Annotations obligatoires aux frontières publiques (defs top-level typées).
  Preuve: grammaire (def exige le type) + tests.
- [ ] Erreurs de type structurées avec catalogue public stable.
  Preuve: `public_error.ml` + `protoss explain --list`.
- [ ] Égalité définitionnelle disponible comme outil : `protoss nf` et comparaison de
  formes normales. Preuve: `protoss compare` + tests.
- [ ] Le typechecker ne dépend pas de l'ordre non canonique du texte source
  (forward references, ordre des defs). Preuve: tests d'ordre + hash stable.

---

## 5. Hash-consing global

### 5.1 Identifiants

- [ ] Domaines d'identité définis, versionnés et documentés : `DefId` (defid-v2),
  hash programme (`protoss-canon-v2`), `HarnessId`, `WorldRef`/`EventRef`,
  `UniverseRoot`, ref d'artefact compilé, id de patch (chaîne d'audit).
  Preuve: doc « ce qui entre dans chaque hash » + tests même contenu ⇒ même id,
  contenu différent ⇒ id différent, métadonnées ⇒ id inchangé.
- [ ] Pas de collision de namespace entre domaines de hash (préfixes explicites).
  Preuve: revue + test de préfixes.

### 5.2 Store canonique

- [ ] Store local content-addressed : écriture atomique, lecture par hash, interning
  global (`PROTOSS_GLOBAL_STORE`), déduplication physique (hardlinks).
  Preuve: `store.ml` + tests store.
- [ ] Vérification d'intégrité du store + détection de corruption.
  Preuve: slice corruption de la suite workspace.
- [ ] GC des nœuds non référencés + pinning des racines. Preuve: `protoss store gc` + tests.
- [ ] Export/import du store par racines (restauration depuis hash racine).
  Preuve: `project export-layout` + test de restauration.

### 5.3 Graphe Merkle

- [ ] `UniverseRoot` référence métadonnées de package, defs, types, harness refs,
  policies, world refs, registres ; tout état périmé est rejeté par audit, locked
  build, lock et package. Preuve: tests `universe-root` existants.
- [ ] Diff structurel entre deux racines/programmes. Preuve: `protoss compare` + diff de patch.
- [ ] Merge structurel : refusé sans politique explicite quand ambigu.
  Preuve: politiques de merge du ledger + tests reject/manual.
- [ ] Commande de vérification de racine. Preuve: `protoss audit` / vérif root + test.

---

## 6. Syntaxes officielles

### 6.1 Protoss/S — S-expressions (source `.pt`/`.protoss`)

- [ ] Grammaire complète parsée et testée (toutes les déclarations et formes du noyau).
  Preuve: parser + suite core.
- [ ] Erreurs de parsing structurées et localisées. Preuve: codes publics + tests.

### 6.2 Protoss/H — syntaxe humaine (Elm-like)

- [ ] Parser H complet sur le périmètre V1.0 (modules, imports, exports, ADT, records,
  alias, fonctions, lambdas, case, if, annotations, capabilities, indentation).
  Preuve: `elm_syntax.ml` + fixtures.
- [ ] Émetteur H (`protoss fmt --human`) : projection AST → texte humain,
  hash-round-trip safe, `Unrenderable` explicite pour les formes sans projection.
  Preuve: tests round-trip + idempotence (plancher de fixtures).
- [ ] `hash(parse(H)) == hash(parse(S))` pour programmes équivalents.
  Preuve: fixtures `elm_like` / `elm_like_equiv`.
- [ ] Étendre H aux formes restantes (defpoly/defcap…) ou documenter le refus.
  Preuve: liste exacte des `Unrenderable` testée.

### 6.3 Protoss/C — canonique textuel (`.ptc`)

- [ ] Format canonique textuel sans ambiguïté, indépendant du whitespace, exposant les
  identifiants canoniques, couvrant tout terme du noyau.
  Preuve: `protoss canon` + golden `examples/basic.ptc` + round-trip.
- [ ] Rendu et re-parse canoniques (`canon --from-graph`, `convert --to ptc`).
  Preuve: `invariants` graph round-trip.

### 6.4 Protoss/B — binaire canonique (`.ptb`)

- [ ] Encodeur/décodeur binaires déterministes, encodage unique par graphe.
  Preuve: `canon --ptb`, `convert --to ptb`, tests de déterminisme byte-à-byte.
- [ ] Représentation canonique des entiers et des textes (UTF-8), ordre canonique des
  collections, refus des encodages non minimaux. Preuve: tests format binaire.

### 6.5 Équivalence des syntaxes

- [ ] `.pt`, `.ptc`, `.ptb` représentent le même graphe :
  `hash(parse(.pt)) == hash(parse(.ptc)) == hash(.ptb)`. Preuve: tests d'équivalence.
- [ ] Le rendu humain, les commentaires et les noms locaux ne modifient jamais le
  `DefId`. Preuve: tests alpha/commentaires/rename.

---

## 7. Évaluation et mémoïsation

### 7.1 Évaluateur de référence

- [ ] Interpréteur de référence strict, total, déterministe, sur le graphe canonique.
  Preuve: `runtime.ml` + suite core + `invariants` (typed Process resume).
- [ ] Annotation `strict` vérifiée et sans effet sémantique (uniquement coût).
  Preuve: tests `strict`.
- [ ] Traces d'évaluation optionnelles sans impact sur le hot path par défaut.
  Preuve: flags opt-in existants + garde perf (CLAUDE.md).

### 7.2 Mémoïsation content-addressed

- [ ] `EvalKey` pur défini : `DefId` + hash canonique des arguments (+ politique
  runtime pertinente). Preuve: doc de la clé + implémentation du cache opt-in.
- [ ] Cache d'évaluation pur : hit sur appel identique, pas de faux hit, invalidation
  par changement de définition. Preuve: tests hit/miss/invalidation.
- [ ] `EvalKey` effectful : inclut `WorldRef` et le scope de capabilities/secrets ;
  pas de fuite inter-capability ou inter-secret par cache.
  Preuve: tests de partitionnement du cache.
- [ ] Outils d'inspection et de vérification du cache.
  Preuve: `protoss cache stats|list` + commande de vérification.

---

## 8. Process, effets et monde

### 8.1 Type `Process`

- [ ] `Process caps a` canonique avec scope de capabilities exact optionnel ;
  `done`, requêtes typées, `bind` (continuation typée). Preuve: noyau + tests.
- [ ] Productivité des processus garantie par construction (pas de boucle d'effets
  non productive exprimable). Preuve: grammaire + tests.
- [ ] Combinateurs de processus fournis par la stdlib (map/andThen via bind).
  Preuve: prélude + tests.

### 8.2 Requêtes

- [ ] Représentation canonique des requêtes du catalogue V1.0 (`AskHuman`, `HttpGet`,
  `ReadClock`, `SaveLocal`, `LoadLocal`, `ServerRequest`), typées requête/réponse.
  Preuve: `req_to_canonical` + types + tests.
- [ ] Toute réponse est vérifiée contre le type attendu de la requête au resume.
  Preuve: tests resume typé (réponse invalide refusée).
- [ ] Une requête sans capability est refusée au check et à l'exécution.
  Preuve: tests `missing capability`.

### 8.3 World ledger

- [ ] Ledger append-only sur `WorldRef`/`EventRef` : pending/resolved requests,
  événements humains/réseau/horloge/stockage, erreurs externes typées.
  Preuve: `ledger.ml` + tests run/resume/replay.
- [ ] Replay déterministe : même `WorldRef` ⇒ mêmes effets, même résultat ;
  monde différent ⇒ `WorldRef` différent. Preuve: `invariants` (ledger events) + tests.
- [ ] Inspection et vérification du monde. Preuve: `protoss runtime world|inspect|audit`.

### 8.4 Branches et merges de monde

- [ ] Fork de monde + merge déterministe ; conflits définis ; politiques `reject`,
  `manual`, convergence sur états compatibles ; merge ambigu refusé sans politique.
  Preuve: branch/merge du ledger + tests mergeable/non-mergeable/approval.

---

## 9. Capabilities et sécurité d'exécution

- [ ] Capabilities canoniques du catalogue V1.0 (`Human.ask`, `Http.get`, `Clock.read`,
  `Local.storage`, `Server.request`), visibles dans les types (`Process caps a`),
  vérifiées au typecheck, à l'exécution et au moment des patches.
  Preuve: catalogue + tests autorisé/refusé + erreur structurée stable.
- [ ] Les policies sont exécutables, pas descriptives (ex. `NoNetworkExceptDeclared`
  exige les capabilities `Http.*`/`Server.*` au manifeste). Preuve: tests policies.
- [ ] `SecretRef scope a` : handles scellés, scope vérifié, jamais de valeur en clair
  dans le graphe, les hashes, les vues ou les logs. Preuve: `secrets.ml` + tests
  non-sérialisation + erreur `SecretLeakRisk`-équivalente.
- [ ] Cache partitionné par capability et par scope de secret (pas de fuite par memo).
  Preuve: tests dédiés (§7.2).
- [ ] Policies de package/module/patch/release dans le `UniverseRoot`, vérifiées
  pendant check/build/patch. Preuve: tests policy + `PolicyViolation`-équivalent.

---

## 10. MCP-first

### 10.1 Serveur MCP

- [ ] Un serveur MCP Protoss expose le contrat agent existant : lecture de racine,
  requête de graphe, lecture/explication de nœud, propose/check/apply/diff/rollback
  de patch, run de harness, check de policy, fork/merge de monde, dérivation
  d'artefact. Preuve: serveur + suite de tests contractuels (succès + erreurs).
- [ ] Le serveur MCP ne peut rien insérer qui contourne `patch check`.
  Preuve: test d'injection refusée.
- [ ] Compatibilité client validée contre au moins un client MCP réel documenté.
  Preuve: test d'intégration scripté.

### 10.2 Patch model

- [ ] Patchs structurés content-addressed couvrant le périmètre V1.0 : ajout de def,
  remplacement de corps, renommage (vue), ajout/retrait de champ via migration,
  factorisation par DefId, capabilities, harness, policies.
  Preuve: `patch.ml` + fixtures `patches/` + tests.
- [ ] Un patch est hashé, rejouable, auditable ; un patch invalide ne modifie pas le
  root ; `patch apply` est atomique et rollbackable (chaîne d'audit
  `previous-root`/`root-ref`). Preuve: `patch_audit.ml` + tests.
- [ ] `agent commit` exige au moins un harness et rejette les rapports en échec avant
  mutation du store. Preuve: tests `agent commit`.

### 10.3 Validation de patch

- [ ] `patch check` vérifie parsing, canonicalisation, typage, totalité/productivité,
  capabilities, secrets, policies, harnesses affectés ; produit un diff structurel
  et l'état futur sans l'appliquer. Preuve: tests de chaque axe de refus.
- [ ] Erreurs structurées stables pour chaque famille de refus de patch.
  Preuve: catalogue public + `protoss explain --list`.

### 10.4 Édition texte comme vue

- [ ] Export de vues texte depuis le graphe (`project export-layout`, `fmt`,
  `fmt --human`) et import d'une édition texte comme candidate de patch.
  Preuve: round-trip vue → patch → graphe.
- [ ] Un diff texte ambigu ou produisant un état invalide est refusé avec explication.
  Preuve: tests rename vs replace-body vs refus.

---

## 11. Harness IA intégré

- [ ] `Harness` canonique (`.pth`, `HarnessId`, bytes canoniques), référencé par
  l'`UniverseRoot`, attachable aux defs/modules/applications.
  Preuve: `harness.ml` + `store/harness.graph.json` + tests.
- [ ] Exécution des harnesses (`protoss harness run`) + détection des harnesses
  affectés par un patch. Preuve: runner JSON + tests `harness affected`-équivalents.
- [ ] Types de tests V1.0 : exemples, tests unitaires, property tests avec générateurs
  typés, invariants, tests de migration, replay ledger, benchmarks, snapshots UI
  canoniques. Preuve: harness suite + contre-exemples minimisés quand applicable.
- [ ] IA = agent de proposition, jamais autorité : toute sortie IA passe par
  `patch check` + harness avant acceptation ; propositions journalisées.
  Preuve: `agent_protocol.ml` (canonical-write guard, commit wrapper) + tests.
- [ ] Obligation de harness : un patch comportemental public sans harness mis à jour
  est refusé avec erreur structurée. Preuve: test de refus dédié.

---

## 12. Packages et dépendances

- [ ] Package canonique : manifeste (`protoss.toml`), exports/imports, harnesses,
  policies, docs ; restaurable depuis son root. Preuve: `workspace.ml` + tests package.
- [ ] Résolution par hash exact ; alias humains (`package_aliases`,
  `package_policy_aliases`) validés puis résolus vers le hash locké ; vue semver
  jamais identité réelle. Preuve: tests aliases + lock.
- [ ] Lock canonique dans le root ; dépendances non hashées refusées en mode strict.
  Preuve: `project lock --check` + tests.
- [ ] Registres déclarés au manifeste (`package_registry_local|global`),
  local > global, refs de registre dans `UniverseRoot`, lock et descripteurs.
  Preuve: tests registres existants.
- [ ] Publication/installation par root de package avec vérification d'intégrité
  (registre local + export/import ; registre distant différé).
  Preuve: scénario scripté publish/install local.

---

## 13. Standard library V1.0

- [ ] Core : `Bool`, `Nat`, `String`, `Unit`, `Maybe`, `Result`, `List`, `Pair`,
  `Assoc`, `Map`, `Set`, `Order`(comparaisons), helpers `String`/`Nat`.
  Preuve: prélude + tests stdlib.
- [ ] Process : wrappers des requêtes du catalogue (`Human`, `Http`, `Clock`,
  `Local`, `Server`). Preuve: prélude + fixtures.
- [ ] Application : modules UI minimal (View), JSON (`Json`), sérialisation
  canonique, validation. Preuve: prélude (`json.ml` exposé) + todo app.
- [ ] Self-host : `Sexp`/`Json`/`Protoss` parsers, typechecker report, canonicalizer
  écrits en Protoss dans le prélude. Preuve: section self-host de la suite.
- [ ] Harnesses pour chaque module stdlib exposé. Preuve: couverture harness stdlib.

---

## 14. Applications full-stack

- [ ] Modèle UI pur : `Model`/`Msg`/`update`/`view`, événements typés, effets via
  `Cmd`/`Process`. Preuve: noyau View + exemples web.
- [ ] Build web déterministe : `index.html`, runtime JS, graphe canonique embarqué,
  capabilities + host contract en JSON ; ref d'artefact dérivée uniquement de
  (`UniverseRoot`, target, optimization policy). Preuve: `web.ml` + tests web.
- [ ] Backend : endpoints typés via `Server.request`, validation d'entrée, policies.
  Preuve: tests runtime/server + refus par policy.
- [ ] Stockage : schémas comme types Protoss, lecture/écriture typées via
  `Local.storage`, état rejouable depuis le ledger, migrations vérifiées.
  Preuve: tests storage + replay.
- [ ] **Démo obligatoire** : la todo app full-stack (UI + backend + storage + ledger
  + harness complet) est modifiée via patch structuré pour ajouter `priority`
  (type, champ, migration, UI, harness), le patch passe `patch check`, est
  appliqué, l'app recompilée fonctionne, le replay ledger donne le même résultat,
  et la démo tourne depuis un checkout propre en une commande documentée.
  Preuve: scénario scripté de bout en bout.

---

## 15. Compilation et artefacts

- [ ] Pipeline : artefact = f(`UniverseRoot`, target, optimization policy), jamais
  source de vérité, vérifiable et explicable.
  Preuve: `protoss-compiled-artifact.txt` + commandes build/verify.
- [ ] Backends V1.0 : interpréteur de référence (autorité) + target web (UI) +
  **une VM bytecode réelle** (format d'instructions content-addressed déterministe,
  compilation du graphe canonique, exécuteur avec parité testée contre
  l'interpréteur sur les fixtures). Les autres backends (wasm/llvm/sql/gpu)
  restent des manifestes explicitement non cochés. Preuve: VM + tests de parité.
- [ ] Les backends ne découvrent jamais d'erreurs de typage (le graphe inséré est déjà
  bien formé) ; seuls erreurs de cible, limites de ressources ou policy denial.
  Preuve: tests de classification d'erreurs backend.
- [ ] Optimisations V1.0 : inlining sûr et élimination de code mort, chacune prouvée
  préservant la sémantique (parité avant/après). Preuve: tests de non-régression.
- [ ] Déterminisme d'artefact : deux builds du même root produisent les mêmes bytes.
  Preuve: test de rebuild byte-identique.

---

## 16. Diff, review et provenance

- [ ] Diff structurel entre définitions, programmes, roots et mondes, rendu en vue
  canonique et en vue humaine, par chemins canoniques stables.
  Preuve: `protoss compare` + diff de patch + tests de stabilité.
- [ ] Rapport d'impact par patch : defs affectées, harnesses affectés, capabilities et
  policies modifiées. Preuve: `patch review` + tests.
- [ ] Approval humain supporté quand une policy l'exige ; rejet avec raison structurée.
  Preuve: tests policy d'approval.
- [ ] Provenance : auteur de proposition, vérifications exécutées, roots avant/après,
  records natifs `store/provenance` ; mapping git (`git map`, `git blame` ledger).
  Preuve: tests provenance + déterminisme des fichiers générés.

---

## 17. Self-hosting

### 17.1 Composants écrits en Protoss (pattern « kernel-verified candidate »)

Chaque composant suit le contrat établi : le kernel vérifie d'abord et fournit les
identités ; le composant Protoss émet un candidat ; une commande `--compare` échoue
fort sur divergence ; toute forme hors périmètre = erreur explicite, jamais de
sortie non vérifiée ; sweep de parité avec plancher anti-test-creux.

- [ ] Parser S-expr + parser de déclarations écrits en Protoss.
  Preuve: `Sexp.parseText`/`Protoss.parseText` + parité parse.
- [ ] Formatter écrit en Protoss. Preuve: `Protoss.formatText` + idempotence.
- [ ] Typechecker report écrit en Protoss avec parité de verdict.
  Preuve: `protoss self compare-typecheck` + sweep.
- [ ] **Canonicalizer écrit en Protoss avec parité byte-à-byte.**
  Preuve: `Protoss.canonProgramText`, `protoss self canon --compare`,
  sweep `__canon_parity_*` + golden `examples/basic.ptc` + plancher.
- [ ] Patch validator écrit en Protoss avec parité de verdict contre `Patch.check`.
  Preuve: sweep sur `patches/*.json` + `--compare`.
- [ ] Normalizer écrit en Protoss avec parité contre `protoss nf` sur le fragment
  supporté. Preuve: sweep de parité + plancher.
- [ ] Harness runner et package resolver : périmètre Protoss réel défini, vérifié par
  parité sur leur fragment, ou explicitement listés non self-hosted V1.0.
  Preuve: parité ou liste d'exclusion documentée.
- [ ] Le TCB V1.0 (kernel OCaml : hashes, format binaire, vérification de types,
  exécution des effets) est nommé, documenté et minimal.
  Preuve: `docs/self-hosting.md` + audit de surface.

### 17.2 Différé explicite (post-V1.0)

- Compilation de Protoss par Protoss et self-build binaire reproductible.
- MCP server, optimizer, compiler backend écrits en Protoss.

---

## 18. CLI et UX développeur

- [ ] CLI complète V1.0 : `init`, `check`, `nf`, `hash`, `canon`, `compare`, `fmt`
  (+`--human`), `run`/`runtime`, `harness run`, `patch check|apply|audit|review`,
  `project init|check|build|lock|package|export-layout`, `store *`, `bench`,
  `spec check`, `doctor --v1`. Preuve: usage + tests CLI.
- [ ] Chaque commande a des erreurs structurées (catalogue public) ; mode JSON là où
  un agent consomme la sortie. Preuve: `public_error.ml` + `explain --list` + tests.
- [ ] Diagnostics : expliquer une erreur de type, de terminaison, de capability, de
  policy, de harness, de patch refusé ; expliquer pourquoi deux définitions ont ou
  non le même `DefId`. Preuve: `protoss explain` + `duplicates` + tests.
- [ ] Éditeur V1.0 : coloration + navigation existantes (`editors/`), rendu de vue
  humaine depuis le graphe. LSP complet différé. Preuve: extension + commande de vue.

---

## 19. Conformance suite

- [ ] La suite officielle couvre : parsers (S/H), formats (`.ptc`/`.ptb`), hashing,
  store, typechecker, normalizer, évaluateur, cache, process, ledger, capabilities,
  policies, patches, harness, packages, backends, self-hosting, todo app.
  Preuve: `dune build @fulltest` + `invariants` + conformance goldens.
- [ ] Golden projects V1.0 : hello-world, pure-library, process-clock, human-ask,
  todo-fullstack, migration-demo, capability-denied-demo, patch-demo,
  self-host-demo. Chacun passe un check doctor par projet.
  Preuve: `examples/` complétés + commande de validation par projet.

---

## 20. Performance minimale V1.0

- [ ] Benchmarks officiels content-addressed : parsing, canonicalisation, typecheck,
  normalisation, hash, cache hit/miss, `patch check`, `harness run`, build web.
  Preuve: `protoss bench` + rapports persistés.
- [ ] Seuils minimaux définis ; `doctor --v1` échoue si un seuil critique est raté.
  Preuve: seuils dans le doctor + test.
- [ ] Le dev loop reste quasi instantané (invariant projet) : aucun bookkeeping dans
  le hot path de l'évaluateur ; rebuild incrémental sub-seconde.
  Preuve: garde perf + `PROTOSS_PERF_STATS`.
- [ ] Résultats de benchmark publiés avec la release. Preuve: artefact de release.

---

## 21. Sécurité release

- [ ] Audit du TCB déclaré (surface kernel OCaml), du hashing, de la séparation
  secrets/hash, du cache par capability, du patch validator.
  Preuve: rapport d'audit versionné + tests associés.
- [ ] Fuzzing parser (S et H) et décodeur binaire ; fuzzing du format de patch.
  Preuve: harness de fuzz + corpus + crash = test de régression.
- [ ] Tests de corruption store, de replay ledger malveillant, de package altéré
  (root ≠ contenu refusé). Preuve: tests dédiés.
- [ ] Différé explicite : signatures cryptographiques, sandbox OS, mode no-network
  au niveau OS (les capabilities couvrent le niveau langage).

---

## 22. Documentation opérationnelle

- [ ] Installer Protoss ; créer un projet ; écrire Protoss/S et Protoss/H ; lire
  Protoss/C ; format des patches ; commandes CLI ; modèle de capabilities ; modèle
  de ledger ; modèle de harness ; packaging ; self-hosting ; erreurs structurées ;
  la todo app ; vérifier une release V1.0.
  Preuve: chaque claim technique de la doc est couvert par un test ou une commande
  (vérification mécanique type `spec check`).

---

## 23. Gate final V1.0

Protoss V1.0 est livré uniquement quand ces items passent dans l'ordre, depuis un
checkout propre :

- [ ] `protoss doctor --v1` passe intégralement.
- [ ] La conformance suite passe (`@fulltest` + `invariants`).
- [ ] Les golden projects passent.
- [ ] La todo app full-stack fonctionne.
- [ ] Un patch structuré réel (`priority`) est vérifié, appliqué, recompilé ;
  les harnesses affectés passent ; le nouveau `UniverseRoot` est valide.
- [ ] Le ledger rejoue le même résultat.
- [ ] Le cache ne fuit pas entre capabilities ni entre scopes de secrets.
- [ ] Les packages sont restaurables par hash.
- [ ] Les composants self-hosted passent leurs parités kernel.
- [ ] La release est identifiée par son hash canonique ; le rapport `doctor --v1`,
  la conformance suite, les golden projects et les checksums sont publiés.

---

## Définition finale

- [ ] **Protoss V1.0 est SHIPPED quand `protoss doctor --v1` passe, qu'une application
  full-stack réelle tourne, qu'elle peut être modifiée par patch structuré validé
  avant insertion, que tous les harnesses passent, que le graphe reste canonique,
  que les effets rejouent via ledger branchable, que les dépendances sont hashées,
  que les secrets ne fuient pas, que les composants self-hosted tiennent leur
  parité kernel, et qu'aucun état invalide ne peut entrer dans un `UniverseRoot`.**

---

## Annexe — Différés explicites (post-V1.0, avec raison)

| Différé | Raison |
|---|---|
| Pi-types, univers, inductifs généraux | rupture du noyau et de tous les hashes ; V2 planifiée, pas un incrément |
| `Int`, `Float`, `Bytes` | politique canonique (overflow, arrondi, NaN) = chantier de déterminisme entier |
| Runtime lazy call-by-need | non observable sémantiquement (totalité) ; la memo globale fournit le partage ; coût/complexité runtime |
| Bisimulation / minimisation des process | fragment décidable à concevoir ; canonicité structurelle suffit pour V1.0 |
| Requêtes `fs.*`, `net.post`, `entropy.*` | chaque requête étend le monde répliquable ; catalogue V1.0 suffisant pour la démo full-stack |
| Signatures cryptographiques, clés de release | infrastructure de clés et canal de publication non décidés ; le hash canonique identifie la release |
| Registre distant | hébergement non décidé ; local + export/import par hash couvre V1.0 |
| LSP complet | l'extension éditeur + vues couvrent V1.0 |
| Self-build (Protoss compile Protoss), MCP/optimizer/backend en Protoss | dépend de la VM bytecode et de la chaîne self-hosted complète ; pattern de parité établi d'abord |
| CRDT généraux pour merges de monde | politiques reject/manual/convergence couvrent V1.0 ; CRDT riches ensuite |
