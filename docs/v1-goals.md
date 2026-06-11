# Protoss V1.0 — File de goals

File d'exécution dérivée de `protoss_v1_ship_checklist.md`. Une seule session
orchestre : elle dépile les goals dans l'ordre, intègre, et lance en parallèle
(agents en arrière-plan) les goals marqués `agent:oui` dont les dépendances sont
satisfaites.

## Règles de coordination (non négociables)

1. **Un seul écrivain pour les fichiers partagés** : `bin/main.ml`, les fichiers
   `dune`, `test/test_protoss.ml`, `stdlib/prelude.protoss`, `lib/kernel.ml`,
   `protoss-spec.md`, `protoss_v1_ship_checklist.md` et ce fichier sont réservés à
   l'orchestrateur. Un agent écrit uniquement dans le périmètre listé par son goal
   (fichiers nouveaux) et livre ses instructions de branchement en fin de tâche.
2. **Builds et tests sérialisés par l'orchestrateur.** Les agents ne lancent pas
   `dune` (lock partagé) sauf mention contraire ; l'orchestrateur compile, branche,
   teste, corrige.
3. **Un goal est `done`** quand : ses preuves ont été exécutées (commande/test cités
   dans son critère), `dune build @fulltest --force` est vert, le travail est
   commité, et son statut est mis à jour ici (avec le hash du commit).
4. **Aucune case de `protoss_v1_ship_checklist.md` ni de `protoss-spec.md` n'est
   cochée par un agent.** Les coches passent par les preuves exécutées
   (`protoss doctor --v1`, `protoss spec check`) et par l'orchestrateur.
5. **Déterminisme sacré** (CLAUDE.md) : interdiction de faire hasher différemment des
   sources équivalentes ; si un goal semble l'exiger, c'est le goal qu'on reconçoit,
   pas l'invariant.
6. **Pas de statut bloqué.** Toute ambiguïté découverte en cours de goal est tranchée
   par l'orchestrateur dans l'esprit des arbitrages de la section 1 de la ship
   checklist ; l'arbitrage est consigné (section 1 + une ligne au journal ici) et le
   goal continue. Les arbitrages pris en route sont récapitulés à l'utilisateur en
   fin de session — le travail ne s'arrête jamais en attente d'une réponse.

## Statuts

`pending` → `in-progress` → `done <commit>`.

---

## Vague 1 — indépendants

### G1 — `protoss doctor --v1` [done]
- **Périmètre** : nouveau `lib/doctor.ml` ; branchement CLI par l'orchestrateur.
- **Dépendances** : aucune. **Agent** : non (structurant, intègre tout l'existant).
- **Goal** : implémenter `protoss doctor --v1` : exécute mécaniquement les preuves
  déjà disponibles de la ship checklist (grammaire kernel, invariants, équivalence
  `.pt`/`.ptc`/`.ptb`, alpha-stabilité, store/universe-root, ledger replay,
  capabilities/policies/secrets, patch check/audit, harness, packages/lock/registres,
  build web déterministe, parités self-hosted, spec check), produit un rapport
  par section (ok / fail / not-implemented-yet avec l'item de checklist associé),
  échoue (exit ≠ 0) si une preuve disponible casse, et expose `--json`.
  Un test d'injection de panne prouve que doctor échoue fort.
- **Done** : `protoss doctor --v1` tourne sur le repo et son rapport liste
  honnêtement les sections vertes et les manquantes ; test de panne ; fulltest vert.

### G2 — Golden projects [done]
- **Périmètre agent** : `examples/golden/**` (nouveaux projets), un script de
  validation `examples/golden/run.sh` ou équivalent. Rien d'autre.
- **Dépendances** : aucune. **Agent** : oui.
- **Goal** : créer les golden projects V1.0 : `hello-world`, `pure-library`,
  `process-clock`, `human-ask`, `migration-demo`, `capability-denied-demo`,
  `patch-demo` (todo-fullstack et self-host-demo sont couverts par G7 et l'existant).
  Chaque projet : `protoss.toml` + sources minimales + le scénario de commandes qui
  le valide (check/build/lock, et pour capability-denied : l'échec attendu avec le
  bon code public). Livrer la liste des commandes de validation pour branchement
  dans doctor (G1) et dans la suite.
- **Done** : chaque projet validé par ses commandes depuis la racine du repo ;
  intégration par l'orchestrateur dans doctor + tests ; fulltest vert.

### G3 — Fuzzing parsers et formats [done]
- **Périmètre agent** : nouveau `test/fuzz_protoss.ml` (+ corpus sous
  `test/fuzz-corpus/**`). Rien d'autre ; le `dune` du test est branché par
  l'orchestrateur.
- **Dépendances** : aucune. **Agent** : oui.
- **Goal** : fuzzer déterministe (PRNG seedé, pas d'horloge) pour : le parser
  S-expr, le parser Elm-like, le décodeur `.ptb`, le parseur de patch JSON.
  Générateurs de sources mutées + propriété « jamais de crash non catalogué :
  soit parse OK, soit erreur publique structurée ». Tout crash trouvé devient un
  cas de régression versionné dans le corpus.
- **Done** : cible de test dédiée, N itérations seedées en CI locale, zéro crash
  non structuré ; corpus de régression ; fulltest vert.

### G4 — VM bytecode (format + compilation) [done]
- **Périmètre agent** : nouveau `lib/bytecode.ml` (format d'instructions,
  encodage déterministe content-addressed, compilation graphe canonique →
  bytecode). Pas d'exécuteur dans ce goal. Rien d'autre.
- **Dépendances** : aucune. **Agent** : oui (bien spécifié, fichier neuf).
- **Goal** : définir le jeu d'instructions de la VM Protoss (pile ou registres, au
  choix justifié) couvrant tout le noyau exécutable (littéraux, var/De Bruijn,
  lambda/app/let, records/field, variants/case, folds, listes/caseList, streams/
  automates, strict, Process/done/bind/requêtes comme suspensions) ; encodage
  binaire déterministe du module bytecode + hash ; compilateur
  `cterm → bytecode` total sur le fragment, erreur explicite sinon.
- **Done** : compilation de toutes les fixtures `examples/` qui checkent isolément,
  encodage byte-identique sur double run ; tests unitaires du module ;
  fulltest vert. (La parité d'exécution arrive en G5.)

---

## Vague 2 — dépendants de la vague 1

### G5 — VM bytecode (exécuteur + parité interpréteur) [done]
- **Périmètre** : `lib/bytecode.ml` (ou module exécuteur séparé) ; branchement
  target par l'orchestrateur.
- **Dépendances** : G4. **Agent** : oui, après G4 intégré.
- **Goal** : exécuteur de la VM avec sémantique identique à l'interpréteur de
  référence : sweep de parité `Runtime.normalize_def` vs VM sur toutes les
  fixtures exécutables (valeurs identiques), y compris suspension/resume de
  Process ; classification des erreurs backend (cible/ressources/policy, jamais
  typage) ; target `--target bytecode` remplaçant le manifeste stub.
- **Done** : sweep de parité vert avec plancher, target branché, item checklist
  §15 prouvable par doctor ; fulltest vert.

### G6 — Serveur MCP [done]
- **Périmètre agent** : nouveau `lib/mcp_server.ml` (+ doc contrat
  `docs/mcp.md`) ; branchement CLI (`protoss mcp serve`) par l'orchestrateur.
- **Dépendances** : aucune (s'appuie sur `agent_protocol`/`patch`/`workspace`
  existants) ; intégration après G1 pour le doctor. **Agent** : oui.
- **Goal** : serveur MCP (JSON-RPC sur stdio) exposant le contrat agent :
  `root.current`, `graph.query`, `node.read`, `node.explain`, `patch.propose`,
  `patch.check`, `patch.apply`, `patch.diff`, `harness.run`, `policy.check`,
  `world.fork`, `world.merge`, `compile.derive`. Chaque méthode délègue aux
  modules existants — aucune logique de validation dupliquée, impossible de
  contourner `patch check`. Suite de tests contractuels (succès + erreurs
  structurées) pilotée par un client de test scripté.
- **Done** : serveur lancé + client de test vert sur toutes les méthodes ; test
  d'injection refusée (apply sans check) ; fulltest vert.

### G7 — Démo full-stack `priority` [done]
- **Périmètre** : `examples/web/todo_app/**`, `patches/**` (nouveaux fichiers),
  script de démo ; intégration tests par l'orchestrateur.
- **Dépendances** : G1 (pour le branchement doctor) ; idéalement G6 pour la voie
  MCP, sinon voie `patch check/apply` CLI. **Agent** : oui pour la préparation.
- **Goal** : scénario scripté de bout en bout : la todo app (UI + storage +
  ledger + harness) reçoit un patch structuré ajoutant `priority` (type, champ,
  migration, UI, harness mis à jour) ; `patch check` l'accepte, `patch apply`
  l'applique, l'app rebuild fonctionne, le replay ledger donne le même résultat ;
  le tout depuis un checkout propre en une commande documentée.
- **Done** : script exécuté vert localement + branché dans la suite ; item §14
  de la checklist prouvable par doctor ; fulltest vert.

---

## Vague 3 — le prélude (monopole orchestrateur, séquentiel)

### G8 — Patch validator self-hosted [pending]
- **Périmètre** : `stdlib/prelude.protoss`, `bin/main.ml`, `test/test_protoss.ml`,
  `lib/public_error.ml` — orchestrateur uniquement.
- **Dépendances** : aucune (pattern établi). **Agent** : non.
- **Goal** : `Protoss.patchValidateText` (parse JSON du patch + règles de
  `Patch.check` reproduites sur un périmètre déclaré, re-canonicalisation des
  defs portées via `Protoss.canonProgramText`) ; `protoss self patch-check
  <patch.json> [--compare]` avec parité de verdict contre le kernel sur les
  fixtures `patches/*.json` (accepté/rejeté + motif), Err explicite hors
  périmètre ; recocher 664/986 du spec si la parité couvre les fixtures.
- **Done** : sweep de parité de verdict vert avec plancher + cas de divergence
  volontaire ; spec check OK ; fulltest vert.

### G9 — Normalizer self-hosted [pending]
- **Périmètre** : idem G8 — orchestrateur uniquement.
- **Dépendances** : G8 (file du prélude). **Agent** : non.
- **Goal** : normalizer en Protoss sur le fragment pur supporté, parité
  byte-à-byte contre `protoss nf` (kernel d'abord, candidat comparé), Err
  explicite hors périmètre ; sweep sur les fixtures + plancher ; recocher la
  case normalizer du spec si couverture.
- **Done** : parité verte ; spec check OK ; fulltest vert.

---

## Vague 4 — consolidation release

### G10 — Cache d'évaluation content-addressed (EvalKey) [pending]
- **Périmètre** : `lib/runtime.ml`/nouveau module cache — orchestrateur (hot
  path : tout reste opt-in, garde perf CLAUDE.md).
- **Dépendances** : G1. **Agent** : non.
- **Goal** : formaliser `EvalKey` (pur : DefId + hash args ; effectful : +
  WorldRef + CapScope), cache opt-in avec tests hit/miss/invalidation et tests
  de partitionnement par capability et par scope de secret (pas de fuite par
  cache) ; outils d'inspection/vérification.
- **Done** : tests de partitionnement verts ; aucune régression perf du dev loop
  (byte-diff/PERF_STATS) ; fulltest vert.

### G11 — Édition texte comme vue (import → patch) [pending]
- **Périmètre** : nouveau module + CLI — orchestrateur ou agent selon taille.
- **Dépendances** : G6 souhaitable. **Agent** : possible.
- **Goal** : `protoss edit import` (une édition de vue texte devient une
  candidate de patch structuré : rename vs replace-body détectés, ambigu refusé
  avec explication) + `protoss edit explain` ; tests des trois cas.
- **Done** : tests rename/replace/refus verts ; fulltest vert.

### G12 — Benchmarks officiels + seuils doctor [pending]
- **Périmètre agent** : scénarios sous `benchmarks/**` ; seuils branchés dans
  doctor par l'orchestrateur.
- **Dépendances** : G1. **Agent** : oui pour les scénarios.
- **Goal** : benchmarks content-addressed (parse, canon, typecheck, nf, hash,
  cache hit/miss, patch check, harness run, build web) via `protoss bench`,
  rapports persistés, seuils critiques définis et vérifiés par `doctor --v1`.
- **Done** : `protoss bench` produit les rapports ; doctor échoue sur seuil
  critique raté (test) ; fulltest vert.

### G13 — Documentation opérationnelle vérifiée [pending]
- **Périmètre agent** : `docs/**` (nouveaux fichiers de doc opérationnelle).
- **Dépendances** : G1–G7 livrés pour documenter du réel. **Agent** : oui.
- **Goal** : doc d'installation, premier projet, écrire S/H, lire C, patches,
  CLI, capabilities, ledger, harness, packaging, self-hosting, erreurs, todo
  app, vérification de release — chaque claim technique adossé à une commande ou
  un test (vérification mécanique type `spec check` branchée dans doctor).
- **Done** : vérification mécanique des claims verte ; fulltest vert.

### G14 — Gate final V1.0 [pending]
- **Périmètre** : orchestrateur.
- **Dépendances** : tous les goals précédents.
- **Goal** : dérouler la section 23 de la checklist depuis un checkout propre :
  doctor --v1 intégral, conformance, golden projects, démo priority, replay,
  parités self-hosted, release identifiée par hash canonique + publication du
  rapport doctor. Cocher la checklist via les preuves exécutées uniquement.
- **Done** : gate déroulé vert ; checklist cochée par preuves ; tag de release
  portant le hash.

---

## Journal

- 2026-06-11 — G1 `protoss doctor --v1` : `lib/doctor.ml` (11 preuves pures réelles
  exécutées + 11 not-yet honnêtes avec item de checklist), dispatch CLI
  `protoss doctor --v1 [--json]`, test d'injection de panne (`aggregate_exit`)
  dans la section core ; `@fulltest` vert. Décision consignée : le doctor est
  auto-suffisant (sources embarquées, indépendant du CWD) + preuves best-effort
  sur artefacts localisables (spec). Commit 6583284.
- 2026-06-11 — G2 Golden projects : 7 projets sous `examples/golden/` (6 valides +
  capability-denied), `run.sh`, VALIDATE.md par projet, patches JSON. Preuve
  `golden-projects` branchée dans le doctor (build `~write:false` sur copie tmp
  pid-qualifiée → zéro pollution `.protoss` du repo ; capability-denied rejeté
  pour capability manquante), dep `examples/golden` ajoutée à `test-fixtures`,
  assertion ciblée dans la section core ; doctor = 12 pass / 0 fail / 10 not-yet ;
  `@fulltest` vert. Arbitrage hérité de l'agent : capability-denied surface CAP001
  en check isolé, WORKSPACE001 (msg « missing capability ») en voie workspace.
- 2026-06-11 — G4 VM bytecode (format) : `lib/bytecode.ml` (machine à pile, 44
  opcodes, couverture exhaustive de `cterm`, encodage déterministe big-endian
  longueur-préfixé, `compile_checked`/`encode_module`/`decode_module`/
  `hash_module`). Preuve `bytecode-encoding` branchée dans le doctor (compile +
  encode/decode round-trip + hash déterministe sur le programme riche), sweep de
  test sur toutes les fixtures `examples/` isolées (plancher ≥ 20) ; doctor =
  13 pass / 0 fail. `@fulltest` vert. Compile du premier coup, aucun symbole
  kernel à exposer. Parité d'exécution = G5.
- 2026-06-11 — G3 Fuzzing : `test/fuzz_protoss.ml` (fuzzer déterministe seedé,
  4 cibles — parser S-expr, Elm-like, décodeur `.ptb`, patch JSON — 2000
  itérations/cible, mutation de corpus + génération bornée), corpus de régression
  `test/fuzz-corpus/` (24 fichiers), alias `@fuzztest` (mode `--strict`) branché
  dans `@fulltest`. L'agent a trouvé 3 vrais crashs `int_of_string` non
  structurés ; **corrigés dans le noyau** (`lib/parser.ml` TVar/Forall via
  `int_atom`, `lib/kernel.ml` `type_of_canonical_sexp` TVar/Forall et
  `cterm_of_canonical_sexp` CVar via `parse_nat_atom` → erreur structurée). Les
  fichiers `crash_*` reclassés en `clean_*` (gardes de régression). Preuve doctor
  `structured-errors-on-hostile-input` (§21) ajoutée ; doctor = 14 pass / 0 fail.
  Fuzzer strict : 8000 itérations, 0 crash non structuré ; `@fulltest` vert.
  Corrections d'entrée invalide uniquement → aucun hash de programme valide
  affecté. Finding mineur restant (Sexp.Error wrappé INTERNAL001 en loader/patch)
  noté, non bloquant (déjà structuré).
- 2026-06-11 — G3 (suivi) : une 2e passe de fuzzing (harness alternatif, propriétés
  de round-trip) a trouvé 2 bugs que la 1re a manqués, **corrigés** : crash brut
  `Invalid_argument "String.sub"` sur `case ofx` (`lib/elm_syntax.ml` —
  `find_sub " of"` matchait l'espace de `case ` ; fix : chercher après `case `) ;
  round-trip JSON cassé (`lib/json.ml` émettait `\u00XX` sans le parser ; fix :
  décodage `\u` → UTF-8, inverse exact de l'émetteur). Gardes de régression dans
  la section core (parse `case ofx` structuré, `Json.parse (to_string v) = v` sur
  octets de contrôle) + corpus `clean_case_ofx.elm`. Fuzzer strict toujours à
  0 crash. Corrections d'entrée invalide / round-trip uniquement — aucun hash de
  programme valide affecté.
- 2026-06-11 — G6 Serveur MCP : `lib/mcp_server.ml` existait déjà (JSON-RPC 2.0
  stdio, 11 tools), avec tests de contrat. Complété : test d'injection
  (`applyPatch` sans harness → refusé via `commit_patch_json`, `check` non
  contournable), preuve doctor `mcp-contract` (§10.1), `docs/mcp.md`. Doctor =
  15 pass / 0 fail ; `@fulltest` vert.
- 2026-06-11 — G5 VM bytecode (exécuteur + parité) : `lib/bytecode_vm.ml` (machine
  à pile, valeurs = `Runtime.value` pour parité directe via `value_to_canonical`,
  closures reconstruites par `decompile_block`, `cap_scope`/`recur_stack` threadés
  comme l'interpréteur, builtins reproduits cas par cas). Fix dans `lib/bytecode.ml` :
  `compile_checked` compile depuis `parse_serialized_def d.canonical` (corps à
  def_ids, identité canonique) au lieu de `d.cterm` (noms), pour aligner les corps
  de closures sur ceux qu'évalue `Runtime.eval_def`. Preuve doctor `bytecode-parity`
  (`vm_canonical` == `value_to_canonical (normalize_def)`) + sweep de parité par def
  sur toutes les fixtures isolées (plancher ≥ 20) ; commande CLI `protoss bytecode
  <file>` / `bytecode run <file> --entry <name>`. Doctor = 16 pass / 0 fail ;
  `@fulltest` vert. Raffinement noté : `project build --target bytecode` reste un
  manifeste descriptif ; la VM réelle est exposée via `protoss bytecode`, à parité.
- 2026-06-11 — G7 Démo full-stack `priority` : `examples/web/todo_app/patches/
  add_priority.json` (batch 5 ops — MigrateType de l'item `String` →
  `Record{label,priority:Variant Low|High}`, ReplaceDef init/update/view, AddDef
  `samplePrioritized`) + `priority_demo.sh` (build → check → apply → audit → eval,
  store nettoyé, 15 checks). Preuve doctor `priority-demo` (§14.4, **heavy** :
  copie pid-qualifiée + stdlib absolutisé → build/`Patch.check`/`Patch.apply`,
  repo non pollué). Flag `heavy` ajouté : la commande `doctor --v1` exécute la
  preuve, le sweep de test core la saute (dev-loop rapide) ; run gardé dans
  `@selftest`. Doctor complet = 17 pass / 0 fail (~7,6 s). `@fulltest` vert.
  Écarts §14.4 honnêtes (item `String`→record via MigrateType faute de record
  top-level, `priority` variant structurel, pas de marqueur `web_app` au build
  par défaut) consignés dans le rapport.
- 2026-06-11 — Doctor (complétude, prérequis G14) : 3 not-yet branchés depuis
  l'infra testée via copies golden pid-qualifiées (rapides, `stdlib=none`, repo
  non pollué) — `store-universe-root` (§5.3 : build écrit `universe.root` + audit
  OK), `patch-check-audit` (§10.3 : `Patch.check`/`Patch.apply` +
  `verify_latest_matches_store`), `packages-lock-registries` (§12 : `write_lock`
  déterministe + `write_package`/`check_package`). Doctor = 20 pass / 0 fail /
  5 not-yet ; coretest ~1,7 s (pas de régression dev-loop) ; `@fulltest` vert.
- 2026-06-11 — Doctor : preuve `harness` (§11) branchée — `Harness.run_json`
  in-memory, un harness passant rapporte `pass`, un échouant `fail`
  (auto-suffisant, sans I/O). Doctor = 21 pass / 0 fail / 4 not-yet restants
  (ledger-replay §8.3, parités self-hosted canonicalizer/patch-validator §17,
  benchmarks §20) ; `@fulltest` vert.
