# Journal V1 — boucle autonome

Démarrée le 2026-06-12. Objectif : faire avancer Protoss vers une V1.0 solide, large
périmètre (pas que la syntaxe). Règles : déterminisme sacré (sucre/refacto préservent le
hash, prouvé) ; `dune build @fulltest --force` vert avant chaque commit ; commit + push à
chaque chose finie ; changements kernel/risqués via agent isolé en worktree avec preuves de
déterminisme (hash avant/après, sweep `examples/`) vérifiées avant intégration.

## En cours
- **NOUVELLE DIRECTION (priorité, demandée par le user) : backend full-stack façon Lamdera, ULTRA PERF.**
  Design directeur dans `docs/backend-architecture.md` (commit d887049). Le user veut l'ergonomie Lamdera
  (BackendModel + updateBackend, infra abstraite/modulaire) mais ULTRA PERF. Choix tranché : event-sourcing
  sur le ledger existant (BackendModel = fold déterministe, pas blob RAM), stockage = adaptateur content-
  addressed interchangeable (FS/SQLite/PG), perf = backend COMPILÉ (bytecode/natif, pas Node) + cache
  déterministe parfait + snapshots incrémentaux + sharding. Plan en 5 briques (voir le doc).
  **Brique ① FAITE** (commit ac687a8) : `app check` reconnaît la moitié backend optionnelle —
  `initBackend : BackendModel` + `updateBackend : ToBackend -> BackendModel -> (Tuple BackendModel
  (Cmd caps ToFrontend))` (miroir de la forme cmd), rapporte `Backend OK backendModel=… toBackend=…
  toFrontend=…`. Additif : app frontend-only → `backend = None`, host contract intact, hash compteur
  inchangé `b96036…`. Non-laxité : WEB021 (model mismatch), WEB001 (initBackend manquant). Pas eu besoin
  d'agent worktree : le changement est dans web.ml (pas le kernel), additif, prouvé par hash + @fulltest.
  **Brique ② FAITE** (commit ee7779e) : BackendModel = fold déterministe d'`updateBackend` sur les
  événements `to-backend` du ledger (nouveau kind, même discipline content-addressed `add_event`,
  validation stricte). `lib/backend.ml` : messages typés par le kernel via mini-programme structurel
  (mal typé → rejet AVANT append) ; chaque événement porte le hash de la valeur canonique typée,
  re-vérifié au replay (BACKEND005 si forgé). CLI `protoss backend state|send` (ledger sous
  `.protoss/ledger`, branche `backend`), codes BACKEND001-005 au catalogue. Preuves : bump/bump/reset/
  bump → {count = 1} ; state (replay pur) == dernier send (même BackendModelRef p2:970a41…) ; 2 ledgers
  indépendants, même séquence → world ET model refs byte-identiques ; `ledger replay` natif valide les
  événements. `@fulltest` vert.
  **Brique transport FAITE** (commit 57247a3) : le runtime web POSTe toute suspension `Server.request`
  vers `/__server` ({"route","payload"}) et résume le process avec la réponse texte. `Web.serve` prend le
  handler en callback (`?server_request`, câblé dans bin/main.ml → pas de cycle de modules) : route
  `__backend` → `Backend.send` (type, append ledger, fold) → répond le nouveau BackendModel. Erreurs
  handler → HTTP 500 avec code public. `?bind_any` (`--public`) pour écouter 0.0.0.0. Prouvé par curl :
  POST (Bump unit) → {count = 1} → {count = 2} inter-requêtes, mal typé → 500 BACKEND002, et
  `backend state` (CLI) lit le même {count = 2} — serveur et CLI partagent l'état event-sourcé.
- **`protoss deploy` FAIT** (commit 8abcf47, demande explicite du user) : valide l'app AVANT l'infra,
  serveur Hetzner par app `protoss-<name>` idempotent (hcloud), provision opam/ocaml-system, rsync des
  sources protoss + build distant, sync app en PRÉSERVANT le `.protoss` serveur (le ledger de prod
  survit aux redeploys), réécrit le chemin stdlib du manifest, systemd `protoss live --port 80 --public`,
  DNS Cloudflare : upsert A record PROXIED `<name>.<domain>` (TLS via CF) si CLOUDFLARE_API_TOKEN,
  sinon imprime le record exact. Domaine défaut charlon.dev. DEPLOY001-005 au catalogue.
  **Déploiement réel de /tmp/demo (--name demo) EN COURS en background** (compteur partagé S-exp,
  Backend OK). Vérifier au réveil : health check IP, rapport au user (coût cx22, destruction).
- Découvert + qualifié : `elm_syntax.ml:310` parse `Process|Cmd { caps }` mais SEULEMENT en tête de
  signature — pas imbriqué dans un type composé (ex. `Tuple Model (Cmd {} Msg)`, la forme exacte
  d'updateBackend). `{}` imbriqué tombe dans le parseur de record → `(Cmd (Record ) …)` → erreur.
  Item syntaxe : gérer le scope `{caps}` dans le parseur de type imbriqué (fix ciblé, hash-preserving
  à prouver vs la forme S-exp). En attendant, les apps backend s'écrivent en S-exp.
- **DÉPLOIEMENT RÉEL RÉUSSI** (2026-06-12) : cax11 ARM épuisé dans les 3 DC EU → cpx11/ash (x86 US,
  ~4.35€/mois). Serveur `protoss-demo` (IP : `hcloud server ip protoss-demo` — on ne committe pas les IP d'origine). Pipeline complet OK : create → provision
  opam/ocaml-system (build OCaml 4.14 distant passe) → rsync sources + build → sync app → systemd :80.
  VÉRIFIÉ EN PROD : frontend HTTP 200 ; backend event-sourcé `POST /__server (Bump unit)` → {count = 1}
  → {count = 2} (ledger de prod persiste et fold) ; mal typé → 500 propre. DNS : pas de
  CLOUDFLARE_API_TOKEN en env → record à créer par le user : `A demo.charlon.dev → <hcloud server ip protoss-demo> (proxied)` ; avec le token exporté, le prochain `protoss deploy` le fera seul. Destruction :
  `hcloud server delete protoss-demo`.
- Item DX en attente (mineur, repoussé) : nettoyer les messages d'erreur de type redondants (double
  « expression X, expression X » au wrapper de def kernel.ml:4151 ; « expected context: expected » via
  require_type_expr 1987/2003). Edit prêt, non appliqué.

## Note opérationnelle
- 2026-06-12 — L'agent worktree `ac3e66` (todo Protoss/H) a échoué à REPRENDRE via SendMessage :
  « Not logged in · Please run /login » (0 token, échec immédiat — hoquet d'auth transitoire). Son
  PREMIER run (rapport d'investigation) avait réussi. J'ai donc fait le fix kernel input/list moi-même
  (pattern éprouvé + preuves complètes). Leçon : si un agent échoue sur le login, ne pas insister —
  faire le travail soi-même quand il est bien cadré et prouvable.

## Audit V1 (ce tour)
- `protoss doctor --v1` : **25 pass, 0 fail, 3 not-yet** — et les 3 not-yet (canon/nf/patch self-host parity)
  sont en fait prouvés par les sweeps `@selftest`, juste pas dupliqués dans le doctor (eval prélude lent).
  Conclusion : système mûr, aucun trou Fail/Not_yet réel. §8.4 (branch/merge) pas dans le doctor mais
  déjà testé (test_protoss.ml:7672-7804) + CLI (`protoss ledger fork|merge|diff|branches`). La valeur
  restante est DX/ergonomie/exemples/profondeur, pas le comblement de trous béants.
- **Expressivité Protoss/H (découvert ce tour)** : records pleinement supportés (type/littéral/accès/update,
  `p2:558b…`). Lambdas anonymes `\x -> body` parsent MAIS sans annotation de type (jamais `\(x:T)`), donc
  type de param TOUJOURS inféré. `List.map (\x -> succ x) xs` ÉCHOUE (lambda en 1ʳᵉ position → rien ne fixe
  son type : "cannot infer List.map: expected function type"). `::` (cons) PAS supporté en Elm-like (→ `Cons`).
  → Item kernel potentiel : inférence du type de param d'une lambda anonyme depuis la signature de la fonction
  appliquée (suite logique du fix bidirectionnel listes/variants). L'agent todo dira si l'app bute dessus.

## File priorisée (révisée à chaque tour — impact × ce que ça débloque)
- [x] Variants courts (`Increment unit` / bare `Reset`) — FAIT, commit 4071106
- [x] Scaffold `protoss init` en Protoss/H (alias + `[...]` + variants courts, hash identique) — FAIT, 29e2795
- [x] Messages d'erreur : seuil « did you mean » relatif à la longueur (plus de `Nope`→`bad`) — FAIT, 5c7769e
- [x] Inférence lambdas anonymes input/list (widgets `input`/`list`) — FAIT, 8f1592e
- [x] Exemple full-stack réel : app todo réécrite en Protoss/H, hash identique, priority_demo PASS — FAIT, 51e78ae
- [x] Constructeurs de variant comme candidats de suggestion (`Nome`→`None`) — FAIT, 187571f
- [x] README à jour (variants courts + inférence lambdas input/list) — FAIT, d35ea8b
- [ ] `fmt --human` émet les formes courtes (`[...]`, variants courts) au lieu de Cons/Nil/variant explicites
- [ ] Inférence lambda anonyme en 1ʳᵉ position (`List.map (\x -> ...) xs`) — kernel, reste du fix HOF
- [x] Cas check `when`/`on` (dernières vues sans propagation du type attendu) — FAIT, 55a64e9
- [ ] Robustesse runtime/ledger/Process (audit ce tour : mûr — approfondir les cas limites resume/replay)
- [x] Fuzzer : 5e target `checker` (parse + check_program), 0 crash / 2000 iter — FAIT, 9535f9b
- [ ] `fmt --human` formes courtes : l'émetteur est hash-safe mais VERBEUX (émet `column (Cons …)` et `variant (Variant …) Set t` au lieu de `[ … ]` / `Set t`). Load-bearing → tour dédié (agent ?)
- [ ] Audit spec : features de protoss-spec.md / ship-checklist marquées faibles ou incomplètes
- [ ] Doc : cheatsheet de la syntaxe humaine Protoss/H

## Avancement
- 2026-06-12 — Démarrage du loop. DX déjà livrée juste avant (cette session) :
  `protoss init` alias robuste (ba6bee7) ; scaffold full-stack + `protoss live` + hot reload SSE
  multiplexé + résilience SIGPIPE ; VDOM diff/patch par position puis keyed (8b04600, 341dee2) ;
  `Html.keyed` (52fe5b7) ; inférence bidirectionnelle des littéraux de liste sous `column`/`row`
  (7e82b31, +6 lignes kernel, déterminisme prouvé). Découverte notée : le prélude n'est pas
  tree-shaké (toute def ajoutée change le Build hash de toute app ; aucun hash épinglé en dur).
- 2026-06-12 — **Variants courts FAIT** (commit 4071106, agent worktree a6673f44, diff relu +
  réintégré + reprouvé moi-même). 3 arms `check_elab` (body-only, .mli figé intact) :
  `(Con payload)` et bare `Con` (payload Unit) contre un type Variant attendu élaborent vers le
  graphe canonique EXACT de `(variant TYPE Con payload)` ; propagation du type attendu à travers
  `button`. Preuve indépendante CLI : `(Increment unit)` == `(variant Msg Increment unit)` ==
  `p2:a7478bbf…` ; sweep examples/ (104 fichiers) byte-identique ; `(Nope unit)` toujours rejeté
  REF001 `:3:23:`. `@fulltest --force` vert (26 s). Permet `column [ button "Increment"
  (Increment unit) ]` en Protoss/H. Réflexe noté (corrigé) : le moteur « did you mean » suggère
  le def en cours (`Nope`→`bad`) — capturé comme prochain item DX.
- 2026-06-12 — **Scaffold `protoss init` en Protoss/H FAIT** (commit 29e2795). `counter_app_source`
  réécrit en Elm-like : `type alias Msg`, `case ... of`, `column [ ... ]`, `(Increment unit)`.
  Preuve d'équivalence bout en bout : un projet scaffizé build à `p2:b96036…1641` que le source soit
  la forme Protoss/H ou l'ancienne S-exp (via `project build` + prélude complet) — l'invariant central
  (même programme, deux syntaxes, même hash) démontré sur le vrai scaffold. `@fulltest --force` vert.
- 2026-06-12 — **Seuil « did you mean » FAIT** (commit 5c7769e). `distance <= 4` fixe →
  `max 1 (len/3)` (style rustc) ; plus de suggestion absurde (`Nope`→`bad`, 4 éditions sur 4 lettres),
  vraies typos toujours suggérées (`incremen`→`increment`). Error-path pur (code mort pour tout
  programme valide → zéro hash bouge), test de régression ajouté. Trou identifié : constructeurs de
  variant pas candidats (`Nome`→`None`) → item séparé.
- 2026-06-12 — **Inférence input/list + app todo en Protoss/H FAIT** (8f1592e fix kernel, 51e78ae exemple).
  L'agent `ac3e66` avait caractérisé le trou (EInput/EListView sans cas `check_elab` → lambda anonyme sans
  type attendu) puis échoué à reprendre (login). Fix fait moi-même : 2 cas `check_elab` sous TView (handler
  contre `TFun(String, msg_ty)` ; render contre `TFun(item_ty, View msg_ty)`), miroir exact des cas infer →
  cterm byte-identique. Preuves : app todo originale S-exp ET réécrite Protoss/H buildent toutes deux à
  `p2:f90aa6e…fd17` ; sweep examples/ 103 fichiers 0-diff parent vs patché ; test unitaire input/list
  inféré==annoté `p2:f359c2…` ; `priority_demo.sh` PASS (15 checks, patch+migration intacts) ; `@fulltest` vert.
  Série widgets bidirectionnels complète : column/row (7e82b31) → button (4071106) → input/list (8f1592e).
  Protoss/H exprime désormais une app full-stack entière à hash identique.
- 2026-06-12 — **Fuzzer étendu au checker FAIT** (commit 9535f9b, harnais de test seul, 0 hash). Le
  fuzzer de robustesse (G3) couvrait les 4 décodeurs d'entrée mais s'arrêtait au parser ; un programme
  qui parse mais fait crasher le checker de façon non structurée passait. Ajout d'un 5e target `checker`
  (parse + `check_program` sur seeds mutés bornés en profondeur → total → termine), + un seed « vue
  complète » (column/input/list/button) pour couvrir les cas check_elab des widgets. Résultat :
  success=311, structured=1689, **0 new + 0 known crash** → checker robuste sur entrée parsable.
  `@fulltest --force` vert (targets=5, strict). Découverte annexe : `fmt --human` est hash-safe mais
  émet du verbeux (Cons/Nil, variant explicite) → item « émetteur formes courtes » (tour dédié, load-bearing).
- 2026-06-12 — **Suggestion de constructeurs + README FAIT** (187571f kernel, d35ea8b doc).
  `suggestion` ne piochait que dans locals/globals/builtins → une typo sur un constructeur (`Nome` pour
  `None`) n'était pas suggérée (les constructeurs vivent dans le corps des type aliases, pas dans un
  namespace plat). Collecte des noms de constructeurs de tous les `TVariant` en scope → `(Nome unit)`
  donne « Did you mean None? ». Error-path pur (0 hash), test ajouté, `@fulltest` vert. README : ajout
  des variants courts + inférence lambdas input/list à la description de la surface Elm-like. Note :
  `examples/web/site_vitrine` existe déjà comme app Protoss/H complète (en plus du todo réécrit).
- 2026-06-12 — **Fuzzer → évaluateur FAIT** (commit 7a4cf4d, harnais de test seul, 0 hash) + investigation
  perf (négatif utile). 6e target `evaluator` (check + `Runtime.normalize_all`, total → termine sur seeds
  bornés ; `Runtime.fail = Kernel.fail` → erreurs structurées). success=327, **0 crash** → évaluateur
  robuste, pas de divergence. `@fulltest` ~25s inchangé. **Perf dev-loop** mesurée : `app check` 0.83s,
  @coretest 1.15s, @selftest 7.3s (goulot = éval interprétée du frontend self-hosté). L'évaluateur est
  DÉJÀ optimisé : `eval_app` court-circuite le hash de clé de cache quand le cache est off (piège CLAUDE.md
  déjà corrigé), `trace` gardé, concaténations après court-circuit. Seul lookup linéaire restant : `nth_env`
  (env De Bruijn en liste, O(i)) — optimisation liste→array invasive, gain incertain, écartée pour l'instant.
  Robustesse parser+checker+eval désormais toute sous fuzzing (6 targets).
- 2026-06-12 — **Cas check `when`/`on` FAIT** (commit 55a64e9). C'étaient les dernières vues à inférer
  leur sous-terme bottom-up (donc variant court/lambda anonyme échouait dedans) : `when c (button "x"
  (Tick unit))` et `on "click" (Tick unit)` donnaient « unknown name: Tick ». 2 cas `check_elab` :
  `TView msg_ty, EWhenView` (body contre `TView msg_ty`) et `TAttr msg_ty, EOn` (msg contre `msg_ty`),
  miroir des cas infer → cterm identique. Preuves : courts == explicites (`a3daf9…`/`b3f7f5…`), sweep
  examples/ 0-diff, test ajouté, `@fulltest` vert. **Couverture widgets complète** : column/row/button/
  input/list/node/when/on propagent tous le type attendu. Écarté ce tour : inférence HOF `List.map
  (\x -> ...)` (lambda non annotée en 1ʳᵉ position → nécessiterait méta-variables/report d'élaboration,
  chantier majeur, pas un cas check) ; `fmt --human` formes courtes (hash-safe local difficile à garantir).
- 2026-06-12 — **Exemple tour des vues Protoss/H FAIT** (commit a165b42). `examples/protoss_h_views.pt`
  exerce TOUS les widgets (column/row/button/input/list/when/on/node) en formes ergonomiques (variants
  courts, lambdas anonymes, `[...]`) — démo + fixture de régression (test `Loader.check_file`, compile
  `p2:56db7b…`). Valide de bout en bout le travail d'inférence des vues de ce cycle. Investigation
  runtime/effets : effets dispo (Human.ask/Clock.read/Http.get/Server.request/Local.save), cycle Process
  run→resume→replay testé par le doctor (`ledger-replay` PASS) ; modèle de base sain, pas de trou évident.
- 2026-06-12 — **Fuzzer → invariant round-trip canonique FAIT** (commit 5eb70e9, harnais de test seul).
  Saut qualitatif : le fuzzer vérifiait « pas de crash » ; il vérifie désormais une PROPRIÉTÉ DE
  CORRECTION. 7e target `canonical-roundtrip` : pour chaque programme VALIDE, le graphe canonique doit
  round-tripper vers la même sérialisation canonique ET re-dériver un checked depuis le graphe préserve
  le program hash (l'invariant central du content-addressing). Une violation lève `Invariant_violation`
  (NON structurée) → rapportée comme bug, distincte d'une erreur de check ordinaire. Résultat :
  success=325, **0 violation** → l'invariant central tient sur des milliers de programmes aléatoires
  mutés, pas seulement les fixtures fixes. `@fulltest` vert (targets=7). Fuzzer couvre maintenant :
  4 parsers + checker + évaluateur + invariant de correction.
- 2026-06-12 — **Backend bytecode réel FAIT** (commit 942f8b6, coche spec §15 case 719). `project build
  --target bytecode` n'écrivait qu'un manifeste de métadonnées (stub), alors que la codegen bytecode
  (`Bytecode.compile_checked`/`encode_module`) existait et était testée (doctor bytecode-encoding/parity).
  Branché : `build_compiler_backend` écrit maintenant le vrai module encodé `<ref>.ptvm` déterministe.
  Additif (octets content-derived → compiled-artifact ref inchangé). Preuve : 2 builds indépendants →
  bytecode byte-identique (1.4 Mo) ; test : `.ptvm` == `encode_module` du build + décode round-trip stable.
  README + spec à jour, `spec check` 308 (était 307), doctor spec-audit PASS, `@fulltest` vert. Vérifié au
  passage : runtime web JS gère TOUS les widgets (ListView/WhenView/On/Node) → pas de trou de cohérence.
  Autres backends (wasm/llvm/js/sql/gpu) restent des stubs manifeste (post-V1).
- 2026-06-12 — **`bytecode exec` FAIT** (commit 27ba586). Le backend émettait le `.ptvm` mais rien ne le
  consommait, et `Bytecode_vm.exec_module` (exécute un module nu décodé, sans contexte checked) n'avait
  aucun appelant. Branché à une commande CLI : `protoss bytecode exec <file.ptvm> --entry <name>` décode
  le module buildé et exécute la def sur la VM → backend bytecode END-TO-END (build → .ptvm → exec, sans
  source). Globals résolus parmi les defs du module, scope de capabilities vide → exact sur le fragment
  pur (`bytecode run <src>` pour les defs à effets qui ont besoin des capabilities déclarées). Vérifié :
  `main` d'un module buildé exec → 3 == run depuis source. Test de parité ajouté, README/usage à jour,
  `@fulltest` vert. Le backend bytecode est désormais le 1er backend complet (compile + exécute).
