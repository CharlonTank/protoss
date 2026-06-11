# Journal V1 — boucle autonome

Démarrée le 2026-06-12. Objectif : faire avancer Protoss vers une V1.0 solide, large
périmètre (pas que la syntaxe). Règles : déterminisme sacré (sucre/refacto préservent le
hash, prouvé) ; `dune build @fulltest --force` vert avant chaque commit ; commit + push à
chaque chose finie ; changements kernel/risqués via agent isolé en worktree avec preuves de
déterminisme (hash avant/après, sweep `examples/`) vérifiées avant intégration.

## En cours
- (rien — prochain item à décider au réveil)

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
- [ ] Cas check pour les widgets restants (`on`, `image`, `when`) si une lambda/variant court y est attendu
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
