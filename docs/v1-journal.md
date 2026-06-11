# Journal V1 — boucle autonome

Démarrée le 2026-06-12. Objectif : faire avancer Protoss vers une V1.0 solide, large
périmètre (pas que la syntaxe). Règles : déterminisme sacré (sucre/refacto préservent le
hash, prouvé) ; `dune build @fulltest --force` vert avant chaque commit ; commit + push à
chaque chose finie ; changements kernel/risqués via agent isolé en worktree avec preuves de
déterminisme (hash avant/après, sweep `examples/`) vérifiées avant intégration.

## En cours
- **Exemple full-stack réel : app todo en Protoss/H** — agent isolé `ac3e66` (background). Réécriture de
  `examples/web/todo_app/src/app.protoss` (S-exp → Elm-like) à HASH IDENTIQUE, en préservant noms/types
  (pour garder `priority_demo.sh` + `add_priority.json` valides). Test d'expressivité §14.

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
- [ ] Robustesse runtime/ledger/Process : cas limites des effets typés, resume/replay (en cours)
- [ ] Constructeurs de variant comme candidats de suggestion (`Nome`→`None`) — découvert ce tour
- [ ] `fmt --human` émet les formes courtes (`[...]`, variants courts) au lieu de Cons/Nil/variant explicites
- [ ] Inférence du type de param des lambdas anonymes (HOF type `List.map (\x -> ...) xs`) — kernel, suite du fix bidirectionnel
- [ ] Fuzzer : nouvelle passe sur parser/checker/runtime pour débusquer des crashs non structurés
- [ ] Exemple d'app full-stack réel et propre en Protoss/H (todo réécrit avec `[...]`, keyed, alias)
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
