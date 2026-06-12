# Protoss full-stack backend — design (Lamdera-shaped, content-addressed, ULTRA PERF)

Status: design / direction. Tracks a post-V1.0 build-out. The developer-facing
shape mirrors Lamdera (`BackendModel` + `updateBackend`, typed `ToBackend`/
`ToFrontend`); the engine underneath is event-sourced on the existing ledger,
content-addressed, storage-agnostic, and compiled — not an interpreted Node
process.

## 1. What the developer writes (the only thing they touch)

```
type alias FrontendModel = { ... }
type FrontendMsg = ...

type alias BackendModel = { ... }
type ToBackend  = ...                      -- client -> server messages
type ToFrontend = ...                      -- server -> client messages

updateBackend : ToBackend -> BackendModel -> (BackendModel, Cmd ToFrontend)
-- frontend side: sendToBackend : ToBackend -> Cmd FrontendMsg
```

That is the whole contract. No database, no connection handling, no schema, no
migrations to hand-write (type migrations are already structured patches in
Protoss). Exactly the Lamdera ergonomics: you write a model and an update; the
system owns transport, persistence, scale, and caching.

## 2. Engine (modular, abstracted away from the app)

- **Event store = the ledger.** A `ToBackend` is appended to the append-only
  ledger as a content-addressed event (`ServerRequest` is already a ledger event
  today; `replay_events`/`replay` already fold a world deterministically, with
  branch/merge). The `BackendModel` is **not** a RAM blob to snapshot like
  Lamdera — it is the deterministic *fold* of the event log: `foldl updateBackend
  initialBackendModel events`.
- **Storage is a swappable adapter.** Everything is content-addressed
  (`put(hash, bytes)` / `get(hash)` + append-event), so the on-disk store
  (`.protoss/store`, FS today) is one adapter behind an interface. SQLite
  (embedded, zero-config), Postgres (scale), or a KV (Redis/FoundationDB) are
  drop-in alternatives. The app never sees which one — answering "pg or
  something better": it's a deploy-time choice, not an architecture commitment.

## 3. ULTRA PERF — the levers (where it beats Lamdera)

Lamdera is one interpreted Node process holding the whole model in RAM. Protoss
attacks performance structurally:

1. **Compiled, not interpreted.** `updateBackend` runs as compiled code, not in
   Node. The bytecode backend already lands (`project build --target bytecode`
   emits a real `.ptvm`; `protoss bytecode exec` runs it on the VM). `--target
   wasm`/`llvm` extend this to native. No interpreter on the hot path.
2. **Deterministic + content-addressed ⇒ a perfect cache.** `updateBackend` is
   total and pure, so `(eventHash, modelHash) → resultHash` is a content key
   that can never go stale (the hash *is* the key — no manual invalidation).
   A fold step already computed anywhere is never recomputed.
3. **Incremental snapshots.** The `BackendModel` is a periodic content-addressed
   snapshot plus the delta of events since it. Reconstruction is `O(events since
   snapshot)`, not `O(history)`.
4. **Content sharding.** Deterministic event sourcing + content-addressing means
   the model can be partitioned (e.g. per aggregate key) across workers, each
   folding its shard. Horizontal scale that a single Node process can't do.
5. **Dedup + zero-copy reads.** Identical events/states are shared by hash across
   storage and the wire; snapshot reads are served by hash from cache/CDN.

Determinism is what unlocks 2–4: a non-deterministic backend (arbitrary JS)
cannot cache, replay, or shard safely. Protoss's totality/content-addressing is
the moat.

## 4. What already exists vs. what to build

Exists: the ledger (event log, deterministic replay, branch/merge), `ServerRequest`
as a ledger event, the content-addressed store, the frontend `Cmd` architecture,
and the **bytecode backend (compile + execute)** — the perf brick.

To build (ordered bricks, each shippable and proven):

1. **Backend architecture types.** Teach `protoss app check` the
   `BackendModel`/`ToBackend`/`ToFrontend`/`updateBackend` shape, alongside the
   existing Process/Cmd frontend architecture. (Kernel/workspace; the socket
   everything else plugs into.)
2. **Ledger-backed `BackendModel`.** `ToBackend` → ledger event; `BackendModel`
   → deterministic fold + cached content-addressed snapshot. Replay/audit/
   time-travel come for free from `ledger.ml`.
3. **Storage adapter interface.** Abstract the store behind put/get-by-hash +
   append-event; add SQLite then Postgres adapters. App-invisible.
4. **Compiled backend execution.** Run `updateBackend` via the bytecode/native
   backend on the server hot path (reuses `--target bytecode` + `bytecode exec`).
5. **Transport + end-to-end demo.** Wire `sendToBackend`/`sendToFrontend`; ship a
   toy full-stack app (a shared list) exercising frontend → ToBackend →
   updateBackend → ledger → ToFrontend.
   - **`sendToBackend` landed.** `sendToBackend e : Process BackendModel` is a
     first-class typed effect node (`Ast.ESendToBackend` → `Kernel.CBackendSend`,
     under the `Server.request` capability — distinct from `ServerRequest`
     because its payload is a value and its response type, the program's
     `BackendModel`, is read by the kernel from `updateBackend`). The kernel
     checks the payload against `ToBackend` (short variant constructors work),
     types the result as `BackendModel`, and rejects a missing backend half with
     the stable `BACKEND010` error. The browser runtime evaluates the payload to
     a value, POSTs it to `/__server` as value-JSON (`{route:"__backend",
     backendSend:<value-JSON>}`), the dev server decodes it back to a typed
     `Runtime.value` (`Web.value_of_json`, the inverse of `value_to_json`), folds
     it via `Backend.send_value` (which writes the SAME `to-backend` ledger event
     as the stringly text path — transport-agnostic replay), and answers the new
     `BackendModel` as value-JSON, which the process resumes with directly.
   - **`broadcast` landed (the server → client push).** `broadcast e :
     Cmd caps ToFrontend` is the symmetric typed transport: the value
     `updateBackend` returns in its command slot (`Cmd.none` stays `unit`).
     `Ast.EBroadcast → Kernel.CBroadcast` carries the `ToFrontend` type (read
     from `updateBackend`, like `CBackendSend` carries `BackendModel`); the node
     adds no fixed capability (its scope is the declared `Cmd` caps). It is
     rejected with `BACKEND010` (no backend half) or `BACKEND013` (used where the
     surrounding `Cmd`'s message is not the contract `ToFrontend`). At runtime the
     cmd evaluates to a `Runtime.VBroadcast (toFrontendTy, value)`;
     `Backend.broadcast_of_cmd` extracts the `ToFrontend` value (`Cmd.none` →
     `None`). The dev server holds a second SSE channel `GET /__events` (distinct
     from `/livereload`); when a fold's command slot is a `broadcast`, it pushes
     the `ToFrontend` value-JSON to **every** subscribed client. The optional
     convention `fromBackend : ToFrontend -> Msg` (validated by `app check` —
     `WEB025`/`WEB026`/`WEB027`) is the receive half: when present, the emitted
     bundle carries its def-id and the runtime subscribes to `/__events`, mapping
     each pushed `ToFrontend` to a `Msg` and dispatching it. **The broadcast is an
     ephemeral OUTPUT effect: it is NOT recorded in the ledger** — only the
     `to-backend` event is appended, so the `BackendModel` fold stays
     reconstructible from `to-backend` events alone and broadcasts re-derive from
     the fold (no new event kind). The scaffold (`protoss project init`) demos the
     full loop: `updateBackend` returns `(tuple model' (broadcast (Synced ...)))`,
     `fromBackend` maps `(Synced n) → (GotShared n)`, and a click in one browser
     updates the shared counter in every open browser.
     Still to wire: the shared-list demo and bytecode lowering of both transports.

Each brick is built in isolation (kernel changes via worktree agents with
determinism proofs), `@fulltest` green before commit, hashes proven stable.
