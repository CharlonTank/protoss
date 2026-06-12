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

Each brick is built in isolation (kernel changes via worktree agents with
determinism proofs), `@fulltest` green before commit, hashes proven stable.
