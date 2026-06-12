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
   - **Interface + FS adapter landed** (`Store.BACKEND` / `Store.Fs_backend`,
     byte-identical refactor; `PROTOSS_STORE_BACKEND` + manifest `store_backend`
     select it). SQLite was investigated and deliberately deferred — see
     section 5 for the interface, why a byte-safe SQLite swap is blocked today,
     and the recommended path.
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

## 5. Storage adapter (brick 3) — delivered surface and the SQLite question

The content-addressed object/graph layer of `.protoss/store` is now mounted
behind a backend interface, `Store.BACKEND`:

```
module type BACKEND = sig
  val name : string
  val put_object_payload : root:string -> hash:string -> payload:string -> unit
  val read_object        : root:string -> hash:string -> string
  val object_exists      : root:string -> hash:string -> bool
  val list_objects       : root:string -> string list
  val put_graph          : root:string -> graph_hash:string -> json:string -> unit
end
```

This surface is derived from what the store actually does, not a speculative
API: objects are keyed by their own content hash (idempotent put-if-absent,
read, exists, list), graphs by the hash of their canonical bytes (idempotent
put). The public `Store.put_object`/`get_object`/`list_objects`/`write_graph`
keep their historical signatures and delegate to the mounted backend, so every
call site and the on-disk byte layout are unchanged. `Store.Fs_backend` is the
filesystem adapter — the historical behaviour verbatim, including global-store
interning + hardlinking (`PROTOSS_GLOBAL_STORE`).

Generic filesystem I/O is deliberately **out** of this interface. `read_file` /
`write_file_atomic` / `ensure_dir_cached` are used across ~14 modules for source
files, lockfiles, reports, the ledger event log and the runtime world store;
those are not "the content-addressed object store" and abstracting them would be
abstracting the whole filesystem. The honest swappable surface is the object/
graph layer only.

**Selection.** The backend is chosen from `PROTOSS_STORE_BACKEND` (process-level
authority, resolved lazily on first store use) and declared in the manifest as
`store_backend = "fs"` (validated at parse time; `Workspace.check_store_backend`
requires the manifest's declared backend to match the mounted one, since one
process serves one store). Like `store_dir`, `store_backend` is a physical /
deploy-time choice and is **not** part of the content-addressed identity — it
never enters `UniverseRoot`. An unknown backend fails loudly (`STORE001`) rather
than silently falling back, so a misconfigured store can never write divergent
bytes. Today only `"fs"` is implemented.

**SQLite: investigated, deliberately deferred.** Three routes were evaluated:

- **(a) opam `sqlite3` binding (optional via dune `select`/virtual lib).**
  Rejected: it breaks the hard constraint that the library depends on nothing
  beyond `unix` (the `protoss deploy` server provisions with `apt opam + dune`
  only and builds the lib from source — `lib/deploy.ml` `provision_script`), and
  it would restructure the single `protoss` library into a virtual-library build.

- **(b) Drive the `sqlite3` CLI over a process.** In practice store objects are
  ASCII text (`kind=…\n<canonical text/JSON>`), so escaping is tractable, but two
  walls remain: (1) `Store.put_object` interns + **hardlinks** project objects to
  the user-level FS global store (`Unix.link`) — a row in a DB cannot be
  hardlinked to a file, so the cross-project dedup is FS-specific and a SQLite
  adapter could not reproduce it; (2) more decisively, the store layout is read by
  **direct filesystem path manipulation** in many places that bypass the object
  API — `workspace.ml` writes `program.graph.json`/host-contract objects with its
  own `write_file`, and `workspace.ml`/`patch.ml`/`runtime_store.ml`/`bin/main.ml`
  read store objects via `Filename.concat store …` + `read_file` + `Sys.file_exists`.
  A SQLite backend that does not also write those exact FS files would break every
  direct reader, and making it write both files *and* rows defeats the purpose and
  cannot be byte-identical. Routing all ~30 direct accessors through the interface
  is a large, invasive change that would itself risk perturbing the stable byte
  layout — out of scope for "the storage adapter" and against the byte-identity
  constraint.

- **(c) Ship the interface + FS adapter, document the SQLite path. ← chosen.**
  An honestly-reduced scope: the backend is now genuinely interchangeable *in
  principle* (the design-doc goal) without pretending a SQLite swap is byte-safe
  while 30 direct FS readers still expect files at fixed paths.

**Recommended SQLite path, when pursued.** Treat it as its own brick, sequenced
*after* a preparatory refactor that is itself proven byte-identical:
1. First route **all** store object/graph access through `Store.BACKEND` (no
   direct `Filename.concat store …` reads/writes anywhere; `program.graph.json`,
   host-contract objects, units/unit-defs included). Prove 0-diff before adding
   any new backend. This is the real cost and must not be rushed.
2. Then add `Store.Sqlite_backend` storing `(hash → payload)` and
   `(graph_hash → json)` rows in one `store.db`, with the *same object bytes*
   (only the arrangement changes, so all content refs / `UniverseRoot` /
   `BackendModelRef` match FS). Prefer the **opam `sqlite3` binding gated behind a
   dune `(select)`** so the default build still needs only `unix`; the binding is
   compiled in only when explicitly enabled. If the binding cannot be made
   optional cleanly, fall back to the `sqlite3` CLI over a process with
   `BEGIN IMMEDIATE`/`COMMIT` transactions and blob-literal (`x'…'`) or
   `.import`-style writes — accepting that global-store hardlink interning is not
   available under SQLite (a documented behavioural difference, not a hash
   difference).
3. Only then does `protoss deploy` need `apt-get install -y sqlite3
   libsqlite3-dev` added to `provision_script`, and only for projects whose
   manifest selects `store_backend = "sqlite"`.
