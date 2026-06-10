# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

Protoss is an OCaml/Dune prototype for **graph-first, content-addressed** apps. A `.protoss`
source file is checked, canonicalized into a typed graph, and hashed to a stable
`p2:`/sha256 content ref. The same program written in different surface syntaxes (S-expression,
Elm-like, with/without type/lambda inference, with/without aliases) **must produce the identical
canonical graph and hash**. Preserving that property is the central design constraint — treat any
change that makes equivalent sources hash differently, or that introduces non-determinism, as a bug.
Workspace builds also write a deterministic `UniverseRoot` (`universe.root` plus
`universe.root.content`) over package metadata, defs, types, harness slots, policies, and world refs;
lock/package flows must keep their `universe-root` field in sync with that store root.
Policy names are executable, not just descriptor text: for example
`NoNetworkExceptDeclared` requires `Http.*` and `Server.*` capabilities to be present in manifest
`capabilities`.
Package aliases are local manifest entries (`package_aliases = ["name@semver=path"]` or
`package_policy_aliases = ["name@policy=path"]`) and must validate the imported package
name/version or advertised policy before resolving to the locked package hash.
`protoss grammar kernel` is the stable executable grammar surface for the trusted core; update
`Kernel.executable_grammar_text` when adding core forms.
`Store.put_object` interns through `PROTOSS_GLOBAL_STORE` or `$HOME/.protoss/global-store` and
hardlinks project objects to that global payload when possible; preserve content-addressed writes.

`README.md` is the de-facto spec: it enumerates every supported surface form, command, and
invariant in detail. Consult it before assuming a feature does or doesn't exist.

## Commands

```sh
dune build                       # compile
dune runtest --force             # fast smoke suite (test/smoke.ml, ~seconds)
dune build @fulltest             # everything: correct without --force (rules declare fixture deps)
dune build @fulltest --force     # same, but always rerun (~10 s)
dune exec protoss -- <args>      # run the CLI (see README for the full command list)
```

- `test/test_protoss.ml` (~7k lines of flat `let () = ...` assertion blocks) holds the real
  suite. Its sections are gated by env vars and exposed as parallel dune aliases:
  `@coretest` (always-on core), `@selftest` (PROTOSS_RUN_SELF_HOST_TESTS),
  `@stdlibtest` (PROTOSS_RUN_STDLIB_TESTS), `@integrationtest` (PROTOSS_RUN_INTEGRATION_TESTS,
  itself split by PROTOSS_INTEGRATION_PART into `@integrationtest-workspace`,
  `@integrationtest-web`, `@integrationtest-runtime`). The workspace part is sliced by
  PROTOSS_WORKSPACE_PART (`-project`/`-consumer`/`-corruption`) and the web part by
  PROTOSS_WEB_PART (`-app`/`-patches`/`-audit`); slices rebuild their fixture projects
  deterministically instead of sharing state, and unknown slice names fail loudly.
  `@fulltest` aggregates per-section aliases as parallel processes: the workspace part runs
  whole (its tests use a mini stdlib written by the test, so it is sub-second), the web slices
  stay split (each rebuilds the todo app against the full prelude — that is where full-prelude
  build/audit coverage lives). Test rules declare their fixture deps (`test-fixtures` alias),
  so plain `dune build @fulltest` is correct: no-op when nothing changed, rerun on fixture or
  binary changes. Run it before declaring kernel/runtime/workspace changes safe. Test temp
  files must be pid-qualified (see `temp_dir`/`patch_file`): sections run concurrently and
  race on fixed temp paths otherwise. Tests must copy only example `src/` trees (never whole
  example dirs): fixtures live read-only in _build once declared as deps, and CLI runs may
  leave git-ignored `.protoss` stores that the suite must not inherit.
- Compilation is fast (~1-2 s incremental); the slow part is *running* tests, because the
  interpreted self-hosted frontend (prelude evaluation) dominates. Keep cache-key hashing and
  other bookkeeping out of the evaluator's hot path (see `Runtime.eval_app`) — work that only
  serves the opt-in persistent cache/tracing must stay behind those flags.
- `kernel.ml` has a **frozen inferred `kernel.mli`** so body-only edits (including new private
  helpers) don't recompile every dependent module; when adding public kernel API, add its
  signature to `kernel.mli` too (the compiler error dictates it).
- Expensive whole-program kernel outputs are memoized twice: few-slot caches by *physical
  identity* (one checked program flowing through build/lock/package), and by *content hash*
  (`check_program` keyed on the digest of the marshaled AST; node graph / graph fields /
  graph content hash keyed on the canonical program hash) because audit/patch/locked flows
  re-load byte-identical programs under fresh identities. Determinism makes content keys
  sound: same canonical hash ⇒ byte-identical artifacts. Only successful checks are cached;
  failures keep their exact error behavior.
- SHA-256 dispatches to a hardware-accelerated C stub on macOS (`protoss_sha256_stubs.c`,
  CommonCrypto) and falls back to the pure-OCaml `Hashcons.digest_pure` elsewhere; both are
  asserted bit-identical in the core tests. The same stub file exposes `Store.try_clone`
  (APFS copy-on-write clonefile) used by tests to copy project trees cheaply.
- Store writes go through `Store.write_file_atomic_if_changed`/`ensure_dir_cached`: artifacts
  are deterministic, so identical rewrites are skipped and directory chains are stat'd once
  per process. Nothing in the project model may depend on store-file mtimes.
- If a store-write skip predicate (e.g. `Workspace.prepared_store_current`) guards a write
  block, it must cover *everything* that block writes (units and type aliases included) —
  unit metadata is keyed by absolute source path, so program-level hashes alone are not enough.
- `dune exec protoss -- invariants ...` runs executable correctness checks (canonicalization,
  graph round-trip, alpha-stability, typed Process resume, ledger events). These are the
  load-bearing self-checks — run them after touching the kernel, IR, or runtime.
- Common dev loop: `dune exec protoss -- app check examples/web/todo_app` then
  `project build ... --target web`. `fmt --check`, `audit`, and `project lock --check` guard drift.

## Architecture

Everything lives in the single `protoss` library under `lib/`; `bin/main.ml` is a thin CLI
dispatcher (pattern-matches argv → `command_*` functions → `Protoss.<Module>` calls).

**Compilation pipeline** (source text to hash):

1. `parser.ml` — entry point for text. Auto-detects surface syntax: if `Elm_syntax.looks_like`,
   it routes through `elm_syntax.ml` (`to_sexp_source`) to lower Elm-like text to S-expressions
   first; otherwise parses S-expressions directly (`sexp.ml`). Either way the result is one
   canonical S-expression AST, so both syntaxes converge before checking.
2. `ast.ml` — the surface AST (`typ`, `expr`, `def`, `program`, `req`). Plain data, no logic.
3. `loader.ml` — reads files/workspaces, applies aliases/desugaring, attaches `path:line:col`
   source locations to errors.
4. `kernel.ml` — **the pure, total core** (~4000 lines, by far the most important file). Holds the
   typechecker, the canonicalizer (surface AST → canonical `cterm`/graph), capability checking,
   normalization, and all serialization/hashing logic. `canonical_ir.ml`, `canonical_type.ml`,
   `typechecker.ml`, `normalizer.ml`, `hasher.ml`, `normal_value.ml`, and `kernel_error.ml` are
   thin re-export shims over `kernel.ml` — real changes happen in the kernel.
5. `hashcons.ml` — the sha256 implementation and `p2:` content-ref hashing/interning.

**Around the core:**

- `runtime.ml` — interpreter over the canonical graph. Evaluates `view`/`update`, suspends
  external effects as typed `Process` requests (`AskHuman`, `HttpGet`, `ReadClock`, `SaveLocal`,
  `LoadLocal`, `ServerRequest`).
- `workspace.ml` — project model (`protoss.toml`): build, lock, package, interface. Writes the
  content-addressed `.protoss/store` (canonical defs, graph objects, host contracts, lockfile).
- `store.ml` — content-addressed on-disk store primitives (atomic writes, graph-put/get).
- `web.ml` — deterministic web bundle emission (`index.html`, runtime JS, embedded canonical graph
  + capabilities + host contract as JSON).
- `ledger.ml` — append-only event log over `WorldRef`/`EventRef` for Process
  run/resume/replay/diff, deterministic branch/merge, and typed external-error events.
- `patch.ml` / `patch_audit.ml` — atomic content-addressed edits to a store, with a hash-linked
  audit chain (`store/patches/latest`).
- `json.ml`, `string_prim.ml` — pure JSON ADT/encoder and string primitives, also exposed to
  Protoss programs via the prelude.
- `invariants.ml` — the executable invariant checks behind the `invariants` command.

**Standard library:** `stdlib/prelude.protoss` (polymorphic List/Maybe/Result/Map/Set/String/Nat
helpers, plus self-hosted Sexp/Json/Protoss parsers written in Protoss itself).

## Conventions and invariants to respect

- **Determinism is sacred.** No wall-clock, no randomness, no map/set iteration-order leaks into
  output. Canonical JSON serialization rejects unknown fields and non-canonical ordering on the way
  back in. File writes are atomic (`write_file_atomic`).
- **Surface sugar must elaborate, not extend the canonical graph.** New surface forms (inference,
  `match`, tuples, `letRecord`, aliases, modules) lower to existing canonical nodes; they must not
  introduce new canonical node kinds or change DefIds/hashes of equivalent programs. Add a test
  asserting the sugared and desugared forms hash identically.
- **Alpha-stability:** binder names must not affect the canonical graph or hash.
- Error messages carry `path:line:column` locations; preserve that when adding error paths.
- Public CLI errors are prefixed through `public_error.ml`; add or reuse a stable catalog code when
  exposing a new user-visible error family, and keep `protoss explain --list` useful.
- When checking a box in `protoss-spec.md`, add a local `Preuves:` block or section-level proof
  marker and run `dune exec protoss -- spec check protoss-spec.md`.
- `examples/` holds `.protoss` fixtures used by tests; `patches/` holds JSON patch fixtures.
- Editor support (syntax highlighting, go-to-definition) lives in `editors/cursor/protoss-syntax`.
