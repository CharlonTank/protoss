# Protoss V1.0 benchmark suite (goal G12)

Reproducible, content-addressed benchmarks covering the full pipeline: parsing,
canonicalization, typecheck, normalization (`nf`), hashing, `patch check`,
`harness run`, and web build. This directory is **inputs + a runner + this
doc**; it does not modify the compiler. The benchmark *artifacts* (hashes,
graphs, reports, web bundles) are content-addressed and timestamp-free; only the
*timings* printed by the runner are wall-clock.

## Layout

```
benchmarks/
  run.sh                         # deterministic runner (see below)
  README.md                      # this file
  inputs/
    small_pure.protoss           # tiny, NO prelude  — the "fast floor"
    medium_pure.protoss          # ~200 defs, NO prelude — kernel-only, still fast
    medium_prelude.protoss       # imports stdlib/prelude.protoss — realistic typecheck/nf
  projects/
    harness_min/                 # stdlib="none" workspace — build + harness run, no prelude cost
      protoss.toml  src/main.protoss  harness/smoke.pth
    web_min/                     # full-prelude web app (todo) — build web + patch check (HEAVY)
      protoss.toml  src/app.protoss  patches/change_button_text.json
```

Building a project writes a git-ignored `.protoss/` store under that project
(`**/.protoss/` is in `.gitignore`); the runner creates and reuses these and
they are safe to delete. Keep the checked-in tree to the files listed above.

## Running

Run **from the repository root**. The binary is the first positional arg;
default is `dune exec protoss --`.

```sh
bash benchmarks/run.sh                                # via dune (slower startup)
bash benchmarks/run.sh _build/default/bin/main.exe    # prebuilt binary (recommended)
BENCH_REPS=10 bash benchmarks/run.sh _build/default/bin/main.exe   # tighter medians
```

Each scenario is repeated `BENCH_REPS` times (default 7) and the **median**
wall-clock is reported, with min/max and an `[ok]`/`[FAILED]` flag (non-zero
exit ⇒ FAILED). Section E re-runs the hashing commands and asserts the emitted
content ref is byte-identical across runs (determinism guard).

## What `protoss bench` covers natively vs. what needs `time`

`protoss bench build <project>` (see `command_bench` in `bin/main.ml`,
`lib/benchmark.ml`) is the **only** native bench subcommand today. It:

- builds the project, measures the build with `Unix.gettimeofday`, and prints a
  `protoss-benchmark-v1` report plus a `benchmark-ref=` content hash;
- the report body contains a `seconds=%.6f` line **and** the build stats, but the
  report's *hash* (`Benchmark.report_ref`) is `H("protoss-benchmark-report-v1\n"
  + body)` — so two builds with the same stats but different wall-clock produce
  **different** report bytes yet the timing is informational only; the build
  artifact itself (`build_id`, `UniverseRoot`) is fully deterministic.

There is **no** native bench subcommand for parse / check / hash / nf / patch
check / harness run / web build. For those, the runner wraps the ordinary CLI
command in a Python `perf_counter` timer (more precise than `/usr/bin/time -p`,
which only resolves to 0.01 s). So:

| Metric source | Scenarios |
|---|---|
| **Native** (`protoss bench build`) | optional: project build seconds + content-addressed report. The suite measures `project build` via the timing wrapper instead, for a uniform median across all scenarios; `protoss bench build` remains available when you want the hashed report. |
| **`time`-style wrapper** (runner) | everything: parse, check, hash, nf (pure + prelude), `project build`, `app check`, `project build --target web`, `patch check`, `harness run`. |

## The two benchmark classes (and why timings look the way they do)

The single most important measured fact: **the trusted kernel is cheap; prelude
evaluation is what costs time.** A no-prelude `check`/`hash`/`nf` of even ~200
defs stays at the ~5 ms process-startup floor. The moment a source imports
`stdlib/prelude.protoss` (~740 defs), every `check`/`hash`/`nf` pays ~130–190 ms
to elaborate the whole prelude, and a full *workspace* build pays seconds
(canonical serialization + per-def graph-object hashing + host contract + web
bundle + store writes for the whole prelude graph).

- **PURE (no prelude):** floor guards. A regression here means cold-start or
  small-graph handling got slower.
- **PRELUDE-bearing:** the realistic workloads where typecheck / normalize /
  build-pipeline regressions actually show up.

## Benchmarks: command, metric, measured median, proposed threshold

Measured on this machine (Apple arm64, `_build/default/bin/main.exe`, medians of
3–7 reps). Thresholds are **generous ceilings** (≈ 2× the median, rounded) so a
real regression (≈ 2–3×) trips them while normal run-to-run variance does not.
They are *order-of-magnitude* gates, not microbenchmark targets.

| # | Bench | Exact command (from repo root) | Metric | Measured median | Proposed ceiling |
|---|---|---|---|---|---|
| A1 | parse small (pure) | `protoss parse benchmarks/inputs/small_pure.protoss` | wall ms | ~4.5 ms | **50 ms** |
| A2 | parse medium (pure) | `protoss parse benchmarks/inputs/medium_pure.protoss` | wall ms | ~4.6 ms | **50 ms** |
| A3 | check small (pure) | `protoss check benchmarks/inputs/small_pure.protoss` | wall ms | ~4.5 ms | **50 ms** |
| A4 | check medium (pure) | `protoss check benchmarks/inputs/medium_pure.protoss` | wall ms | ~5.4 ms | **60 ms** |
| A5 | hash medium (pure) | `protoss hash benchmarks/inputs/medium_pure.protoss` | wall ms | ~5.2 ms | **60 ms** |
| A6 | nf medium (pure) | `protoss nf benchmarks/inputs/medium_pure.protoss` | wall ms | ~5.8 ms | **60 ms** |
| B1 | check (prelude) | `protoss check benchmarks/inputs/medium_prelude.protoss` | wall ms | ~134 ms | **400 ms** |
| B2 | hash (prelude) | `protoss hash benchmarks/inputs/medium_prelude.protoss` | wall ms | ~154 ms | **450 ms** |
| B3 | nf (prelude) | `protoss nf benchmarks/inputs/medium_prelude.protoss` | wall ms | ~186 ms | **500 ms** |
| C1 | build (no prelude, warm) | `protoss project build benchmarks/projects/harness_min` | wall ms | ~5.4 ms | **80 ms** |
| C2 | build (no prelude, cold) | `rm -rf …/.protoss; protoss project build benchmarks/projects/harness_min` | wall ms | ~17 ms | **150 ms** |
| C3 | harness run (no prelude) | `protoss harness run benchmarks/projects/harness_min benchmarks/projects/harness_min/harness/smoke.pth` | wall ms | ~6.2 ms | **80 ms** |
| D1 | app check (prelude) | `protoss app check benchmarks/projects/web_min` | wall ms | ~2.15 s | **6 s** |
| D2 | build web (prelude, cold) | `rm -rf …/.protoss; protoss project build benchmarks/projects/web_min --target web` | wall ms | ~3.6 s | **8 s** |
| D3 | build web (prelude, warm) | `protoss project build benchmarks/projects/web_min --target web` | wall ms | ~2.45 s | **6 s** |
| D4 | patch check (prelude store) | `protoss patch check benchmarks/projects/web_min/.protoss/store benchmarks/projects/web_min/patches/change_button_text.json` | wall ms | ~223 ms | **600 ms** |

(Replace `protoss` with `dune exec protoss --` or `_build/default/bin/main.exe`.)

Notes on a few numbers:
- **Warm vs cold build:** separate CLI processes do **not** share the in-process
  memoization, so "warm" here means an already-populated on-disk store
  (`write_file_atomic_if_changed` skips unchanged writes); "cold" wipes
  `.protoss` first. The cold web build (~3.6 s) is the upper bound to gate.
- **`patch check` (~223 ms) ≪ `app check`/`build web` (~2–2.5 s):** `patch check`
  validates against the already-canonicalized store (cheap), whereas `app check`
  and `build web` re-elaborate from source, normalize, and (for build) emit and
  hash the whole prelude graph + web bundle.

## Determinism contract

- **Artifacts are timestamp-free and content-addressed.** Re-running any
  `hash`/`check --graph`/`project build` yields identical hashes (section E of
  the runner asserts this for the two `hash` benches).
- **Timings are wall-clock and live only in the runner's stdout.** They are never
  written into a hashed artifact. The one place the compiler records a duration —
  `protoss bench build`'s `seconds=` line — is informational; the deterministic
  build identity (`build_id`, `UniverseRoot`, web `compiledArtifact`) is computed
  independently of it.

## Wiring `benchmarks-thresholds` into `protoss doctor --v1`

`protoss doctor --v1` already lists this proof as not-yet-wired:

```
[ -- ] 20     benchmarks-thresholds — official benchmarks meet critical thresholds
        checklist §20: wired by goal G12 (benchmarks)
```

(See `lib/doctor.ml`: the `Not_yet "checklist §20: wired by goal G12 (benchmarks)"`
entry with `id = "benchmarks-thresholds"`, `section = "20"`.)

To turn it green, replace that `run` with a proof that **builds a representative
project and asserts the wall-clock is under the ceiling**. The exact pattern to
copy is the existing heavy `priority_demo` check in the same file (it
`find_up`s `examples/web/todo_app` + `stdlib/prelude.protoss`, copies to a
pid-qualified temp, `absolutize_stdlib`, builds with the full prelude, cleans
up). For the benchmark proof:

1. **Locate + stage** a full-prelude project (reuse `find_up` for
   `examples/web/todo_app` or `benchmarks/projects/web_min`, plus
   `stdlib/prelude.protoss`), copy to a pid-qualified temp dir, `absolutize_stdlib`.
2. **Measure**: `let t0 = Unix.gettimeofday () in ignore (Workspace.build manifest);
   let secs = Unix.gettimeofday () -. t0 in` (web: `Web.build root`).
3. **Assert the ceiling** — the critical one to gate is the full web build:
   `pass_if (secs < 8.0) (Printf.sprintf "web build took %.1fs (ceiling 8s)" secs)`.
   Optionally also time a cheap pure op (`check` of an embedded no-prelude source)
   and gate it at `< 0.20` s to catch a cold-start regression.
4. **Mark it heavy**: add `"benchmarks-thresholds"` to `heavy_ids` in
   `lib/doctor.ml` (alongside `"priority-demo"`), because it builds the full
   prelude. `is_heavy` already exists; heavy checks run under `protoss doctor
   --v1` but are skipped by the fast dev-loop sweep.

Concretely, the command the orchestrator's proof executes internally is the
equivalent of:

```
protoss project build <full-prelude-project> --target web
```

and the threshold it verifies is **wall-clock < 8 s for the cold web build**
(the D2 ceiling above). Use a single generous ceiling rather than the tight
per-bench numbers: a doctor proof must not flap on a busy CI box. If a tighter
signal is wanted, gate the warm build at `< 6 s` (D3) instead, but note doctor
runs each proof once from a fresh process, i.e. effectively a cold build.

**Honesty caveats the orchestrator should keep:**
- This is a *heavy* proof (full prelude build, seconds). It belongs behind
  `heavy_ids`, exactly like `priority-demo`.
- A wall-clock ceiling in a doctor proof is a coarse regression gate, not a
  reproducible measurement. The reproducible, machine-comparable numbers come
  from `benchmarks/run.sh` (medians of N reps). The doctor proof only answers
  "did something get catastrophically slower"; the suite answers "how fast is
  it." Keep both, and treat the table above as the source for the ceiling
  constants.
