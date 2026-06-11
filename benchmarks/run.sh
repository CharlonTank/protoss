#!/usr/bin/env bash
# benchmarks/run.sh — Protoss V1.0 official benchmark suite (goal G12).
#
# Runs each benchmark scenario and prints REPRODUCIBLE wall-clock measurements
# (median of N repetitions) plus the content-addressed artifact each command
# produces. Designed to be run FROM THE REPOSITORY ROOT:
#
#     bash benchmarks/run.sh                       # uses `dune exec protoss --`
#     bash benchmarks/run.sh _build/default/bin/main.exe   # use a prebuilt binary (faster, no dune)
#     BENCH_REPS=10 bash benchmarks/run.sh <bin>   # more reps for tighter medians
#
# Determinism contract:
#   * The Protoss ARTIFACTS measured here (hashes, graphs, reports, web bundles)
#     are content-addressed and carry NO timestamps — re-running yields identical
#     hashes (the script asserts a few of them are stable across runs).
#   * The TIMINGS are wall-clock and therefore machine/run dependent. They are
#     printed to stdout for humans / CI dashboards and are NEVER written into a
#     hashed artifact. `protoss bench build` does embed a `seconds=` line in its
#     report, but that report's *hash* is derived only from kind/subject/build-id
#     stats — see lib/benchmark.ml report_ref — so timing never perturbs the ref.
#
# Two classes of benchmark (see benchmarks/README.md for the full table):
#   * PURE kernel ops on self-contained inputs (no prelude): the "fast floor".
#   * PRELUDE-bearing ops (single-file check/nf, and full workspace/web/patch
#     /harness builds): the realistic, heavier workloads where regressions show.

set -u

# ---- binary under test -------------------------------------------------------
# First positional arg is the binary; default to `dune exec protoss --`.
if [ "$#" -ge 1 ] && [ -n "${1:-}" ]; then
  BIN_ARGS=("$1")
else
  BIN_ARGS=(dune exec protoss --)
fi

# ---- repo root ---------------------------------------------------------------
# Resolve repo root from this script's location so the suite is CWD-independent
# for locating inputs, while commands still run with repo root as CWD (relative
# import/store paths in the fixtures assume that).
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
cd "$ROOT" || exit 1

REPS="${BENCH_REPS:-7}"
IN="benchmarks/inputs"
PROJ="benchmarks/projects"

# ---- timing helper -----------------------------------------------------------
# run_bench <label> <prep|-> <cmd...>
#   Repeats <cmd> REPS times, prints the median wall-clock ms.
#   <prep> (or "-") is a shell snippet run before EACH repetition (e.g. to wipe a
#   store for a cold build); use "-" for none.
run_bench() {
  local label="$1"; shift
  local prep="$1"; shift
  python3 - "$label" "$REPS" "$prep" "$@" <<'PY'
import sys, time, subprocess
label, reps, prep = sys.argv[1], int(sys.argv[2]), sys.argv[3]
cmd = sys.argv[4:]
ts = []
ok = True
for _ in range(reps):
    if prep and prep != "-":
        subprocess.run(prep, shell=True, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
    s = time.perf_counter()
    r = subprocess.run(cmd, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
    ts.append((time.perf_counter() - s) * 1000.0)
    ok = ok and (r.returncode == 0)
ts.sort()
med = ts[len(ts)//2]
status = "ok" if ok else "FAILED(rc!=0)"
print(f"  {label:<42s} median={med:8.1f} ms  min={ts[0]:7.1f}  max={ts[-1]:7.1f}  reps={reps}  [{status}]")
PY
}

# run_quiet <cmd...> : run once, swallow output (for warming a store / asserting it works)
run_quiet() { "$@" >/dev/null 2>&1; }

# assert_stable_hash <label> <cmd...> : run the hashing cmd twice, confirm identical
#   stdout (determinism guard for the content-addressed artifact).
assert_stable_hash() {
  local label="$1"; shift
  local a b
  a="$("$@" 2>/dev/null)"
  b="$("$@" 2>/dev/null)"
  if [ "$a" = "$b" ] && [ -n "$a" ]; then
    echo "  [determinism OK] $label -> $a"
  else
    echo "  [determinism FAIL] $label : '$a' != '$b'"
  fi
}

echo "==================================================================="
echo " Protoss V1.0 benchmark suite"
echo " binary : ${BIN_ARGS[*]}"
echo " root   : $ROOT"
echo " reps   : $REPS (median reported; set BENCH_REPS to change)"
echo "==================================================================="

echo
echo "--- [A] PURE kernel pipeline (no prelude; the fast floor) ----------"
run_bench "parse  small_pure"   - "${BIN_ARGS[@]}" parse "$IN/small_pure.protoss"
run_bench "parse  medium_pure"  - "${BIN_ARGS[@]}" parse "$IN/medium_pure.protoss"
run_bench "check  small_pure"   - "${BIN_ARGS[@]}" check "$IN/small_pure.protoss"
run_bench "check  medium_pure"  - "${BIN_ARGS[@]}" check "$IN/medium_pure.protoss"
run_bench "hash   small_pure"   - "${BIN_ARGS[@]}" hash  "$IN/small_pure.protoss"
run_bench "hash   medium_pure"  - "${BIN_ARGS[@]}" hash  "$IN/medium_pure.protoss"
run_bench "nf     small_pure"   - "${BIN_ARGS[@]}" nf    "$IN/small_pure.protoss"
run_bench "nf     medium_pure"  - "${BIN_ARGS[@]}" nf    "$IN/medium_pure.protoss"

echo
echo "--- [B] PRELUDE-bearing single-file ops (realistic typecheck/nf) ---"
run_bench "check  medium_prelude" - "${BIN_ARGS[@]}" check "$IN/medium_prelude.protoss"
run_bench "hash   medium_prelude" - "${BIN_ARGS[@]}" hash  "$IN/medium_prelude.protoss"
run_bench "nf     medium_prelude" - "${BIN_ARGS[@]}" nf    "$IN/medium_prelude.protoss"

echo
echo "--- [C] Workspace / harness (no prelude; stdlib=none) -------------"
run_quiet "${BIN_ARGS[@]}" project build "$PROJ/harness_min"
run_bench "build  harness_min (warm)" - "${BIN_ARGS[@]}" project build "$PROJ/harness_min"
run_bench "build  harness_min (cold)" "rm -rf $PROJ/harness_min/.protoss" "${BIN_ARGS[@]}" project build "$PROJ/harness_min"
run_quiet "${BIN_ARGS[@]}" project build "$PROJ/harness_min"
run_bench "harness run harness_min" - "${BIN_ARGS[@]}" harness run "$PROJ/harness_min" "$PROJ/harness_min/harness/smoke.pth"

echo
echo "--- [D] Web build / patch (full prelude; the HEAVY workloads) -----"
run_bench "app check web_min" - "${BIN_ARGS[@]}" app check "$PROJ/web_min"
run_bench "build web_min --target web (cold)" "rm -rf $PROJ/web_min/.protoss" "${BIN_ARGS[@]}" project build "$PROJ/web_min" --target web
run_quiet "${BIN_ARGS[@]}" project build "$PROJ/web_min" --target web
run_bench "build web_min --target web (warm)" - "${BIN_ARGS[@]}" project build "$PROJ/web_min" --target web
run_bench "patch check web_min" - "${BIN_ARGS[@]}" patch check "$PROJ/web_min/.protoss/store" "$PROJ/web_min/patches/change_button_text.json"

echo
echo "--- [E] determinism guards (content-addressed artifacts are stable) -"
assert_stable_hash "hash medium_pure"    "${BIN_ARGS[@]}" hash "$IN/medium_pure.protoss"
assert_stable_hash "hash medium_prelude" "${BIN_ARGS[@]}" hash "$IN/medium_prelude.protoss"

echo
echo "Done. Timings above are wall-clock (NOT embedded in any hashed artifact)."
