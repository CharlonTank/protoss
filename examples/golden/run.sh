#!/usr/bin/env bash
# Golden projects validation harness (V1.0 goal G2).
#
# Replays, for each project under examples/golden/, the exact scenario
# documented in its VALIDATE.md:
#   - hello-world            check/build/audit/eval
#   - pure-library           check/build/lock/package/interface/eval
#   - process-clock          check/build/capabilities/run (suspends ReadClock)
#   - human-ask              check/build/capabilities/typed resume invariant
#   - capability-denied-demo MUST fail: CAP001 (file) / WORKSPACE001 (project)
#   - patch-demo             build -> patch review/check/apply -> audit -> re-verify
#   - migration-demo         build -> migration patch check/apply -> audit -> re-verify
#
# Golden hashes are pinned: any drift in canonical hashing, UniverseRoot
# derivation, patch refs, or evaluation output fails this script. That is the
# point — determinism is the central invariant.
#
# Usage (from the repository root):
#   examples/golden/run.sh                  # uses _build/default/bin/main.exe
#   examples/golden/run.sh "dune exec protoss --"
#   examples/golden/run.sh path/to/main.exe
#   KEEP_STORES=1 examples/golden/run.sh    # keep .protoss stores for inspection
#
# The script always rebuilds each project's git-ignored .protoss store from
# source (scenarios are one-shot over fresh stores) and removes those stores
# on exit so the test suite never inherits them.

set -u

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/../.." && pwd)"
cd "${REPO_ROOT}"

# Hermetic: never intern objects into the user-level global store.
export PROTOSS_GLOBAL_STORE=

if [ "$#" -ge 1 ] && [ -n "${1}" ]; then
  # shellcheck disable=SC2206
  PROTOSS=( ${1} )
else
  PROTOSS=( _build/default/bin/main.exe )
fi

G="examples/golden"
FAILURES=0
RAN=0

run_protoss() { "${PROTOSS[@]}" "$@"; }

cleanup_stores() {
  if [ "${KEEP_STORES:-0}" != "1" ]; then
    find "${G}" -name .protoss -type d -prune -exec rm -rf {} + 2>/dev/null || true
  fi
}
trap cleanup_stores EXIT

# expect_ok "<label>" "<required substring or empty>" <protoss args...>
expect_ok() {
  local label="$1" needle="$2"; shift 2
  RAN=$((RAN + 1))
  local out status
  out="$(run_protoss "$@" 2>&1)"; status=$?
  if [ "${status}" -ne 0 ]; then
    echo "FAIL ${label}: expected exit 0, got ${status}"
    printf '%s\n' "${out}" | sed 's/^/  | /'
    FAILURES=$((FAILURES + 1)); return
  fi
  if [ -n "${needle}" ] && ! printf '%s' "${out}" | grep -qF -- "${needle}"; then
    echo "FAIL ${label}: missing expected output: ${needle}"
    printf '%s\n' "${out}" | sed 's/^/  | /'
    FAILURES=$((FAILURES + 1)); return
  fi
  echo "ok   ${label}"
}

# expect_fail "<label>" "<required substring>" <protoss args...>
expect_fail() {
  local label="$1" needle="$2"; shift 2
  RAN=$((RAN + 1))
  local out status
  out="$(run_protoss "$@" 2>&1)"; status=$?
  if [ "${status}" -eq 0 ]; then
    echo "FAIL ${label}: expected non-zero exit, got 0"
    printf '%s\n' "${out}" | sed 's/^/  | /'
    FAILURES=$((FAILURES + 1)); return
  fi
  if ! printf '%s' "${out}" | grep -qF -- "${needle}"; then
    echo "FAIL ${label}: expected error output: ${needle}"
    printf '%s\n' "${out}" | sed 's/^/  | /'
    FAILURES=$((FAILURES + 1)); return
  fi
  echo "ok   ${label} (failed as expected)"
}

reset_store() { rm -rf "${G}/$1/.protoss"; }

echo "== Golden projects validation =="
echo "repo root: ${REPO_ROOT}"
echo "protoss:   ${PROTOSS[*]}"
echo

# ---------------------------------------------------------------- hello-world
reset_store hello-world
expect_ok "hello-world project check" "Project OK golden-hello-world" \
  project check "${G}/hello-world"
expect_ok "hello-world project build" \
  "Build p2:35fdec2f5537ec599157a5aeb7e56ffa6331469fe538f9d76207ecc91105da67" \
  project build "${G}/hello-world"
expect_ok "hello-world audit" "Audit OK" \
  audit "${G}/hello-world"
expect_ok "hello-world eval main" 'main = "hello, world"' \
  eval "${G}/hello-world/src/main.protoss" --entry main

# --------------------------------------------------------------- pure-library
reset_store pure-library
expect_ok "pure-library project check" "Project OK golden-pure-library" \
  project check "${G}/pure-library"
expect_ok "pure-library project build" \
  "Build p2:a84b0d16255d9e70d4757f74758cbb1cae80b3ed72ad660072dafda723e0841a" \
  project build "${G}/pure-library"
expect_ok "pure-library project lock" \
  "Lock p2:ef1542a8a37c10d05a5b01314b95c314a313cc022ca97f9d5c1e33b3192c0b4c" \
  project lock "${G}/pure-library"
expect_ok "pure-library lock --check" \
  "Lock OK p2:ef1542a8a37c10d05a5b01314b95c314a313cc022ca97f9d5c1e33b3192c0b4c" \
  project lock "${G}/pure-library" --check
expect_ok "pure-library project package" \
  "Package p2:6e91e02b9cbadc5a079e74dd692ba47f602f34e14ae147d12c2a976a17e1dac4" \
  project package "${G}/pure-library"
expect_ok "pure-library package --check" \
  "Package OK p2:6e91e02b9cbadc5a079e74dd692ba47f602f34e14ae147d12c2a976a17e1dac4" \
  project package "${G}/pure-library" --check
expect_ok "pure-library interface" "exports=15" \
  project interface "${G}/pure-library"
expect_ok "pure-library eval seven" "seven = 7" \
  eval "${G}/pure-library/src/lib.protoss" --entry seven

# -------------------------------------------------------------- process-clock
reset_store process-clock
expect_ok "process-clock project check" "Project OK golden-process-clock" \
  project check "${G}/process-clock"
expect_ok "process-clock project build" \
  "Build p2:f72d69db64f578b2195204012166581b5b85283e3d7aa1b0bc5b1558b59f435b" \
  project build "${G}/process-clock"
expect_ok "process-clock capabilities" "program-caps=[Clock.read]" \
  capabilities --project "${G}/process-clock"
expect_ok "process-clock run suspends ReadClock" "Request ReadClock" \
  run "${G}/process-clock/src/main.protoss" --entry now

# ------------------------------------------------------------------ human-ask
reset_store human-ask
expect_ok "human-ask project check" "Project OK golden-human-ask" \
  project check "${G}/human-ask"
expect_ok "human-ask project build" \
  "Build p2:5d26eb91830e9fffb556ab46cfbbbab37d82821df7bb42ca7b13d08ccc0a567c" \
  project build "${G}/human-ask"
expect_ok "human-ask capabilities" "program-caps=[Human.ask]" \
  capabilities --project "${G}/human-ask"
expect_ok "human-ask typed resume invariant" 'result=Done "Ada"' \
  invariants process "${G}/human-ask/src/main.protoss" --entry askName --response String:Ada

# ------------------------------------------------- capability-denied-demo (FAIL)
reset_store capability-denied-demo
expect_fail "capability-denied check (CAP001)" \
  "CAP001" \
  check "${G}/capability-denied-demo/src/main.protoss"
expect_fail "capability-denied check names the capability" \
  "missing capability: Http.get" \
  check "${G}/capability-denied-demo/src/main.protoss"
expect_fail "capability-denied project check (WORKSPACE001 wrapper)" \
  "missing capability: Http.get" \
  project check "${G}/capability-denied-demo"
expect_fail "capability-denied project build refuses" \
  "missing capability: Http.get" \
  project build "${G}/capability-denied-demo"
# The CLI may scaffold empty store directories before the check fails, but it
# must not write any store content (no canonical defs, objects, roots, ...).
RAN=$((RAN + 1))
if [ -n "$(find "${G}/capability-denied-demo/.protoss" -type f 2>/dev/null | head -1)" ]; then
  echo "FAIL capability-denied must not write store content"
  FAILURES=$((FAILURES + 1))
else
  echo "ok   capability-denied wrote no store content"
fi

# ----------------------------------------------------------------- patch-demo
reset_store patch-demo
expect_ok "patch-demo project build" \
  "Build p2:11010968b8570735a85dbcb4fc55073c34141cf37fbcef943a6731d4c007f59b" \
  project build "${G}/patch-demo"
expect_ok "patch-demo patch review" "op 1: AddDef" \
  patch review "${G}/patch-demo/patches/add_total.json"
expect_ok "patch-demo patch check" \
  "Patch valid p2:bb87817ece787c88ae2b9578ba95790ecd3e9f2dd0a8d2e78f357550cc50f6a1" \
  patch check "${G}/patch-demo/.protoss/store" "${G}/patch-demo/patches/add_total.json"
expect_ok "patch-demo patch apply" \
  "Patch accepted p2:2f1183f04d08a51b3934168b9478cf1a202b1e1aaf5a986f1ee502daac76d0a0" \
  patch apply "${G}/patch-demo/.protoss/store" "${G}/patch-demo/patches/add_total.json"
expect_ok "patch-demo patch audit" \
  "Patch audit OK p2:2f1183f04d08a51b3934168b9478cf1a202b1e1aaf5a986f1ee502daac76d0a0" \
  patch audit "${G}/patch-demo/.protoss/store"
expect_ok "patch-demo store lists patched def" \
  "total p2:ccd0be0b2e0a43d9230d11236070427dca21fa4540b72d7ef11c70bdb8620f78 Nat deps=[base]" \
  store list "${G}/patch-demo"
expect_ok "patch-demo eval patched def from store graph" "total = 3" \
  eval --store-graph "${G}/patch-demo" \
  p2:e2cbaf88897644a1b0f6d1d1fa463c63c66310692f476e0e4f719f3d79077e78 --entry total
expect_ok "patch-demo project audit after apply" "Audit OK" \
  audit "${G}/patch-demo"
expect_fail "patch-demo duplicate AddDef rejected (PATCH001)" \
  "AddDef target already exists: total" \
  patch check "${G}/patch-demo/.protoss/store" "${G}/patch-demo/patches/add_total.json"

# ------------------------------------------------------------- migration-demo
reset_store migration-demo
expect_ok "migration-demo project build" \
  "Build p2:a8353feddc610c058997f1afb48da4707e79b2999b75c1b6d575585a54ec1781" \
  project build "${G}/migration-demo"
expect_ok "migration-demo patch review" "op 1: MigrateType" \
  patch review "${G}/migration-demo/patches/add_done_field.json"
expect_ok "migration-demo patch check" \
  "Patch valid p2:346bf6a339048c61aaba0ab77c6d3166bf89bf1829cbe60719bb5c6baab93575" \
  patch check "${G}/migration-demo/.protoss/store" "${G}/migration-demo/patches/add_done_field.json"
expect_ok "migration-demo patch apply" \
  "Patch accepted p2:db684cd5ccafac9f466b21f32aa4298b2ffe4ad924f335cb7b36d6f6245cac1b" \
  patch apply "${G}/migration-demo/.protoss/store" "${G}/migration-demo/patches/add_done_field.json"
expect_ok "migration-demo patch audit" \
  "Patch audit OK p2:db684cd5ccafac9f466b21f32aa4298b2ffe4ad924f335cb7b36d6f6245cac1b" \
  patch audit "${G}/migration-demo/.protoss/store"
expect_ok "migration-demo store lists evolved type" \
  "initial p2:79c826272630947aa5f353c35801ac8536f8f74a62af253469344c2fcb06a1fd (Record (done Bool) (title String)) deps=[migrate_v1_v2]" \
  store list "${G}/migration-demo"
expect_ok "migration-demo eval migrated value" \
  'initial = {done = false, title = "first item"}' \
  eval --store-graph "${G}/migration-demo" \
  p2:b034ad0ad777d67574e35570cc07d032fd258860fc5f3994cff3fae4327ca735 --entry initial
expect_ok "migration-demo untouched dependent still evaluates" \
  'headline = "first item"' \
  eval --store-graph "${G}/migration-demo" \
  p2:b034ad0ad777d67574e35570cc07d032fd258860fc5f3994cff3fae4327ca735 --entry headline

# -------------------------------------------------------------------- summary
echo
echo "== Summary =="
echo "checks run: ${RAN}"
if [ "${FAILURES}" -ne 0 ]; then
  echo "RESULT: FAIL (${FAILURES} failing check(s))"
  exit 1
fi
echo "RESULT: PASS (all golden projects behaved as expected)"
exit 0
