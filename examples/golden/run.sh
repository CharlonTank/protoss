#!/usr/bin/env bash
# Golden projects validation harness (V1.0 goal G2).
#
# Validates every project under examples/golden/ with the real protoss binary:
#   - the six well-formed projects must check/build with exit 0;
#   - capability-denied-demo must FAIL with the public code CAP001.
#
# Usage (run from the repository root):
#   examples/golden/run.sh                       # uses `dune exec protoss --`
#   examples/golden/run.sh path/to/main.exe      # uses an already-built binary
#   examples/golden/run.sh "dune exec protoss --"
#
# The script is deterministic: it rebuilds each project's git-ignored
# .protoss/store from source so a patch can be checked, and removes those
# stores again on exit so nothing is left behind for the test suite to inherit.

set -u

# --- Resolve the repository root (parent of examples/) -----------------------
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/../.." && pwd)"
cd "${REPO_ROOT}"

# --- Resolve the protoss invocation ------------------------------------------
# Default to `dune exec protoss --`; if an argument is given, use it verbatim
# (either a path to a binary, or a full command string).
if [ "$#" -ge 1 ] && [ -n "${1}" ]; then
  # shellcheck disable=SC2206
  PROTOSS=( ${1} )
else
  PROTOSS=( dune exec protoss -- )
fi

GOLDEN_DIR="examples/golden"
FAILURES=0
RAN=0

run_protoss() {
  "${PROTOSS[@]}" "$@"
}

# Remove the git-ignored build stores this script creates.
cleanup_stores() {
  find "${GOLDEN_DIR}" -name .protoss -type d -exec rm -rf {} + 2>/dev/null || true
}
trap cleanup_stores EXIT

# expect_success "<label>" <protoss args...>
expect_success() {
  local label="$1"; shift
  RAN=$((RAN + 1))
  echo "--- ${label}: protoss $* (expect exit 0)"
  local out status
  out="$(run_protoss "$@" 2>&1)"
  status=$?
  echo "${out}"
  if [ "${status}" -ne 0 ]; then
    echo "FAIL ${label}: expected exit 0, got ${status}"
    FAILURES=$((FAILURES + 1))
  else
    echo "ok   ${label} (exit 0)"
  fi
  echo
}

# expect_failure_code "<label>" "<EXPECTED_CODE>" <protoss args...>
# Asserts a non-zero exit AND that the expected public error code appears.
expect_failure_code() {
  local label="$1"; shift
  local code="$1"; shift
  RAN=$((RAN + 1))
  echo "--- ${label}: protoss $* (expect failure, code ${code})"
  local out status
  out="$(run_protoss "$@" 2>&1)"
  status=$?
  echo "${out}"
  if [ "${status}" -eq 0 ]; then
    echo "FAIL ${label}: expected non-zero exit, got 0"
    FAILURES=$((FAILURES + 1))
  elif ! printf '%s' "${out}" | grep -q "${code}"; then
    echo "FAIL ${label}: expected error code ${code} in output"
    FAILURES=$((FAILURES + 1))
  else
    echo "ok   ${label} (exit ${status}, code ${code})"
  fi
  echo
}

echo "== Golden projects validation =="
echo "repo root: ${REPO_ROOT}"
echo "protoss:   ${PROTOSS[*]}"
echo

# Start from clean build stores so patch-demo's store is reproducible.
cleanup_stores

# 1. hello-world: smallest project that checks and evaluates.
expect_success "hello-world check" \
  project check "${GOLDEN_DIR}/hello-world"
expect_success "hello-world build" \
  project build "${GOLDEN_DIR}/hello-world"

# 2. pure-library: pure, typed, effect-free definitions.
expect_success "pure-library check" \
  project check "${GOLDEN_DIR}/pure-library"
expect_success "pure-library build" \
  project build "${GOLDEN_DIR}/pure-library"

# 3. process-clock: Process using the Clock.read capability.
expect_success "process-clock check" \
  project check "${GOLDEN_DIR}/process-clock"
expect_success "process-clock build" \
  project build "${GOLDEN_DIR}/process-clock"

# 4. human-ask: Process using the Human.ask capability.
expect_success "human-ask check" \
  project check "${GOLDEN_DIR}/human-ask"
expect_success "human-ask build" \
  project build "${GOLDEN_DIR}/human-ask"

# 5. migration-demo: record schema migration (field added).
expect_success "migration-demo check" \
  project check "${GOLDEN_DIR}/migration-demo"
expect_success "migration-demo build" \
  project build "${GOLDEN_DIR}/migration-demo"

# 6. capability-denied-demo: MUST fail at check with CAP001.
#    The isolated `check` surfaces the exact CAP001 code (the workspace path
#    wraps the same failure as WORKSPACE001).
expect_failure_code "capability-denied-demo check" "CAP001" \
  check "${GOLDEN_DIR}/capability-denied-demo/src/main.protoss"

# 7. patch-demo: build the store, then validate a structured JSON patch.
expect_success "patch-demo build" \
  project build "${GOLDEN_DIR}/patch-demo"
expect_success "patch-demo patch review" \
  patch review "${GOLDEN_DIR}/patch-demo/patches/add_total.json"
expect_success "patch-demo patch check" \
  patch check "${GOLDEN_DIR}/patch-demo/.protoss/store" \
              "${GOLDEN_DIR}/patch-demo/patches/add_total.json"

# --- Summary -----------------------------------------------------------------
echo "== Summary =="
echo "checks run: ${RAN}"
if [ "${FAILURES}" -ne 0 ]; then
  echo "RESULT: FAIL (${FAILURES} failing check(s))"
  exit 1
fi
echo "RESULT: PASS (all golden projects behaved as expected)"
exit 0
