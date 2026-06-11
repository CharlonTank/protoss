#!/usr/bin/env bash
# Protoss V1.0 final gate (goal G14).
#
# Runs, in order, the full V1.0 acceptance from a clean checkout. Every step
# must pass; the script exits non-zero on the first failure. This is the single
# command that answers "is this tree V1.0-shippable by the proofs we have?".
#
# Usage (from the repository root):
#   scripts/v1-gate.sh
#
# It is hermetic: PROTOSS_GLOBAL_STORE is emptied so nothing touches the
# user-level global store, and generated .protoss stores are cleaned at the end.

set -u

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"
cd "${REPO_ROOT}"
export PROTOSS_GLOBAL_STORE=""

PROTOSS=(dune exec protoss --)
fail=0

step() {
  local name="$1"; shift
  echo "=== ${name} ==="
  if "$@"; then
    echo "[ok] ${name}"
  else
    echo "[FAIL] ${name}"
    fail=1
  fi
}

cleanup() {
  find examples benchmarks -name .protoss -type d -exec rm -rf {} + 2>/dev/null || true
}
trap cleanup EXIT

# 1. The tree compiles.
step "build" dune build

# 2. The executable V1.0 acceptance proofs.
step "doctor --v1" "${PROTOSS[@]}" doctor --v1

# 3. The spec audit: every checked claim carries evidence.
step "spec check" "${PROTOSS[@]}" spec check protoss-spec.md

# 4. Golden projects validate from a clean state.
step "golden projects" examples/golden/run.sh "${PROTOSS[*]}"

# 5. The full-stack priority patch demo.
step "priority demo" examples/web/todo_app/priority_demo.sh "${PROTOSS[*]}"

# 6. The whole conformance suite.
step "fulltest" dune build @fulltest --force

# 7. The CLI actually installs and runs OUTSIDE a checkout, with no
#    PROTOSS_STDLIB: a "V1" you can only run via `dune exec` is not shipped.
#    Hermetic: installs to a throwaway prefix, then from a temp dir that is not
#    under the repo it scaffolds the default full-stack app and `app check`s it.
#    Both the binary and the prelude must come from the installed layout
#    (<prefix>/bin/protoss, <prefix>/share/protoss/prelude.protoss).
install_smoke() {
  local prefix demo ok=0
  prefix="$(mktemp -d)"; demo="$(mktemp -d)"
  if dune build @install >/dev/null 2>&1 \
     && dune install --prefix "${prefix}" protoss >/dev/null 2>&1 \
     && ( cd "${demo}" && env -u PROTOSS_STDLIB "${prefix}/bin/protoss" project init app >/dev/null ) \
     && ( cd "${demo}" && env -u PROTOSS_STDLIB "${prefix}/bin/protoss" app check app ) \
          | grep -q 'App OK'; then
    ok=0
  else
    ok=1
  fi
  rm -rf "${prefix}" "${demo}"
  return "${ok}"
}
step "install smoke (scaffold + check a full-stack app outside a checkout)" install_smoke

echo
if [ "${fail}" -eq 0 ]; then
  echo "V1.0 gate: PASS (every available proof is green)"
  echo "Note: the doctor reports the §17 self-hosted parities (canonicalizer /"
  echo "patch-validator / normalizer) as not-yet; each is proven by an @selftest"
  echo "sweep (run under fulltest above), not duplicated in the doctor."
  exit 0
else
  echo "V1.0 gate: FAIL"
  exit 1
fi
