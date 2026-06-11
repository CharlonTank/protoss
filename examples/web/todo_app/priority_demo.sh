#!/usr/bin/env bash
# Todo-app "add priority" demo (V1.0 checklist goal G7, spec section 14.4).
#
# Replays, end to end, the structured-patch scenario that evolves the
# full-stack todo app (examples/web/todo_app) to carry a per-item priority
# WITHOUT editing src/app.protoss: the change is delivered as the structured
# JSON patch patches/add_priority.json and applied to the content-addressed
# store.
#
# Scenario (one atomic 5-op batch over a freshly built store):
#   build -> patch review -> patch check -> patch apply -> patch audit
#         -> store re-listing -> eval (concrete priority) -> project audit
#         -> graph invariants
#
# The patch:
#   op 1 MigrateType migrate_v1_v2 : (-> v1Model v2Model)  -- pure, no Process;
#        rewrites each old item String into (Record (label String)
#        (priority (Variant (Low Unit) (High Unit)))), defaulting to Low.
#   op 2 ReplaceDef init    -- v2 Model shape (empty items list).
#   op 3 ReplaceDef update  -- AddTodo now builds an item record with High
#        priority; NewTodoChanged unchanged.
#   op 4 ReplaceDef view    -- renders (get item label) for the new item shape.
#   op 5 AddDef samplePrioritized -- migrate_v1_v2 applied to a one-item v1
#        model, so `eval` shows a concrete migrated item carrying priority.
#
# Hashes are pinned: any drift in canonical hashing, patch refs, UniverseRoot
# derivation, or evaluation output fails this script. Determinism is the point.
#
# Usage (from the repository root):
#   examples/web/todo_app/priority_demo.sh                  # _build/default/bin/main.exe
#   examples/web/todo_app/priority_demo.sh "dune exec protoss --"
#   examples/web/todo_app/priority_demo.sh path/to/main.exe
#   KEEP_STORES=1 examples/web/todo_app/priority_demo.sh    # keep .protoss for inspection
#
# The script always rebuilds the git-ignored .protoss store from source (the
# scenario is one-shot over a fresh store) and removes it on exit so the test
# suite never inherits it.

set -u

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/../../.." && pwd)"
cd "${REPO_ROOT}"

# Hermetic: never intern objects into the user-level global store.
export PROTOSS_GLOBAL_STORE=

if [ "$#" -ge 1 ] && [ -n "${1}" ]; then
  # shellcheck disable=SC2206
  PROTOSS=( ${1} )
else
  PROTOSS=( _build/default/bin/main.exe )
fi

APP="examples/web/todo_app"
STORE="${APP}/.protoss/store"
PATCH="${APP}/patches/add_priority.json"
# Post-patch content-addressed graph object (deterministic).
POST_GRAPH="p2:4e944b11f190fe853cdf858ffcf7164469ce65c8ebce3648937cb092d824cb3c"

FAILURES=0
RAN=0

run_protoss() { "${PROTOSS[@]}" "$@"; }

cleanup_stores() {
  if [ "${KEEP_STORES:-0}" != "1" ]; then
    rm -rf "${APP}/.protoss"
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

echo "== Todo-app add-priority demo (G7 / spec 14.4) =="
echo "repo root: ${REPO_ROOT}"
echo "protoss:   ${PROTOSS[*]}"
echo

# Fresh store from source (scenario is one-shot).
rm -rf "${APP}/.protoss"

# 1. Build the v1 store.
expect_ok "build v1 store" \
  "Build p2:35cfd3950b9b8864d0ee9c1daeb6aa593f224e360ca53712232e32bb8c6abdfd" \
  project build "${APP}"

# 2. Human-readable patch review (5 ops, priority migration + sample).
expect_ok "patch review lists MigrateType" "op 1: MigrateType" \
  patch review "${PATCH}"
expect_ok "patch review lists samplePrioritized AddDef" "name: samplePrioritized" \
  patch review "${PATCH}"

# 3. Validate the patch against the v1 store (no mutation).
expect_ok "patch check valid" \
  "Patch valid p2:db39d091ac0c6ac25a98b6a5ceb56d7f8ed657807a1ffaf4bd23f37a1c033f28" \
  patch check "${STORE}" "${PATCH}"

# 4. Apply the patch (atomic 5-op batch, audited).
expect_ok "patch apply accepted" \
  "Patch accepted p2:e4e82dbdb06a99ec0befc2804d53c70c6d0dfce1d10b43f7576f097e21d6aaae" \
  patch apply "${STORE}" "${PATCH}"

# 5. Verify the audit chain (5 ops; previous-root pinned to the v1 UniverseRoot).
expect_ok "patch audit OK" \
  "Patch audit OK p2:e4e82dbdb06a99ec0befc2804d53c70c6d0dfce1d10b43f7576f097e21d6aaae" \
  patch audit "${STORE}"
expect_ok "patch audit records 5 ops" "ops=5" \
  patch audit "${STORE}"
expect_ok "patch audit chains to v1 root" \
  "previous-root=p2:e41f9c7a315097e63c0f1451d99a6864d864c4938bedd475c27c030626e9215c" \
  patch audit "${STORE}"

# 6. Store now carries the evolved Model type (priority on each item).
expect_ok "store lists v2 init with priority field" \
  "(priority (Variant (High Unit) (Low Unit))" \
  store list "${APP}"
expect_ok "migrate_v1_v2 present with declared dep" \
  "migrate_v1_v2 p2:04a95fa6638dacef9063d28a166e01c4ad73f076d8f0f2da1e25b0bf2cfaa880" \
  store list "${APP}"

# 7. Eval proves priority concretely from the post-patch content-addressed graph.
expect_ok "eval samplePrioritized shows concrete priority" \
  'samplePrioritized = {draft = "", items = [{label = "buy milk", priority = Low unit}], next = 1}' \
  eval --store-graph "${APP}" "${POST_GRAPH}" --entry samplePrioritized
expect_ok "eval init produces v2 Model (empty items)" \
  'init = Done {draft = "", items = [], next = 0}' \
  eval --store-graph "${APP}" "${POST_GRAPH}" --entry init

# 8. Whole-program revalidation and graph invariants still hold post-patch.
expect_ok "project audit after apply" "Audit OK" \
  audit "${APP}"
expect_ok "graph invariants after apply" "graph_migration=ok" \
  invariants graph --store-graph "${APP}" "${POST_GRAPH}"

# 9. Atomicity contract: re-applying the same batch on the patched store fails
#    (samplePrioritized AddDef target already exists).
expect_fail "duplicate apply rejected (PATCH001)" \
  "AddDef target already exists: samplePrioritized" \
  patch check "${STORE}" "${PATCH}"

echo
echo "== Summary =="
echo "checks run: ${RAN}"
if [ "${FAILURES}" -ne 0 ]; then
  echo "RESULT: FAIL (${FAILURES} failing check(s))"
  exit 1
fi
echo "RESULT: PASS (todo app evolved to carry priority via structured patch)"
exit 0
