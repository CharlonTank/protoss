# Verifying a release: `protoss doctor --v1`

`protoss doctor --v1` is the mechanical release gate. It runs the available V1.0 proofs
from the ship checklist — each tied to a checklist section — and reports per-proof
`pass` / `fail` / `not-yet`. It **fails (exit non-zero) only if an available proof
breaks**; `not-yet` sections (proofs not wired in this build) do not fail it. This page
is verified against the current build.

## Running it

```sh
_build/default/bin/main.exe doctor --v1
```

Tail of the report:

```
summary: 23 pass, 0 fail, 2 not-yet
V1.0 doctor: OK (no available proof is broken)
```

Exit code is `0`.

> **Run it from the repository root.** The doctor is largely self-sufficient (it embeds
> the sources it checks), but a few heavy proofs build real fixtures and use the default
> ledger root, so run it from the repo root. A hermetic invocation
> (`env PROTOSS_GLOBAL_STORE= ... doctor --v1`) avoids touching `$HOME/.protoss`.

## What it proves

Each line is `[PASS] <section> <id> — <description>`. The current passing set (verified):

```
[PASS] 3.1   kernel-grammar — executable kernel grammar is published and a multi-form program checks
[PASS] 3.2   hash-determinism — the same source canonicalizes to the same hash on repeated checks
[PASS] 3.2   alpha-stability — alpha-equivalent programs share one canonical hash
[PASS] 6.5   syntax-equivalence — S-expression and Elm-like sources hash identically
[PASS] 6.3   ptc-roundtrip — Protoss/C serialize -> parse -> hash is stable
[PASS] 6.4   ptb-roundtrip — Protoss/B encode -> decode is deterministic and hash-stable
[PASS] 6.2   human-projection — Protoss/H render -> parse preserves the canonical hash
[PASS] 3.3   totality-rejects-general-recursion — general recursion rejected, structural accepted
[PASS] 4     typecheck-rejects-ill-typed — an ill-typed definition is rejected
[PASS] 9     capability-enforcement — an undeclared effect is rejected, a declared one accepted
[PASS] 22    spec-audit — protoss-spec.md checked claims all carry evidence markers
[PASS] 10.1  mcp-contract — MCP server speaks JSON-RPC, exposes core tools, rejects bad calls
[PASS] 5.3   store-universe-root — a real build writes a UniverseRoot and the project audits clean
[PASS] 8.3   ledger-replay — a Process suspends on a request and the ledger resumes it deterministically
[PASS] 10.3  patch-check-audit — a structured patch is checked, applied, and its audit chain verifies
[PASS] 11    harness — a passing harness reports pass and a failing one reports fail
[PASS] 12    packages-lock-registries — a built package locks deterministically and check_package agrees
[PASS] 19    golden-projects — golden projects build, and capability-denied is rejected
[PASS] 14    priority-demo — todo app gains a per-item priority via a checked+applied structured patch
[PASS] 15    bytecode-encoding — graph compiles to bytecode with deterministic, round-trip-stable encoding
[PASS] 15    bytecode-parity — bytecode VM executes at parity with the reference interpreter
[PASS] 21    structured-errors-on-hostile-input — malformed input fails through the structured error layer
[PASS] 20    benchmarks-thresholds — a full-prelude build stays under a generous wall-clock ceiling
```

These cover, end to end, the central invariants documented across these docs: canonical
determinism and alpha-stability, cross-syntax hash equivalence, the `.ptc`/`.ptb`/graph
round-trips, totality and capability enforcement, the store/`UniverseRoot`, ledger
replay, the patch audit chain, harnesses, packaging, the bytecode VM at interpreter
parity, structured errors on hostile input, and the full-stack priority demo.

## The `not-yet` sections

Sections shown as `[ -- ]` are proofs not wired in this build. They are honest gaps, not
failures, and each names its checklist item. Currently:

```
[ -- ] 17  self-hosted-canonicalizer-parity — Protoss canonicalizer matches the kernel byte-for-byte
[ -- ] 17  self-hosted-patch-validator-parity — Protoss patch validator matches Patch.check verdicts
```

(These are the self-hosting parity sweeps — heavy prelude-evaluation proofs — slated for
goals G8/later. `protoss self canon --compare` already enforces canonicalizer parity per
file; see [self-hosting.md](self-hosting.md).)

> **Note on counts.** The exact pass/not-yet split depends on what the build can prove in
> the current environment. A full run from the repo root reports `23 pass, 0 fail, 2
> not-yet`. If you capture only the tail of an interrupted or non-hermetic run you may see
> a different split; rely on the final `summary:` line and the exit code.

## JSON output

`--json` emits a machine-readable report for CI:

```sh
_build/default/bin/main.exe doctor --v1 --json
```

Top-level keys: `status`, `summary`, `checks`.

```json
{
  "status": "ok",
  "summary": { "pass": 23, "fail": 0, "not_yet": 2 },
  "checks": [
    { "id": "kernel-grammar", "section": "3.1", "status": "pass",
      "description": "executable kernel grammar is published and a multi-form program checks",
      "detail": "" },
    ...
  ]
}
```

`status` is `ok` when no available proof is broken. Each check carries `id`, `section`,
`status` (`pass` / `fail` / `not-yet`), `description`, and `detail`.

## Failure behavior

The doctor is built to fail loud: there is a fault-injection test
(`aggregate_exit`) proving that if any *available* proof breaks, the command exits
non-zero. So a green doctor is meaningful — it is not merely the absence of run proofs.

## Where it fits in the V1.0 gate

The doctor is the spine of the final release gate (checklist §23): run `doctor --v1` from
a clean checkout, replay the golden projects (`examples/golden/run.sh`) and the priority
demo (`examples/web/todo_app/priority_demo.sh`), run `spec check`, and identify the
release by its canonical hash. See [verifying-the-docs.md](verifying-the-docs.md) for how
these same commands also keep this documentation honest.
