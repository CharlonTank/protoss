# Harnesses

A harness is a `.pth` file of executable examples, assertions, and properties attached to
a project. `protoss harness run` evaluates them against a built program and returns a
deterministic JSON pass/fail report. Harnesses are content-addressed: each gets a
`HarnessId = H(canonicalBytes(harness))`, and a project build stores a canonical harness
graph linked into the `UniverseRoot`. This page is verified against the build.

## The `.pth` format

`lib/harness.ml` owns parsing, canonical bytes, `HarnessId` derivation, and the runner.
A harness file is one entry per line. `examples/harness_project/harness/smoke.pth`:

```
harness twoExample = example two
harness twoUnit = unit two == 2
harness labelUnit = unit label == "ok"
harness generatedProperty = property isStable with sample
harness sampleGenerator = generator sample
harness twoBenchmark = benchmark two
harness invariantOk = invariant invariantOk == true
```

### Entry kinds

| Form | Meaning |
|---|---|
| `harness name = example def` | evaluate `def`, report its value |
| `harness name = unit def == expected` | assert `def` normalizes to `expected` |
| `harness name = property prop [with generator]` | generated property check |
| `harness name = generator gen` | a value generator (feeds properties) |
| `harness name = benchmark def` | a benchmark entry |
| `harness name = invariant inv == true` | an invariant assertion |

The format also supports `migration`, `scenario`, `security`, `diagnostic`, and
`ai-eval` entries (see the repo `README.md`).

## Running a harness

`protoss harness run <project-or-store> <harness.pth>` runs against a project build (or a
store's `program.graph.json`). Build first, then run:

```sh
_build/default/bin/main.exe project build <project>
_build/default/bin/main.exe harness run <project> <project>/harness/smoke.pth
```

For `examples/harness_project` the report is (formatted here for readability; the actual
output is one JSON object):

```json
{
  "format": "protoss-harness-v1",
  "source": ".../harness/smoke.pth",
  "programHash": "p2:ca99ccf9...",
  "status": "pass",
  "harnessCount": 7,
  "harnesses": [
    { "name": "twoExample",        "harnessId": "p2:1dcf990f...", "kind": "example",   "passed": true, "actual": "2",      "expected": "",     "diagnostic": "" },
    { "name": "twoUnit",           "harnessId": "p2:fd221864...", "kind": "unit",      "passed": true, "actual": "2",      "expected": "2",    "diagnostic": "" },
    { "name": "labelUnit",         "harnessId": "p2:56df7b26...", "kind": "unit",      "passed": true, "actual": "\"ok\"", "expected": "\"ok\"", "diagnostic": "" },
    { "name": "generatedProperty", "harnessId": "p2:3445242a...", "kind": "property",  "passed": true, "actual": "true",   "expected": "true", "diagnostic": "generator=sample sample=2" },
    { "name": "sampleGenerator",   "harnessId": "p2:b3196e05...", "kind": "generator", "passed": true, "actual": "2",      "expected": "",     "diagnostic": "" },
    { "name": "twoBenchmark",      "harnessId": "p2:5198dcff...", "kind": "benchmark", "passed": true, "actual": "2",      "expected": "",     "diagnostic": "" },
    { "name": "invariantOk",       "harnessId": "p2:550d6c06...", "kind": "invariant", "passed": true, "actual": "true",   "expected": "true", "diagnostic": "" }
  ]
}
```

Each entry carries its `harnessId`, `kind`, pass/fail, the `actual` value, the `expected`
value (for assertions), and a `diagnostic`. The top-level `status` is `pass` only if
every entry passed.

## Harnesses in the store and UniverseRoot

A project build writes a canonical `protoss-harness-graph-v1` to
`.protoss/store/harness.graph.json` and links its hash into the `UniverseRoot`. Package
and universe harness refs use the canonical `HarnessId`, not the raw file hash, so a
harness's identity is stable across moves.

## Harnesses gate agent commits

`protoss agent commit` is intentionally stricter than `protoss patch apply`: it
**requires at least one `--harness` file and rejects failing harness reports before
mutating the store**. This is the agent mutation contract (see [patches.md](patches.md)
and `docs/mcp.md`).

```sh
_build/default/bin/main.exe agent commit <store> <patch.json> --harness <harness.pth>
```

The commit response embeds the full harness report; a failing harness aborts the commit
with a structured error and the store is left unchanged. Verified against
`examples/harness_project` (the `add_three` patch committed with `smoke.pth`, harness
status `pass`).

## Affected-harness detection

`protoss patch` records a reserved harness surface in store diffs, so a change can be
related to the harnesses it affects. The `HARNESS001` (HarnessRegression) public code is
raised when a proposed change regresses an attached harness.
