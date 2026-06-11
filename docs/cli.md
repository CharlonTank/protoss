# CLI reference

This is the command reference, grouped by task. Every command here was run against the
current build; outputs shown are real. Run `_build/default/bin/main.exe --help` for the
raw usage banner.

Conventions: commands run from the repo root; `BIN` = `_build/default/bin/main.exe` (or
`dune exec protoss --`). `<file>` is a `.protoss` / `.pt` / `.ptc` / `.ptb` source;
`<project>` is a directory with `protoss.toml`; `<store>` is a `.protoss/store` directory.

## Single-file: parse, check, normalize, hash, eval

| Command | What it does | Verified output (basic.protoss) |
|---|---|---|
| `parse <file>` | parse to AST | — |
| `check <file>` | parse + typecheck | `OK: 6 definitions` |
| `nf <file>` | normalize every def | `main = 2`, … |
| `hash <file>` | program content hash | `p2:de5374...` |
| `eval <file> --entry <name>` | evaluate one definition | `main = 2` |
| `bytecode <file>` | compile to VM bytecode, print module hash | `p2:7cbcc377...` |
| `bytecode run <file> --entry <name>` | run on the bytecode VM | `2` |
| `termination <file> <def>` | totality report for a definition | `status=trivial`, … |
| `duplicates <file>` | report duplicate DefIds | `duplicates=0` |

`check`, `hash`, `nf`, `eval` also accept `--graph <graph.json>` and
`--store-graph <project-or-store> <graphHash>` to work on a canonical graph directly.

```sh
$BIN eval examples/basic.protoss --entry main           # main = 2
$BIN bytecode run examples/basic.protoss --entry main   # 2  (VM, at parity with the interpreter)
```

## Canonical formats: canon, convert, compare

| Command | What it does |
|---|---|
| `canon <file>` | print canonical text (`protoss-canon-v2`) |
| `canon --version` | print canonical version (`protoss-canon-v2`) |
| `canon --ptb <file>` | emit canonical binary |
| `canon --graph <file>` | emit canonical graph JSON |
| `canon --from-graph <graph.json>` | graph → canonical text |
| `canon --migrate-graph <graph.json>` | validate + re-emit current graph JSON |
| `convert --to pt\|ptc\|ptb <file>` | convert between views |
| `convert --from-graph --to pt <graph.json>` | graph → human source |
| `compare <a> <b>` | report whether two files are the same program |
| `compare --graph <a.json> <b.json>` | same, for graphs |
| `compare --project <a> <b>` | same, for projects |

```sh
$BIN compare examples/basic.pt examples/basic.ptc
# same
# hash=p2:de5374465e4aa71a71bbcf9b21ce08f7a99f60e669706888a680388bcc381718
```

See [canonical-and-formats.md](canonical-and-formats.md).

## Effects: run, resume, world, ledger

| Command | What it does |
|---|---|
| `run <file> --entry <name> [--ledger <root>]` | run a Process; suspend at first effect |
| `resume <file> --entry <name> --event <e> --response <T:v> [--ledger <root>]` | continue |
| `world init [<ledger-root>]` | initialize a world root |
| `ledger inspect\|world\|event\|replay\|diff\|fork\|simulate\|merge\|reject ...` | ledger ops |

`ledger` subcommands operate on the default root `target/ledger`. See
[ledger-and-world.md](ledger-and-world.md).

## Projects: init, check, build, lock, package, interface

| Command | What it does | Verified |
|---|---|---|
| `project init [dir]` | scaffold `protoss.toml` + `src/main.protoss` | `Initialized .../protoss.toml` |
| `project check [project]` | check without writing a store | `Project OK <name>` |
| `project build [project] [--target web\|...] [--stats] [--locked]` | build the store | `Build p2:...` + `UniverseRoot p2:...` |
| `project lock [project] [--check]` | write/verify the lockfile | `Lock p2:...` / `Lock OK p2:...` |
| `project package [project] [--check\|--locked]` | write/verify package + interface | `Package p2:...` |
| `project interface [project] [--json\|--check <file>]` | print/verify public interface | `PackageInterface OK` |
| `project export-layout [project] [--out <dir>]` | portable layout tree | — |
| `build [project] [--target web]` | alias for `project build` | — |
| `audit [project]` | verify the whole store | `Audit OK` |

```sh
$BIN project build examples/golden/hello-world
# Build p2:35fdec2f...
# UniverseRoot p2:e130ca93...
# Store <REPO>/examples/golden/hello-world/.protoss/store
```

`project build --target bytecode|wasm|llvm|javascript|sql-dataflow|gpu-kernel` writes
deterministic content-addressed backend manifests under `.protoss/store/compiled/`. See
[packaging.md](packaging.md) and [project-structure.md](project-structure.md).

## Web apps: app check, web build/serve/inspect

| Command | What it does | Verified (todo_app) |
|---|---|---|
| `app check <project>` | check web conventions (init/update/view) | `App OK model=... architecture=process` |
| `web build <project> [--out <dir>]` | emit a deterministic web bundle | `Web build p2:...` |
| `web inspect <project>` | print app refs (init/update/view, model, msg) | `architecture=process`, … |
| `web serve <project> [--port <n>]` | serve the bundle | — |

The bundle contains `index.html`, `protoss-runtime.js`, `protoss-app.json`,
`protoss-graph.json`, `protoss-canon-graph.json`, `protoss-host-contract.json`,
`protoss-capabilities.json`, `protoss-world.json`, and `protoss-compiled-artifact.txt`
(verified). See [todo-fullstack.md](todo-fullstack.md).

## Capabilities and graphs

| Command | What it does |
|---|---|
| `capabilities <file>` / `capabilities --project <project>` | capability report (incl. risks) |
| `graph <project> --out <graph.json>` / `--dot <graph.dot>` | export the graph |
| `graph --stats\|--roots\|--deps [name]\|--capabilities\|--host-contract\|--node <ref>\|--def <name> <graph.json>` | inspect a graph |
| `graph --store-graph <project> <graphHash> ...` | same, against a stored graph object |
| `agent graph <graph.json> [--summary\|--stats\|--def\|--node\|--deps\|--explain ...]` | versioned JSON envelopes |
| `agent explain <graph.json> <nameOrDefId>` | explain a definition |

```sh
$BIN graph --stats /tmp/basic.graph.json   # version=..., defs=6, nodes=20, ...
```

See [capabilities.md](capabilities.md).

## Patches and diffs

| Command | What it does | Verified |
|---|---|---|
| `patch review <patch.json>` | human-readable op summary | `op 1: AddDef` |
| `patch check <store> <patch.json>` | validate, no mutation | `Patch valid p2:...` |
| `patch apply <store> <patch.json>` | apply atomically + audit | `Patch accepted p2:...` |
| `patch audit <store> [latest\|ref]` | verify the audit chain | `Patch audit OK p2:...` |
| `patch from-diff <store-a> <store-b>` | derive a patch from two stores | — |
| `patch from-text-diff <store> <diff.patch>` | derive a patch from a text diff | — |
| `diff [--json] <store-a> <store-b>` | structural store diff | — |

See [patches.md](patches.md).

## Harnesses and agents

| Command | What it does |
|---|---|
| `harness run <project-or-store> <harness.pth>` | run a harness, print JSON report |
| `agent protocol` | print the versioned agent contract |
| `agent guard-write <path>` | reject direct writes to store internals |
| `agent commit <store> <patch.json> --harness <h.pth> [...]` | mutate via validated+harnessed commit |
| `agent factor-identical <project-or-store> [--out <patch.json>]` | propose dedup patch |
| `agent synthesize-tests <project-or-store>` | suggest harnesses from types |
| `agent generate-migration <old> <new>` | emit a migration patch candidate |
| `agent compare-candidates <project-or-store> <a.patch> <b.patch>` | compare two candidates |
| `mcp serve` | start the JSON-RPC stdio MCP server |

See [harness.md](harness.md), [patches.md](patches.md), and `docs/mcp.md`.

## Self-hosted frontend (`self`)

These run analysis written in Protoss itself, checked by the kernel and evaluated by the
normal runtime. See [self-hosting.md](self-hosting.md).

| Command | What it does |
|---|---|
| `self parse <file>` | parse status + defs/types/imports/exports (JSON) |
| `self resolve <file>` | name-resolution report (JSON) |
| `self deps <file>` | term + type dependency order / cycles (JSON) |
| `self capabilities <file>` | capability report (JSON) |
| `self static <file> [--json]` | aggregate static report |
| `self typecheck <file> [--json]` | kernel-checked self-hosted typecheck report |
| `self type-of <file> --entry <name>` | type of one definition |
| `self fmt [--check] <file>` | self-hosted formatter |
| `self canon <file> [--compare]` | Protoss-authored canonical text; `--compare` = byte parity vs kernel |

> **Real-behavior note.** The self-hosted frontend consumes **S-expression** source. A
> real Elm-like file is rejected with `expected declaration list`:
> `self parse examples/web/site_vitrine/src/site.protoss` →
> `{"status":"error","diagnostic":"expected declaration list"}`. (Note: `examples/basic.pt`
> happens to contain S-expression text, so it does parse.)
>
> `self typecheck` covers a supported subset and reports structured `SELF_TC*` errors
> outside it (e.g. it accepts `examples/structural_recursion.protoss` →
> `Self typecheck OK`, but rejects programs using forms or prelude functions it does not
> model). `self canon --compare examples/basic.protoss` succeeds:
> `Self canonicalizer parity OK`.

## Grammar, errors, spec, doctor

| Command | What it does |
|---|---|
| `grammar kernel` | versioned executable grammar for the trusted core |
| `grammar human` | versioned Protoss/H grammar (S-expr + Elm-like) |
| `fmt [--human] [--check] <file>` | S-expression or Elm-like projection |
| `explain <code>` / `explain --list` | the public error catalog |
| `spec check [protoss-spec.md]` | audit checked spec items for proof markers |
| `doctor --v1 [--json]` | run the V1.0 release proofs |
| `invariants file\|graph\|alpha\|process\|ledger\|package ...` | executable correctness checks |

See [errors.md](errors.md) and [release-verification.md](release-verification.md).

## Store maintenance and benchmarks

| Command | What it does | Verified |
|---|---|---|
| `store list <project>` | defs with hashes, types, deps | per-def lines |
| `store graphs <project>` | content-addressed graph objects | hash list |
| `store graph <project> <hash>` | read + validate one graph object | — |
| `store host-contracts\|host-contract <project> [ref]` | host contracts | — |
| `store gc <store>` | report unreachable objects | `unreachable=0` |
| `store gc --sweep --yes <store>` | remove unreachable objects | — |
| `cache stats <dir>` | cache hit/miss/entries | `hits=0 misses=1 entries=1` |
| `cache list <dir>` | list cache entries | — |
| `bench build <project>` | write a benchmark report, print `benchmark-ref` | `benchmark-ref=p2:...` |

## REPL

```sh
$BIN repl
```

```
Protoss REPL. Enter a single expression or EOF.
protoss>
```

> **Real-behavior note.** The REPL evaluates a **single expression**, not a declaration.
> Feeding `(def x Nat 1)` is treated as an expression and fails with a `REF001` (it tries
> to evaluate `def` as a name). Enter an expression like `(succ 0)` instead.
