# Self-hosting the Protoss frontend

Protoss is moving its **frontend** — the part that turns source text into a
structured, analyzable description of a module — into Protoss itself, written in
`stdlib/prelude.protoss`. The **kernel** stays in OCaml. This document marks
exactly where that boundary is, how the self-hosted frontend is run, and why
content-addressed identity still comes from the kernel.

## What is self-hosted now

The following are implemented as ordinary Protoss functions in the prelude and
evaluated through the normal Protoss evaluator:

| Concern | Prelude entry point |
|---|---|
| Lexing + S-expression parsing | `Sexp.parseText : String -> Result String (List Sexp)` |
| Declaration parsing | `Protoss.parseText : String -> Result String (List ProtossDecl)` |
| Formatting | `Protoss.formatText : String -> Result String String` |
| Name resolution | `Protoss.resolveText : String -> Result String ProtossResolveReport` |
| Type-environment report | `Protoss.checkTypeEnvText : String -> Result String ProtossTypeEnvReport` |
| Capability report | `Protoss.checkCapabilityText : String -> Result String ProtossCapabilityReport` |
| Dependency ordering | `Protoss.staticReportText` → `termOrder` / `typeEnv.order` (`ProtossDepOrder`) |
| Static report (aggregate) | `Protoss.staticReportText : String -> Result String ProtossStaticReport` |
| Canonicalizer (kernel-verified candidate) | `Protoss.canonProgramText : String -> String -> Result String String` |
| JSON report rendering | `Protoss.selfStaticJson`, `Protoss.selfParseJson`, … via `Json.render` |
| Component status reports | `Protoss.selfHumanParserJson`, `Protoss.selfHumanPrettyPrinterJson`, `Protoss.selfCanonicalizerJson`, `Protoss.selfNormalizerJson`, `Protoss.selfTypecheckerJson`, `Protoss.selfPatchValidatorJson`, `Protoss.selfHarnessRunnerJson`, `Protoss.selfPackageResolverJson`, `Protoss.selfMcpServerJson`, `Protoss.selfOptimizerJson`, `Protoss.selfCompilerBackendJson` |
| Bootstrap and TCB reports | `Protoss.selfBootstrapPlanJson`, `Protoss.selfTrustedBoundaryJson` |

These cover: parsing of `module`/`import`/`export`/`capabilities`,
`def`/`defcap`/`defpoly`/`defpolycap`/`defrec`/`defrecpoly`, `type`/`alias`,
`record`, `variant`; expression forms `lambda`/`let`/`case`/`caseList`/
`foldNat`/`foldList`/`foldVariant`/`recur`/`get`/`inst`/`request`/`done`/`bind`;
name resolution (missing/duplicate terms, types, exports); type-environment
arity and duplicate checks; capability declaration/use/scope checks; and
deterministic topological ordering of the term and type dependency graphs
(recursive variants self-reference only through guarded `recur`).

The component status reports are intentionally explicit about the current
boundary. They are ordinary Protoss functions that exercise the corresponding
prelude entry point and return a JSON `status`, `component`, and `entry`
description. They make the self-hosted path inspectable for parser,
pretty-printer, canonicalizer, normalizer, typechecker, patch-validator,
harness-runner, package-resolver, MCP, optimizer, compiler-backend, bootstrap,
and trusted-boundary work without moving final trust away from the kernel.

## What remains the OCaml trusted kernel

Everything that defines **identity and meaning** stays in OCaml (`lib/kernel.ml`
and friends):

- canonicalization (surface AST → canonical `cterm` / graph),
- typechecking that the kernel trusts for soundness,
- normalization / evaluation primitives,
- content-addressed hashing (`p2:` / sha256) and DefIds,
- the graph store, runtime `Process` effects, ledger, and patch audit chain.

The self-hosted frontend and component entries **report**; the kernel
**decides**. A `ProtossResolveReport` saying a name is missing is advisory until
the kernel agrees; a component report saying `canonicalizer` or `normalizer`
ran is evidence for the Protoss-authored path, not a replacement for trusted
canonical graph construction. The canonical graph and its hashes are produced
only by the kernel.

## How the Protoss frontend is evaluated

`protoss self <command> <file>` (see `bin/main.ml`, `command_self`):

1. reads `stdlib/prelude.protoss` (override with `PROTOSS_STDLIB`),
2. splices the target file's source, as a string literal, into a driver
   definition `(def __self_result <T> (<frontend-fn> "<source>"))`,
3. checks the combined program with the **kernel** (`Kernel.check_program`),
4. evaluates `__self_result` with the **normal evaluator**
   (`Runtime.eval_entry`), and
5. prints the resulting `String` (a JSON report, or formatted source for `fmt`).

So the report is computed entirely by Protoss code running on the trusted
runtime. The driver is the only OCaml-authored glue, and it does no analysis.

Commands:

```
protoss self parse <file>          # parse status + definitions/types/imports/exports (JSON)
protoss self fmt [--check] <file>  # deterministic formatted source (--check: nonzero if not formatted)
protoss self resolve <file>        # ProtossResolveReport (JSON)
protoss self deps <file>           # term + type dependency order / cycles (JSON)
protoss self capabilities <file>   # ProtossCapabilityReport (JSON)
protoss self static <file> [--json] # aggregate static report (JSON with --json)
protoss self canon <file> [--compare] # Protoss-authored canonical text; --compare = byte parity vs kernel
```

## The canonicalizer is a kernel-verified candidate

`Protoss.canonProgramText defIdsText source` re-implements the canonicalization
pipeline in Protoss: declaration parsing (via `Protoss.parseText`), type-alias
expansion (recursive variants stay `Named`, fields sorted), top-down
elaboration (lambda/bind annotations, variant type hints from the expected
type, wildcard and unit branches, `defrec` desugaring to `foldNat`/`foldList`/
`foldVariant`, polymorphic `inst` substitution), De Bruijn indices, and the
exact `protoss-canon-v2` byte format of `Kernel.serialize_checked_program`.

The trust contract follows the bootstrap plan's "kernel-verified candidates"
stage:

- the **kernel checks the program first** and supplies every DefId — identity
  never comes from the Protoss component;
- the component emits a full canonical-text **candidate**, and
  `protoss self canon <file> --compare` fails loudly on any byte difference;
- any form outside the supported subset returns an **explicit error** (for
  example `match`/`letRecord` sugar, unannotated lambdas, implicit polymorphic
  instantiation, `module`/`import` declarations) — the component never emits
  unverified canonical text.

The parity sweep in `test/test_protoss.ml` (`__canon_parity_*`) runs the
component against every `examples/*.protoss` fixture that the kernel checks in
isolation and asserts byte equality, with a golden check against
`examples/basic.ptc` and a floor on the number of byte-identical fixtures.

## Why canonical DefIds still come from the kernel

A DefId is the sha256 content hash of a definition's **canonical** form. Canon
is the kernel's invariant: equivalent sources in any surface syntax must hash
identically (see `CLAUDE.md`). The self-hosted parser produces a `ProtossDecl`
surface description, not a canonical `cterm`; it deliberately does **not** hash
anything. `protoss self static --json` therefore reports the
`frontendDefId` — the kernel-computed DefId of `Protoss.staticReportText`
itself — so a report is attributable to the exact frontend code that produced
it, while the DefIds of the *analyzed* program continue to come from the kernel
via `protoss hash` / the store.

This keeps a single source of truth for identity even while analysis migrates
into Protoss.

## Bootstrap path

`Protoss.selfBootstrapPlanJson` exposes the documented 0 -> 5 progression:
hosted reports, parity, advisory candidates, kernel-verified candidates,
default self-hosted execution, then a reduced trusted boundary. The companion
`Protoss.selfTrustedBoundaryJson` names the intended reduced TCB: hashes,
binary format, kernel type verifier, patch validator, and effect runtime.

The canonicalizer is the first component to reach the "kernel-verified
candidates" stage: `Protoss.canonProgramText` emits the full canonical text
and `protoss self canon --compare` requires byte equality with the kernel.
The next trust transition repeats that pattern for the remaining components:
expand parity fixtures, have the Protoss component entries emit richer
candidates where needed, and require the OCaml kernel to verify those
candidates before any hash, patch, package, or backend artifact becomes
trusted.
