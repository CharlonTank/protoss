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
| JSON report rendering | `Protoss.selfStaticJson`, `Protoss.selfParseJson`, … via `Json.render` |

These cover: parsing of `module`/`import`/`export`/`capabilities`,
`def`/`defcap`/`defpoly`/`defpolycap`/`defrec`/`defrecpoly`, `type`/`alias`,
`record`, `variant`; expression forms `lambda`/`let`/`case`/`caseList`/
`foldNat`/`foldList`/`foldVariant`/`recur`/`get`/`inst`/`request`/`done`/`bind`;
name resolution (missing/duplicate terms, types, exports); type-environment
arity and duplicate checks; capability declaration/use/scope checks; and
deterministic topological ordering of the term and type dependency graphs
(recursive variants self-reference only through guarded `recur`).

## What remains the OCaml trusted kernel

Everything that defines **identity and meaning** stays in OCaml (`lib/kernel.ml`
and friends):

- canonicalization (surface AST → canonical `cterm` / graph),
- typechecking that the kernel trusts for soundness,
- normalization / evaluation primitives,
- content-addressed hashing (`p2:` / sha256) and DefIds,
- the graph store, runtime `Process` effects, ledger, and patch audit chain.

The self-hosted frontend **reports**; the kernel **decides**. A
`ProtossResolveReport` saying a name is missing is advisory until the kernel
agrees; the canonical graph and its hashes are produced only by the kernel.

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
```

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

## Future path

- self-hosted **typechecker** producing a `ProtossType` environment the kernel
  can cross-check against (parity first, trust later);
- self-hosted **canonicalizer** emitting candidate `cterm`s, verified against
  the kernel's canonical form before any hash is trusted;
- positioned **diagnostics** (`Diagnostic` with `SourceSpan`) threaded through
  the lexer/parser so malformed source reports `line:column`;
- once parity is exhaustive and audited, promote self-hosted passes from
  *advisory* to *trusted*, shrinking the OCaml kernel to canonicalization +
  hashing + runtime.
