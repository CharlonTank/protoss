# Self-hosted typechecker

Protoss now exposes an executable self-hosted typecheck report through:

```sh
protoss self typecheck <file>
protoss self typecheck <file> --json
protoss self type-of <file> --entry <name>
protoss self compare-typecheck <file>
```

The report is produced by Protoss code in `stdlib/prelude.protoss`:

- `Protoss.tcExpr`
- `Protoss.tcDecls`
- `Protoss.tcText`
- `Protoss.tcTextJson`

The trusted OCaml kernel remains the source of truth for parsing the driver,
checking the prelude, canonicalization, normalization, hashing, graph loading,
and final program acceptance. The self-hosted typechecker is a reporting pass,
not a replacement for the kernel.

Supported in the current kernel-checked subset:

- `Unit`, `Bool`, `Nat`, `String`
- `List A` for annotated `Nil` and expected-context `Nil`/`Cons`
- function types
- top-level `def`, `defcap`, `defpoly`, `defpolycap`, and Nat/List
  `defrec`/`defrecpoly`
- annotated lambdas, with structural checking of direct body subterms
- `let` bindings, with structural checking of direct value/body subterms
- `foldNat`, `foldList`, `foldVariant`, `caseList`, records, fields, explicit
  variants, and inferred variants in an expected variant context when their
  expression subterms stay inside the supported subset
- application where the function and arguments are structurally checked
- named records via `record` declarations and structurally checked record construction,
  including parameter substitution for `Record A`
- named variants with explicit `variant Type Constructor payload` or expected-context
  `variant Constructor payload`, including parameter substitution for `Variant A`
- `case Bool` with structurally checked branch bodies
- `case` over named variants, with exhaustiveness and branch result checks
- `Process A` for `done`, direct request expressions, annotated `bind`, and
  `let` bindings whose final expected type is still `Process`
- explicit polymorphic instantiation with `inst`, including nested type arguments
- expected-context implicit polymorphic instantiation for direct variables,
  direct applications, direct `Nil`/`Cons` arguments that contribute type
  argument constraints under an expected `List A`, nested application spines
  whose prefix arguments can be inferred or are annotated, and current spine
  suffix arguments that need expected-context checking, including argument
  checking after inferred substitution; already-applied prefix `Nil` and
  `Cons ... Nil` list constructors can also use the inferred expected `List A`
  while the outer suffix arguments are checked in expected context

Unsupported constructs are reported with `SELF_TC004` instead of being silently
accepted. A `Process` value bound by `let` is rejected when the surrounding
expected type is pure, matching the trusted OCaml kernel rule. Remaining gaps
include `defrec`/`defrecpoly` over variants and deeper already-applied prefix
list tails whose tail also needs expected-context-only checking without an
annotation. The next step toward a fuller self-hosted checker is to cover those
constructs while continuing to route all recursive expression traversal through
kernel-accepted structural paths before attempting a self-hosted canonicalizer.
