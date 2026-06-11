# Structured errors

Every user-visible Protoss failure is prefixed with a **stable error code** from the
public catalog (`lib/public_error.ml`). The code is part of the contract: you can match
on it in scripts and CI. `protoss explain` documents the catalog. This page lists the
codes (verified output) and shows how to use them.

## The contract

- Public CLI failures exit non-zero and print `CODE <family> error: <message>` (the
  message keeps a `path:line:column` location when it can be recovered).
- A bad or hostile input fails *through* this structured layer — never as a raw,
  uncatalogued crash. The `doctor --v1` proof `structured-errors-on-hostile-input`
  (checklist §21) enforces this, and the fuzzer asserts it across thousands of mutated
  inputs.

## Look up a code

```sh
_build/default/bin/main.exe explain CAP001
```

```
An effect requires a capability that is not declared in scope.
```

```sh
_build/default/bin/main.exe explain PATCH001
```

```
A structural patch is invalid or cannot be applied atomically.
```

## The full catalog

`protoss explain --list` (verified, current build):

```
SYN001        SyntaxError                     - Source text is not valid Protoss syntax.
HUMAN001      AmbiguousHumanSyntax            - Human Protoss/H syntax is ambiguous or unsupported.
TYPE001       TypeMismatch                    - A value has a different type from the expected type.
REF001        UnknownReference                - A definition, type, field, constructor, package, or node reference is unknown.
CAP001        CapabilityDenied                - An effect requires a capability that is not declared in scope.
CAPABILITY    CapabilityDeniedLegacy          - Effects require explicit capabilities in the project or source.
TERM001       NonTerminatingRecursion         - Recursion is not accepted by the structural termination checker.
PROC001       NonProductiveProcess            - A process is not accepted as a productive external-effect program.
HARNESS001    HarnessRegression               - A proposed change regresses an attached harness.
MIGRATION001  UnsafeMigration                 - A model or data migration is missing or unsafe.
POLICY001     PolicyViolation                 - A package, runtime, or patch policy rejects the operation.
SECRET001     SecretLeakRisk                  - A secret value may escape its declared scope.
LOAD001       LoadFailure                     - A source, graph, canonical file, or project input could not be loaded.
CHECK001      CheckFailure                    - Kernel checking rejected the program for a reason without a narrower code.
PATCH001      PatchRejected                   - A structural patch is invalid or cannot be applied atomically.
PATCH_DEPS    PatchDependencyMismatch         - Patch deps must exactly match canonical definition dependencies.
AUDIT001      AuditFailure                    - A content-addressed audit or invariant check failed.
STORE001      StoreFailure                    - A content-addressed store operation failed.
WORKSPACE001  WorkspaceFailure                - A project workspace, lockfile, package, or interface operation failed.
WEB001        WebMissingDefinition            - Missing init, update, or view definition in a web app.
WEB007        WebViewMessageMismatch          - view returns a View whose message type does not match update.
RUNTIME001    RuntimeFailure                  - A runtime store, world, or suspended-process operation failed.
SELF_FMT001   SelfHostedFormatFailure         - The self-hosted formatter rejected the source.
SELF_CANON001 SelfHostedCanonicalizerFailure  - The self-hosted canonicalizer rejected the source or an unsupported form.
SELF_TC000..011  SelfHosted typecheck codes   - Self-hosted typechecker diagnostics (unknown var, mismatch, ...).
INPUT001      InputFailure                    - Input ended unexpectedly or could not be read as requested.
SYSTEM001     SystemFailure                   - The host operating system rejected an operation.
ERROR001      GenericFailure                  - A public command failed without a narrower stable code.
INTERNAL001   InternalFailure                 - Protoss hit an internal implementation failure.
```

(Run `protoss explain --list` for the exact, complete list including every `SELF_TC*`
self-hosted typecheck code.)

## The formal taxonomy categories

A subset of codes are the formal taxonomy families the spec reasons about:

| Category | Code |
|---|---|
| TypeMismatch | `TYPE001` |
| UnknownReference | `REF001` |
| CapabilityDenied | `CAP001` |
| NonTerminatingRecursion | `TERM001` |
| NonProductiveProcess | `PROC001` |
| HarnessRegression | `HARNESS001` |
| AmbiguousHumanSyntax | `HUMAN001` |
| UnsafeMigration | `MIGRATION001` |
| PolicyViolation | `POLICY001` |
| SecretLeakRisk | `SECRET001` |

## A real failure

The golden `capability-denied-demo` produces a `CAP001` with a precise location:

```sh
_build/default/bin/main.exe check examples/golden/capability-denied-demo/src/main.protoss
```

```
CAP001 load error: <REPO>/.../capability-denied-demo/src/main.protoss:9:34: definition fetchData: missing capability: Http.get, expression (Http.get "https://example.invalid/data")
```

> **Path-dependent codes.** The same underlying failure can surface under different codes
> depending on the entry path. The isolated `check` above reports `CAP001`; the workspace
> path (`project check` / `project build`) wraps it as `WORKSPACE001` while keeping the
> `missing capability: Http.get` message. When you need a path-independent assertion in a
> script, match on the message, not just the code. (See [capabilities.md](capabilities.md).)

## Using codes in scripts

The golden harness (`examples/golden/run.sh`) and the priority demo
(`examples/web/todo_app/priority_demo.sh`) both assert on codes/messages. The pattern:

```sh
out="$(_build/default/bin/main.exe <cmd> 2>&1)"; status=$?
[ "$status" -ne 0 ] && printf '%s' "$out" | grep -qF 'AddDef target already exists: total'
```

This is robust because the message text and exit status are part of the stable contract.
