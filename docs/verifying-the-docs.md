# Keeping these docs honest

Every command and output in this documentation was produced by running the built binary,
not from memory. This page explains how to **mechanically re-verify** that the claims are
still true after a change, so the docs cannot silently rot.

The principle: each doc claim corresponds to a command whose real output is already
asserted by a committed harness. Re-running those harnesses re-checks the docs.

## The three load-bearing harnesses

These already exist in the repo and assert the exact commands and outputs these docs
quote. Run them from the repository root.

### 1. Golden projects — `examples/golden/run.sh`

Replays every golden project's documented scenario with **pinned hashes**: check, build,
lock, package, interface, eval, capabilities, the patch and migration flows, and the
capability-denied failure. Any drift in canonical hashing, `UniverseRoot` derivation,
patch refs, or evaluation output fails it.

```sh
examples/golden/run.sh
```

```
== Summary ==
checks run: 42
RESULT: PASS (all golden projects behaved as expected)
```

This backs: [getting-started.md](getting-started.md), [project-structure.md](project-structure.md),
[capabilities.md](capabilities.md), [patches.md](patches.md), [packaging.md](packaging.md),
and the per-project `examples/golden/*/VALIDATE.md` files (which contain the exact byte
outputs).

### 2. Full-stack priority demo — `examples/web/todo_app/priority_demo.sh`

Replays the todo app gaining a per-item priority via a 5-op structured patch, with pinned
hashes for the build, patch refs, audit chain, and migrated values.

```sh
examples/web/todo_app/priority_demo.sh
```

```
== Summary ==
checks run: 15
RESULT: PASS (todo app evolved to carry priority via structured patch)
```

This backs: [todo-fullstack.md](todo-fullstack.md) and the agent/patch claims in
[patches.md](patches.md) and [harness.md](harness.md).

### 3. The release doctor — `protoss doctor --v1`

Runs the available V1.0 proofs (canonical determinism, alpha-stability, cross-syntax hash
equivalence, `.ptc`/`.ptb`/graph round-trips, totality, capability enforcement, store +
`UniverseRoot`, ledger replay, patch audit, harness, packaging, bytecode parity,
structured errors, the priority demo) and fails if any *available* proof breaks.

```sh
_build/default/bin/main.exe doctor --v1
# summary: 23 pass, 0 fail, 2 not-yet
# V1.0 doctor: OK (no available proof is broken)
```

This backs: [release-verification.md](release-verification.md), and the invariant claims
throughout [syntax-sexpr.md](syntax-sexpr.md), [syntax-human.md](syntax-human.md),
[canonical-and-formats.md](canonical-and-formats.md), and [ledger-and-world.md](ledger-and-world.md).

## The spec audit

`protoss spec check` confirms every checked claim in the normative spec carries an
evidence marker. It is the analogous mechanism for the spec, and a good model for the
"every claim is backed by a proof" discipline these docs follow.

```sh
_build/default/bin/main.exe spec check protoss-spec.md
# Spec audit OK
# checked=307
```

## A one-shot doc re-verification

To re-check the documentation in one pass, run the three harnesses plus the spec audit:

```sh
dune build                                            # ensure the binary is current
examples/golden/run.sh                                && \
examples/web/todo_app/priority_demo.sh                && \
_build/default/bin/main.exe doctor --v1               && \
_build/default/bin/main.exe spec check protoss-spec.md
```

If all four succeed, every command and pinned output these docs quote still behaves as
written. If a hash moved (e.g. a kernel or canonicalizer change), the golden/demo scripts
fail with a precise diff, pointing at exactly which documented value to update.

## When you change behavior, update the docs

Because the docs quote real output:

- **Hashes are build-dependent.** A change to the kernel, canonicalizer, or stdlib moves
  `p2:` values. The golden scripts and `VALIDATE.md` files are the source of truth for the
  new values — copy from them, do not invent.
- **Commands are versioned by behavior.** If a command's output format changes, the
  relevant harness fails; update both the harness and the doc excerpt from the new real
  output.
- **Add a doc claim only after running it.** The whole point of this set is that no claim
  is aspirational. New commands get the same treatment: run, observe, document, and tie to
  a harness assertion.

## Known real-behavior caveats documented here

These were observed during verification and are documented inline rather than smoothed
over — re-confirm them if you touch the relevant area:

- **Capability-denied surfaces two codes by path.** Isolated `check` →
  `CAP001`; workspace `project check` / `build` → `WORKSPACE001` (message names
  `missing capability: Http.get` in both). See [capabilities.md](capabilities.md),
  [errors.md](errors.md).
- **The self-hosted frontend consumes S-expression source.** A real Elm-like file is
  rejected with `expected declaration list`; `self typecheck` covers a supported subset
  and reports `SELF_TC*` codes outside it. See the `self` section of [cli.md](cli.md) and
  [self-hosting.md](self-hosting.md).
- **The REPL evaluates a single expression, not a declaration.** Feeding `(def ...)`
  fails with `REF001`. See [cli.md](cli.md).
- **`project interface` requires `project package` first** (else
  `WORKSPACE001 ... missing package pointer`). See [packaging.md](packaging.md).
- **`ledger` subcommands use the default root `target/ledger`**, while `run` / `resume`
  default there but accept `--ledger <root>`. See [ledger-and-world.md](ledger-and-world.md).
