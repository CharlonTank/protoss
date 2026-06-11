# Protoss operational documentation (V1.0)

This is the **usage** documentation for Protoss: how to install it, write programs,
build projects, and operate every shipped command. It is the operational companion
to `README.md` (the de-facto spec, which enumerates every form and invariant) and to
`protoss-spec.md` (the normative spec).

Every command and behavior documented here was verified by running the built binary
`_build/default/bin/main.exe` and copying the real output. Where a command's behavior
differs from what you might expect, the docs say so explicitly rather than describing
an ideal. See [verifying-the-docs.md](verifying-the-docs.md) for how to re-check that
these claims are still true.

> **Hashes are deterministic, not invented.** Every `p2:...` value shown here was
> produced by running the command. Hashes are content-addressed and reproducible from
> byte-identical inputs, but they depend on the exact build; if you change the kernel,
> canonicalizer, or stdlib, they will move. When the hash is not the point, the docs
> show the *structure* of the output instead.

## Where to start

1. [getting-started.md](getting-started.md) — install/build the binary, create your
   first project, the core check/build/eval loop.
2. [project-structure.md](project-structure.md) — `protoss.toml`, source dirs, and the
   `.protoss/store` layout a build writes.

## Writing Protoss

- [syntax-sexpr.md](syntax-sexpr.md) — **Protoss/S**, the canonical S-expression
  surface (the form the kernel grammar is written in).
- [syntax-human.md](syntax-human.md) — **Protoss/H**, the Elm-like human surface, and
  why it hashes identically to the equivalent S-expression program.
- [canonical-and-formats.md](canonical-and-formats.md) — reading **Protoss/C**
  (`.ptc` canonical text, `.ptb` canonical binary) and the canonical graph JSON.

## Operating Protoss

- [cli.md](cli.md) — the command reference, grouped by task, each entry verified.
- [capabilities.md](capabilities.md) — the typed capability/effect model.
- [ledger-and-world.md](ledger-and-world.md) — `Process` effects, suspend/resume, and
  the content-addressed world/event ledger.
- [harness.md](harness.md) — `.pth` harness files and `protoss harness run`.
- [patches.md](patches.md) — the structured patch format, with real golden examples,
  and the content-addressed audit chain.
- [packaging.md](packaging.md) — `lock`, `package`, `interface`, and registries.
- [errors.md](errors.md) — the structured public error catalog (`protoss explain`).

## Going deeper

- [todo-fullstack.md](todo-fullstack.md) — the end-to-end full-stack todo app demo:
  build, web bundle, and an evolving structured patch that adds a per-item priority.
- [self-hosting.md](self-hosting.md) — the self-hosted frontend and the trusted-kernel
  boundary (existing doc; the [cli.md](cli.md) `self` section documents the commands).
- [mcp.md](mcp.md) — the MCP server contract (existing doc).
- [release-verification.md](release-verification.md) — `protoss doctor --v1`, the
  mechanical release gate.
- [verifying-the-docs.md](verifying-the-docs.md) — how to mechanically re-verify that
  every claim in these docs is still accurate.

## Conventions used in these docs

- All commands are shown run from the **repository root** with the built binary at
  `_build/default/bin/main.exe`. You can substitute `dune exec protoss --` for the
  binary path in any example.
- `env PROTOSS_GLOBAL_STORE=` is prefixed on some examples. It only disables
  global-object interning (a hermetic run that never touches `$HOME/.protoss`); it
  changes no command output. It is optional.
- `<REPO>` stands for the absolute path of your repository checkout.
