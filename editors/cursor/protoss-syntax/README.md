# Protoss Syntax for Cursor

Local Cursor/VS Code extension for `.protoss`/`.pt` syntax highlighting and Ctrl+Click go-to-definition.

Install from this repository:

```sh
mkdir -p ~/.cursor/extensions
cp -R editors/cursor/protoss-syntax ~/.cursor/extensions/protoss.protoss-syntax-0.5.0
```

For Cursor Remote/WSL, also install it in the remote extension host:

```sh
mkdir -p ~/.cursor-server/extensions
cp -R editors/cursor/protoss-syntax ~/.cursor-server/extensions/protoss.protoss-syntax-0.5.0
```

Restart Cursor and open any `.protoss` or `.pt` file. Cursor should select the `Protoss` language automatically.

Supported navigation:

- Elm-like top-level functions, for example `add a b = ...`.
- Elm-like type aliases and variants.
- S-expression definitions, for example `(def Nat.add ...)`.
- S-expression named types, records, and variants.
- Built-in primitives (`succ`, `foldNat`, `foldList`, `foldVariant`, `caseList`,
  `recur`, `column`, `row`, `text`, `image`, `button`, `input`, `list`,
  `when`, `done`, `bind`, and the `Process` effects) jump to their documented
  signatures in `builtins.protoss`, since they have no user-level definition.

## Switching between the two syntaxes

Protoss has two surface syntaxes that project the *same* canonical program (same
content hash): the explicit kernel **S-expression** form and the readable,
Elm-like **human** form. Two commands switch the active buffer between them:

- **Protoss: Switch to Human Syntax (Elm-like)** — runs `protoss fmt --human`.
- **Protoss: Switch to Kernel Syntax (S-expression)** — runs `protoss fmt`.

They are available from the command palette and the editor right-click menu on
any `.protoss`/`.pt` file. The switch reformats the current buffer in place as a
single undoable edit and is **hash-stable**: re-parsing the rendered text yields
the identical canonical hash, so switching never changes the program. If a form
has no human projection, `fmt --human` reports `Unrenderable` and the buffer is
left untouched rather than rewritten incorrectly. Like diagnostics, the commands
use `protoss` from `PATH` and fall back to `dune exec protoss --` inside this
checkout.

The toggle is also **completely reversible**: each switch stashes the buffer
text it replaced, and invoking the opposite switch — while the buffer is still
exactly the text the first switch produced — restores the previous text
verbatim (bytes, comments and hand formatting included) instead of
re-canonicalizing through `protoss fmt`. So `human -> kernel -> human` gives
back your original human-syntax file, not its canonical re-rendering. Any edit
between the two switches expires the stash and the command falls back to
running `protoss fmt` as usual; restoring never changes the program, since
both stashed sides project the same canonical hash. The stash is per document,
in memory only, and crosses surfaces only (a pure same-syntax
re-canonicalization is never undone by a repeated switch).

## Diagnostics

Diagnostics run `protoss check <file>` by default and publish the first
`file:line:column` error as a VS Code diagnostic. In this repository, if
`protoss` is not on `PATH`, the extension falls back to
`dune exec protoss -- check <file>`. The command is configurable with
`protoss.diagnostics.command` and `protoss.diagnostics.args`.

Run extension-local tests:

```sh
node editors/cursor/protoss-syntax/test/run-definition-tests.js
```
