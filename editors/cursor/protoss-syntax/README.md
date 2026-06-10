# Protoss Syntax for Cursor

Local Cursor/VS Code extension for `.protoss` syntax highlighting and Ctrl+Click go-to-definition.

Install from this repository:

```sh
mkdir -p ~/.cursor/extensions
cp -R editors/cursor/protoss-syntax ~/.cursor/extensions/protoss.protoss-syntax-0.3.2
```

For Cursor Remote/WSL, also install it in the remote extension host:

```sh
mkdir -p ~/.cursor-server/extensions
cp -R editors/cursor/protoss-syntax ~/.cursor-server/extensions/protoss.protoss-syntax-0.3.2
```

Restart Cursor and open any `.protoss` file. Cursor should select the `Protoss` language automatically.

Supported navigation:

- Elm-like top-level functions, for example `add a b = ...`.
- Elm-like type aliases and variants.
- S-expression definitions, for example `(def Nat.add ...)`.
- S-expression named types, records, and variants.
- Built-in primitives (`succ`, `foldNat`, `foldList`, `foldVariant`, `caseList`,
  `recur`, `column`, `row`, `text`, `image`, `button`, `input`, `list`,
  `when`, `done`, `bind`, and the `Process` effects) jump to their documented
  signatures in `builtins.protoss`, since they have no user-level definition.

Diagnostics run `protoss check <file>` by default and publish the first
`file:line:column` error as a VS Code diagnostic. In this repository, if
`protoss` is not on `PATH`, the extension falls back to
`dune exec protoss -- check <file>`. The command is configurable with
`protoss.diagnostics.command` and `protoss.diagnostics.args`.

Run extension-local tests:

```sh
node editors/cursor/protoss-syntax/test/run-definition-tests.js
```
