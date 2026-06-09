# Protoss Syntax for Cursor

Local Cursor/VS Code extension for `.protoss` syntax highlighting and Ctrl+Click go-to-definition.

Install from this repository:

```sh
mkdir -p ~/.cursor/extensions
cp -R editors/cursor/protoss-syntax ~/.cursor/extensions/protoss.protoss-syntax-0.2.1
```

For Cursor Remote/WSL, also install it in the remote extension host:

```sh
mkdir -p ~/.cursor-server/extensions
cp -R editors/cursor/protoss-syntax ~/.cursor-server/extensions/protoss.protoss-syntax-0.2.1
```

Restart Cursor and open any `.protoss` file. Cursor should select the `Protoss` language automatically.

Supported navigation:

- Elm-like top-level functions, for example `add a b = ...`.
- Elm-like type aliases and variants.
- S-expression definitions, for example `(def Nat.add ...)`.
- S-expression named types, records, and variants.

Run extension-local tests:

```sh
node editors/cursor/protoss-syntax/test/run-definition-tests.js
```
