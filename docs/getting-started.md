# Getting started

This guide builds the Protoss binary, creates a project, and runs the core
check / build / evaluate loop. Every command was run against the current build; the
shown output is real.

## 1. Build the binary

Protoss is an OCaml/Dune project. From the repository root:

```sh
dune build
```

This produces the CLI at `_build/default/bin/main.exe`. Throughout the docs that path
is the binary; you can equivalently run `dune exec protoss -- <args>`.

Sanity check the binary and see the full command surface:

```sh
_build/default/bin/main.exe --help
```

It prints the usage banner listing every command family (`parse`, `check`, `project`,
`patch`, `ledger`, `harness`, `agent`, `doctor`, …). The full grouped reference is in
[cli.md](cli.md).

### Running the test suites (optional)

```sh
dune runtest --force        # fast smoke suite (seconds)
dune build @fulltest        # full suite; correct without --force (rules track fixtures)
```

You do not need the tests to use the CLI, but `@fulltest` is the safety net for any
change to the kernel/runtime/workspace.

## 2. Create a project

`protoss project init` scaffolds a new project in the current directory (or a named
directory):

```sh
_build/default/bin/main.exe project init myapp
```

Output:

```
Initialized <REPO>/myapp/protoss.toml
```

It writes a complete `protoss.toml` and a minimal source file. The scaffolded tree:

```
myapp/
  protoss.toml
  src/main.protoss
```

The generated `src/main.protoss` is the smallest valid program:

```scheme
(def main Nat 0)
```

The generated `protoss.toml` carries the full field set (all packaging fields default
to empty/`none`):

```toml
name = "protoss-app"
version = "0.1.0"
entrypoints = ["src/main.protoss"]
stdlib = "none"
source_dirs = ["src"]
store_dir = ".protoss/store"
cache_dir = ".protoss/cache"
capabilities = []
policies = []
package_aliases = []
package_policy_aliases = []
package_registry_local = "none"
package_registry_global = "none"
package_imports = []
package_interfaces = []
package_contracts = []
```

Field semantics are documented in [project-structure.md](project-structure.md).

Running `project init` with no directory argument initializes `protoss.toml` and
`src/main.protoss` in the current working directory.

## 3. Check, build, evaluate

These three commands are the everyday loop.

**Check** validates the project (parse, types, totality, capabilities) without writing
a store:

```sh
_build/default/bin/main.exe project check myapp
```

```
Project OK protoss-app
```

**Build** checks the project and writes the content-addressed `.protoss/store`,
reporting the program hash and the `UniverseRoot`:

```sh
_build/default/bin/main.exe project build examples/golden/hello-world
```

```
Build p2:35fdec2f5537ec599157a5aeb7e56ffa6331469fe538f9d76207ecc91105da67
UniverseRoot p2:e130ca930c6e1ea56067a854e93751f312f6de165015487867b91cb9f7bee3a1
Store <REPO>/examples/golden/hello-world/.protoss/store
```

(The two `p2:` values above are stable for the `hello-world` golden project: a
byte-identical source reproduces them. They depend on the build.)

**Evaluate** an entrypoint definition. You can point `eval` at a single source file
and name the definition with `--entry`:

```sh
_build/default/bin/main.exe eval examples/golden/hello-world/src/main.protoss --entry main
```

```
main = "hello, world"
```

## 4. The single-file path

You do not need a project to check or evaluate a single `.protoss` / `.pt` file. This
is the fastest way to experiment:

```sh
_build/default/bin/main.exe check examples/basic.protoss     # OK: 6 definitions
_build/default/bin/main.exe nf    examples/basic.protoss     # normalize every def
_build/default/bin/main.exe eval  examples/basic.protoss --entry main   # main = 2
_build/default/bin/main.exe hash  examples/basic.protoss     # p2:de5374...
```

`nf` prints the normal form of every definition:

```
one = 1
two = 2
choose = true
main = 2
rec = {count = 2, ok = true}
readCount = 2
```

## 5. Determinism in one command

The central invariant: the same program in any surface syntax produces the same hash.
`examples/basic.pt` (human view) and `examples/basic.protoss` (S-expression) are the
same program:

```sh
_build/default/bin/main.exe hash examples/basic.protoss
_build/default/bin/main.exe hash examples/basic.pt
```

Both print:

```
p2:de5374465e4aa71a71bbcf9b21ce08f7a99f60e669706888a680388bcc381718
```

`protoss compare` makes this explicit across views (here the canonical text `.ptc` and
binary `.ptb`):

```sh
_build/default/bin/main.exe compare examples/basic.pt examples/basic.ptc
```

```
same
hash=p2:de5374465e4aa71a71bbcf9b21ce08f7a99f60e669706888a680388bcc381718
```

## Next steps

- Learn the surface syntaxes: [syntax-sexpr.md](syntax-sexpr.md) and
  [syntax-human.md](syntax-human.md).
- Understand the project model and store: [project-structure.md](project-structure.md).
- Browse the full command reference: [cli.md](cli.md).
