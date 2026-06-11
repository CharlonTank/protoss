# Project structure

A Protoss project is a directory with a `protoss.toml` manifest and one or more source
trees. `protoss project build` checks the project and writes a content-addressed store
under `.protoss/`. This page documents the manifest fields and the store layout, both
verified against a real built project.

## `protoss.toml`

`protoss project init` scaffolds the full field set. Here is the generated manifest,
with every field explained:

```toml
name = "protoss-app"                # package name (used in interface/lock/package)
version = "0.1.0"                   # package version (semver string)
entrypoints = ["src/main.protoss"]  # source files that anchor the program
stdlib = "none"                     # "none", or a path to a prelude .protoss file
source_dirs = ["src"]               # directories discovered for .protoss / .pt files
store_dir = ".protoss/store"        # where the content-addressed store is written
cache_dir = ".protoss/cache"        # where the evaluation/elaboration cache is written
capabilities = []                   # capabilities the program is allowed to use
policies = []                       # executable package policies (e.g. NoNetworkExceptDeclared)
package_aliases = []                # "name@semver=path" import aliases
package_policy_aliases = []         # "name@policy=path" import aliases
package_registry_local = "none"     # local registry file ("packages.registry") or "none"
package_registry_global = "none"    # global registry file path or "none"
package_imports = []                # "name=path" or "name=name@semver" local imports
package_interfaces = []             # "name=p2:..." pinned imported interface hashes
package_contracts = []              # "name=p2:..." pinned imported contract hashes
```

Key fields in practice:

- **`stdlib`** — `"none"` for self-contained programs (all golden projects use this).
  Point it at `stdlib/prelude.protoss` (or a path relative to the manifest) to get the
  shipped polymorphic List/Maybe/Result/Map/Set/String/Nat helpers and the self-hosted
  Sexp/Json/Protoss parsers. The todo app uses `stdlib = "../../../stdlib/prelude.protoss"`.
- **`source_dirs`** — every `.protoss` and `.pt` file under these directories is part of
  the program. `.pt` (official human source) and `.protoss` (prototype extension) are
  both discovered.
- **`capabilities`** — the allowlist of effects the program may perform. An effect used
  in source but missing here is rejected at check time. This is the central enforcement
  point; see [capabilities.md](capabilities.md).
- **`policies`** — policy names are *executable*, not descriptive text. For example,
  `NoNetworkExceptDeclared` requires that any `Http.*` / `Server.*` capability the source
  uses is explicitly listed in `capabilities`.

Packaging fields (`package_*`, registries) are covered in [packaging.md](packaging.md).

### Source imports

Within source, files can import other files and pin them by content hash:

```scheme
import "math.protoss#p2:..."
```

A hash mismatch is rejected during workspace loading, so an import cannot silently drift.

## The `.protoss/` layout after a build

`project build` writes `store_dir`; `project lock` / `package` add sibling files. Here
is the real layout of a built + locked + packaged project:

```
.protoss/
  cache/                       # content-addressed eval/elaboration cache (cache_dir)
    p2:....cache               #   one entry per cached artifact
    stats                      #   cache statistics
  lock                         # written by `project lock`
  package                      # pointer to the current package descriptor
  packages/
    p2_....package             #   content-addressed package descriptor
  interface                    # pointer to the current public interface
  interfaces/
    p2_....interface.json      #   verified public interface JSON
  store/                       # the content-addressed store (store_dir)
    program.canon              #   canonical text of the whole program
    program.graph.json         #   canonical graph JSON
    harness.graph.json         #   canonical harness graph (protoss-harness-graph-v1)
    host.contract.json         #   host contract (typed request/response codecs)
    universe.root              #   the UniverseRoot ref
    universe.root.content      #   the UniverseRoot content
    current                    #   current build pointer
    roots                      #   root definition list
    capabilities               #   program capability summary
    world_refs                 #   world refs referenced by the program
    builds/                    #   content-addressed build refs
    canonical/                 #   canonical per-definition serializations
    defs/                      #   canonical definition objects
    defids/                    #   DefId index
    deps/                      #   per-definition dependency lists
    normal/                    #   normal forms
    types/                     #   per-definition types
    objects/                   #   content-addressed graph node objects
    graphs/                    #   content-addressed whole-graph objects
    capability-scopes/         #   content-addressed capability scope objects
    host-contracts/            #   content-addressed host contracts
    units/, unit-defs/         #   per-source-unit metadata (keyed by absolute path)
    meta/                      #   store metadata
```

### Properties of the store you can rely on

- **Deterministic and content-addressed.** Identical sources produce byte-identical
  store objects. A build that finds an object already written skips the rewrite.
- **`.protoss/` is git-ignored.** It is derived from source; you never commit it. All
  golden scenarios rebuild it from scratch.
- **`UniverseRoot` is the workspace identity.** It hashes package metadata, defs, types,
  harness refs, policies, world refs, and registry refs. `audit`, `lock --check`,
  `build --locked`, and `package --check` all reject stale `UniverseRoot` state.
- **Global interning.** By default, store objects are hardlinked to a shared global
  store at `$HOME/.protoss/global-store` (override with `PROTOSS_GLOBAL_STORE`; set it
  empty for a fully hermetic run). This physically de-duplicates identical nodes across
  projects and changes no command output.

## Inspecting a store

You do not have to read the raw files. The CLI reads them for you:

```sh
_build/default/bin/main.exe store list <project>            # defs with hashes, types, deps
_build/default/bin/main.exe store graphs <project>          # content-addressed graph objects
_build/default/bin/main.exe store graph <project> <hash>    # read + validate one graph object
_build/default/bin/main.exe audit <project>                 # verify the whole store
```

`store list` prints one line per definition. For the `patch-demo` golden project after
its patch:

```
base p2:31f69a651d436f48c7b3066c0dba6aa91e4a887ce663ebf4fb58b32a4c1bc00d Nat deps=[]
label p2:cfffd9543a5bbaad20240fbfdf58a8a682d16fa569375b9f34582d537712e2f1 String deps=[]
total p2:ccd0be0b2e0a43d9230d11236070427dca21fa4540b72d7ef11c70bdb8620f78 Nat deps=[base]
```

See [cli.md](cli.md) for the full `store` and `graph` command set.
