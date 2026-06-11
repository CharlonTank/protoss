# Packaging: lock, package, interface, registries

A Protoss project can be locked, packaged, and consumed as a dependency — all by content
hash, all deterministic. This page documents the lifecycle and the imports/registries
mechanism, verified against the `pure-library` golden project (which has no external
paths, so its hashes are stable everywhere).

## The lifecycle

```
build  ->  lock  ->  package (+ interface)  ->  (consumed by another project's imports)
```

Each step writes a content-addressed artifact and has a `--check` mode that rejects drift
without rewriting anything.

## 1. Build

```sh
_build/default/bin/main.exe project build examples/golden/pure-library
```

```
Build p2:a84b0d16255d9e70d4757f74758cbb1cae80b3ed72ad660072dafda723e0841a
UniverseRoot p2:f0875b1d6b97571342b4a041e350ac4544cd978a9f5a049f0ddf2f651656f8ce
Store <REPO>/examples/golden/pure-library/.protoss/store
```

## 2. Lock

`project lock` writes `.protoss/lock`, a deterministic content-addressed lockfile over
the package metadata, canonical format versions, hash algorithm/prefix, program hash,
graph hash, host contract hash, DefIds, source unit hashes, imports, exported interface
constraints, imported contract hashes, capabilities, harness file refs, package registry
refs, and package policies.

```sh
_build/default/bin/main.exe project lock examples/golden/pure-library
```

```
Lock p2:ef1542a8a37c10d05a5b01314b95c314a313cc022ca97f9d5c1e33b3192c0b4c
Path <REPO>/examples/golden/pure-library/.protoss/lock
```

`--check` verifies the lockfile against the current build (and rewrites nothing):

```sh
_build/default/bin/main.exe project lock examples/golden/pure-library --check
```

```
Lock OK p2:ef1542a8a37c10d05a5b01314b95c314a313cc022ca97f9d5c1e33b3192c0b4c
```

`project build --locked` builds only if the existing lock matches — a CI-style guard.

## 3. Package and interface

`project package` writes the package descriptor (`.protoss/packages/<hash>.package` +
`.protoss/package`), the verified public interface
(`.protoss/interfaces/<hash>.interface.json` + `.protoss/interface`), and reports the
package, interface, and contract hashes:

```sh
_build/default/bin/main.exe project package examples/golden/pure-library
```

```
Package p2:6e91e02b9cbadc5a079e74dd692ba47f602f34e14ae147d12c2a976a17e1dac4
Path <REPO>/examples/golden/pure-library/.protoss/packages/p2_6e91e02b...package
Interface p2:a9afa6fcc897eb2940f85454cd2396e6c33fe3938038a5526148d355f794d303
InterfacePath <REPO>/examples/golden/pure-library/.protoss/interfaces/p2_a9afa6fc...interface.json
Contract p2:b6bd8194fb8c56825bc229b831adb5de58f447684a782259ef61d5abe9963f80
Lock p2:ef1542a8a37c10d05a5b01314b95c314a313cc022ca97f9d5c1e33b3192c0b4c
Build p2:a84b0d16255d9e70d4757f74758cbb1cae80b3ed72ad660072dafda723e0841a
UniverseRoot p2:f0875b1d6b97571342b4a041e350ac4544cd978a9f5a049f0ddf2f651656f8ce
```

The package descriptor is content-addressed over the lock hash, canonical format
versions, hash algorithm/prefix, program/graph/host-contract hashes, public interface
hash, declared imports/aliases/registries/interfaces/contracts/policies, `harness/**/*.pth`
refs (path + HarnessId + bytes), type aliases, DefIds, capability scopes, and source
units. The public interface hash includes exported canonical types, type hashes, named
type aliases, and capability scopes.

`--check` and `--locked`:

- `project package --check` verifies the current package hash, descriptor, interface
  pointer, and interface JSON artifact against the lock, the current canonical build,
  imported package descriptors, and any `package_interfaces` / `package_contracts`
  constraints.
- `project package --locked` requires the existing lockfile to match before writing.

## 4. Inspect the public interface

`project interface` validates the current package descriptor and prints its public
exports and locked imports:

```sh
_build/default/bin/main.exe project interface examples/golden/pure-library
```

```
PackageInterface OK
package=golden-pure-library
version=1.0.0
package_ref=p2:6e91e02b9cbadc5a079e74dd692ba47f602f34e14ae147d12c2a976a17e1dac4
interface_hash=p2:5104cf4e65dedc0d76425358e59cfb76f27a7423861fd0416919e6f175e64c78
contract_hash=p2:b6bd8194fb8c56825bc229b831adb5de58f447684a782259ef61d5abe9963f80
imports=0
exports=15
export type Point params=- type=(Record (x Nat) (y Nat)) type_hash=p2:5d2bf61b...
export type Step  params=- type=(Variant (Move Nat) (Stay Unit)) type_hash=p2:7726dfaf...
... (13 more export def lines)
```

> **Ordering note.** `project interface` requires `project package` to have run first. On
> a store with no package pointer it fails with
> `WORKSPACE001 workspace error: missing package pointer: .../.protoss/package`.

Variants:

- `project interface --json > out.json` — the same verified contract as a versioned JSON
  object with exported capability descriptors and a portable `contractHash`.
- `project interface --check <file>` — validate a saved interface contract against the
  current package.

## Consuming a package

Local package imports in the consumer's `protoss.toml`:

```toml
package_imports = ["mathlib=../mathlib"]
```

The import is locked by package ref, interface hash, and contract hash. Imported packages
are **rebuilt for verification**, and source drift (including harness ref drift) is
rejected. The importing manifest must declare **every capability** required by the
imported public interface.

### Aliases

```toml
package_aliases        = ["mathlib@1.0.0=../mathlib"]   # then import as "mathlib=mathlib@1.0.0"
package_policy_aliases = ["mathlib@SomePolicy=../mathlib"]
```

`package_aliases` verifies the imported name/version before resolving to the package hash.
`package_policy_aliases` additionally requires the imported manifest to advertise that
policy.

### Pinned constraints

```toml
package_interfaces = ["mathlib=p2:..."]   # pin the imported interface hash
package_contracts  = ["mathlib=p2:..."]   # pin the imported contract hash
```

## Registries

Registries are manifest-declared files containing deterministic
`package@selector=path` entries:

```toml
package_registry_local  = "packages.registry"          # relative to the manifest
package_registry_global = "/path/to/packages.registry" # absolute
```

- `package@semver` selectors validate name/version; non-semver selectors validate the
  advertised package policy before resolving to the locked package hash.
- **Local entries override global entries.**
- Registry refs stay in the `UniverseRoot`, lock, and package descriptors.

There are pure prelude helpers for registry resolution: `PackageRegistry.entry`,
`PackageRegistry.resolveIn`, `PackageRegistry.resolveLocalGlobal`.

## Verifying package consistency

`invariants package` checks lock consistency, descriptor freshness, interface refs,
interface JSON artifacts, exported capability descriptors, `contractHash`, exported
canonical type hashes, imported package freshness, package refs, and audit:

```sh
_build/default/bin/main.exe invariants package examples/golden/pure-library
```

```
Invariants OK
kind=package
package_ref=p2:6e91e02b...
lock_hash=p2:ef1542a8...
build_id=p2:a84b0d16...
```

## Portable layout export

`project export-layout` writes a portable tree (a stable layout contract):

```sh
_build/default/bin/main.exe project export-layout examples/web/todo_app --out /tmp/todo-layout
```

It produces `protoss.lock`, `views/**/*.pt`, `cache/program.ptb`, and
`harness/_empty.pth`.
