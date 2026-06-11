# Capabilities and effects

Protoss effects are explicit and typed. A program can only perform an effect if the
matching **capability** is declared in scope. The checker rejects an undeclared effect
*before* writing any canonical graph or store content. This page documents the model and
the commands that inspect it, verified against the build.

## The model in one sentence

An effect (e.g. `Http.get`) requires a capability (e.g. `Http.get`) to be present in the
program's capability set — declared in source with `(capabilities ...)` and/or in
`protoss.toml`'s `capabilities = [...]` — or checking fails with `CAP001`.

## The shipped capabilities and their requests

Capability-named effect forms and the canonical requests they produce:

| Effect form | Canonical request | Typical capability |
|---|---|---|
| `(Clock.read)` | `ReadClock` | `Clock.read` |
| `(Human.ask "q")` | `AskHuman` | `Human.ask` |
| `(Http.get "url")` | `HttpGet` | `Http.get` |
| `(Local.save "k" "v")` | `SaveLocal` | `Local.storage` |
| `(Local.load "k")` | `LoadLocal` | `Local.storage` |
| server request | `ServerRequest` | `Server.request` |

A `Process` type can pin the exact allowed scope:
`(Process (capabilities Cap.name ...) A)` in S-expression, `Process { Cap.name } A` in
Protoss/H. The bare `(Process A)` is the legacy unconstrained annotation.

## Declaring capabilities

Two places, both checked:

**In source** (top-level):

```scheme
(capabilities Clock.read)
(def now (Process String) (Clock.read))
```

**In `protoss.toml`** (the project allowlist):

```toml
capabilities = ["Clock.read"]
```

The golden `process-clock` project declares `Clock.read` in both. It checks and builds,
and its capability report is:

```sh
_build/default/bin/main.exe capabilities --project examples/golden/process-clock
```

```
program-hash=p2:f72d69db64f578b2195204012166581b5b85283e3d7aa1b0bc5b1558b59f435b
program-caps=[Clock.read]
defs=
now cap-scope-ref=p2:408d834b5736aa42a931cf5f9ba1b640b87aa6ba0d2e2e1b443949c59a5a1440 caps=[Clock.read]
readTime cap-scope-ref=p2:408d834b5736aa42a931cf5f9ba1b640b87aa6ba0d2e2e1b443949c59a5a1440 caps=[Clock.read]
risks=
none
```

## Definition-level capability scopes

`(defcap name (capabilities Cap ...) Type expr)` (and `defpolycap`) pin the exact allowed
capability scope for one definition. The checker compares the declaration against the
inferred direct + inherited scope and rejects a mismatch — **before** the canonical
graph/store is written. The declaration does **not** change the DefId.

```scheme
(defcap readTime (capabilities Clock.read) (Process String)
  (Clock.read))
```

Each definition gets a content-addressed `cap-scope-ref` (visible in the report above).

## Inspecting capabilities

`protoss capabilities <file>` for a single file, `--project <project>` for a workspace.
The report lists the program capability set, each definition's capabilities and scope
ref, and any risks. For the host-effect fixture `examples/effect_sensors.protoss`:

```sh
_build/default/bin/main.exe capabilities examples/effect_sensors.protoss
```

```
program-caps=[Clock.read,Http.get,Server.request]
defs=
readTime    cap-scope-ref=p2:408d834b... caps=[Clock.read]
fetchStatus cap-scope-ref=p2:34a467fc... caps=[Http.get]
askSensor   cap-scope-ref=p2:68d652ef... caps=[Server.request]
risks=
none
```

You can also read capabilities off a graph:

```sh
_build/default/bin/main.exe graph --capabilities graph.json
_build/default/bin/main.exe graph --capability graph.json <nameOrCapRef>
_build/default/bin/main.exe graph --capability-scopes graph.json
```

## What an undeclared effect looks like

The golden `capability-denied-demo` performs `(Http.get ...)` but declares no capability.
It MUST fail. This is the tested, expected behavior.

**Isolated file check** — `CAP001` (CapabilityDenied) with a `path:line:column`:

```sh
_build/default/bin/main.exe check examples/golden/capability-denied-demo/src/main.protoss
```

```
CAP001 load error: <REPO>/examples/golden/capability-denied-demo/src/main.protoss:9:34: definition fetchData: missing capability: Http.get, expression (Http.get "https://example.invalid/data")
```

Exit code is 1.

**Project check / build** — the workspace path wraps the same failure under
`WORKSPACE001` (not `CAP001`); the message still names the missing capability:

```sh
_build/default/bin/main.exe project check examples/golden/capability-denied-demo
```

```
WORKSPACE001 workspace error: definition fetchData: missing capability: Http.get, expression (Http.get "https://example.invalid/data")
```

> **Real-behavior note.** The error *code* differs by path: the isolated `check` surfaces
> `CAP001`; the workspace wrapper (`project check` / `project build`) surfaces
> `WORKSPACE001`. Both name `missing capability: Http.get`. Match on the message if you
> need a path-independent assertion. No store content is written on either failing path.

The code is in the public catalog:

```sh
_build/default/bin/main.exe explain CAP001
# An effect requires a capability that is not declared in scope.
```

## Executable policies

Policy names in `protoss.toml`'s `policies = [...]` are executable, not descriptive.
`NoNetworkExceptDeclared` rejects a workspace build where source declares `Http.*` or
`Server.*` capabilities that are not explicitly listed in the manifest `capabilities`.

## Secret-leak risk reporting

When a definition (or the whole program) combines a local-storage capability with an
outbound-request capability, the capability report adds a conservative `SecretLeakRisk`
entry — a hint that secret data read from storage could escape over the network. The
dedicated fixture is `examples/secret_leak_risk.protoss`:

```sh
_build/default/bin/main.exe capabilities examples/secret_leak_risk.protoss
```

```
program-caps=[Http.get,Local.storage]
defs=
riskyFetch cap-scope-ref=p2:fcb77e19... caps=[Http.get,Local.storage]
risks=
SecretLeakRisk scope=program    sources=[Local.storage] sinks=[Http.get]
SecretLeakRisk scope=def:riskyFetch sources=[Local.storage] sinks=[Http.get]
```

The public taxonomy code for this family is `SECRET001` (SecretLeakRisk). `SecretRef
scope A` is a first-class type for handles to scoped secrets; sealed secret JSON hashes
the scope/type/handle reference and never hashes or stores the raw value.

## Where capabilities flow

Capabilities are not just check-time gates. They are part of the content-addressed
identity and runtime contract:

- The `UniverseRoot`, lockfile, and package descriptor all include capabilities.
- Canonical request nodes carry and validate capability/signature refs.
- Ledger request and resume events record and validate `capability`, `capability-ref`,
  `cap-scope`, and `cap-scope-ref` (see [ledger-and-world.md](ledger-and-world.md)).
- An importing package must declare every capability required by an imported public
  interface (see [packaging.md](packaging.md)).
