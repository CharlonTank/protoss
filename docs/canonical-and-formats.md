# Protoss/C — canonical text, binary, and graph

The same checked program has several views. Source views (`.protoss`, `.pt`) are for
humans; **canonical views** are derived, deterministic, and content-addressed. This page
covers reading the canonical text (`.ptc`), canonical binary (`.ptb`), and the canonical
graph JSON, and converting between them.

This extends [canonical-formats.md](canonical-formats.md) (the format contract) with the
exact commands, verified against the build.

## The three views and their relationship

| Extension | What it is | Produced by |
|---|---|---|
| `.pt` / `.protoss` | human source (Protoss/H or Protoss/S) | you |
| `.ptc` | canonical text, one `protoss-canon-v2` S-expression | `protoss canon` / `convert --to ptc` |
| `.ptb` | canonical binary container wrapping the `.ptc` payload | `canon --ptb` / `convert --to ptb` |

All views of one program share the same program hash:

```sh
_build/default/bin/main.exe compare examples/basic.pt examples/basic.ptc
_build/default/bin/main.exe compare examples/basic.pt examples/basic.ptb
```

Both report:

```
same
hash=p2:de5374465e4aa71a71bbcf9b21ce08f7a99f60e669706888a680388bcc381718
```

## Canonical text (`.ptc`)

`protoss canon <file>` prints the canonical text. The current version is
`protoss-canon-v2`:

```sh
_build/default/bin/main.exe canon --version      # protoss-canon-v2
_build/default/bin/main.exe canon examples/basic.protoss
```

Output (single line, abbreviated):

```scheme
(protoss-canon-v2 (program (caps ) (defs
  (def choose p2:0a88ddf5...e9d0ce3ea Bool true)
  (def main   p2:e2c1c502...95938fc  Nat (case (ref p2:0a88ddf5...) (false (ref p2:fc6c2a91...)) (true (ref p2:1d64c74b...))))
  (def one    p2:fc6c2a91...e400603  Nat (app (builtin succ) 0))
  (def readCount p2:62863fb2...baadcad Nat (field (ref p2:60483e43...) count))
  (def rec    p2:60483e43...524ca62  (Record (count Nat) (ok Bool)) (record (count (ref p2:e2c1c502...)) (ok true)))
  (def two    p2:1d64c74b...9f469b0  Nat (foldNat 2 0 (lam Nat (app (builtin succ) #0)))))))
```

Read this carefully — it shows the canonical model:

- **Definitions are sorted** (by DefId), not in source order.
- Each `def` carries its **DefId** (the content hash of its canonical type+body).
- **Global references are explicit `(ref <DefId>)` atoms** — names are gone.
- **Local binders are De Bruijn indices** (`#0`), so local variable names are not
  semantic data in canonical form.
- Builtins are explicit (`(builtin succ)`); folds, records, fields, cases are canonical
  nodes.

`examples/basic.ptc` on disk is byte-for-byte the `canon` output above; the two were
verified identical.

A `.ptc` file loads directly through `check` / `hash` / `nf` / `eval` without reparsing
human syntax:

```sh
_build/default/bin/main.exe check examples/basic.ptc     # OK: 6 definitions
```

Canonical text validation is strict: exactly one program form; every referenced DefId
must resolve in the same program; every declared DefId must match the canonical hash of
its type and body.

## Canonical binary (`.ptb`)

`.ptb` v1 is a deterministic container: the magic bytes `PROTOSS-PTB\0\1`, a 32-bit
big-endian payload length, then the exact `.ptc` payload bytes.

```sh
_build/default/bin/main.exe canon --ptb examples/basic.protoss > /tmp/basic.ptb
_build/default/bin/main.exe check examples/basic.ptb            # OK: 6 definitions
```

The decoder validates the magic, version, and payload length before decoding.

## The canonical graph (JSON)

`canon --graph` emits the canonical graph as JSON — the same structure the store writes
to `program.graph.json` and that web bundles embed.

```sh
_build/default/bin/main.exe canon --graph examples/basic.protoss > /tmp/basic.graph.json
```

Inspect it without reparsing source:

```sh
_build/default/bin/main.exe graph --stats /tmp/basic.graph.json
```

```
Graph stats
version=protoss-canon-graph-v2
canonical_version=protoss-canon-v2
node_graph_version=protoss-canon-node-graph-v1
program_hash=p2:de5374465e4aa71a71bbcf9b21ce08f7a99f60e669706888a680388bcc381718
graph_hash=p2:9c6e4d2ca3faae78265901fb7abbcbe4ac1674e9acb5d830e77de7daf1f6d231
defs=6
capabilities=0
capability_descriptors=0
nodes=20
```

The graph carries: the `sha256`/`p2:` hash contract, a self-excluding `graphHash`, a
versioned `nodeGraph` table of content-addressed `Type`/`Term` nodes, program capability
refs, per-definition `deps`, `capabilityScope` names, `typeRef`/`termRef` roots and
`edgeRefs`, deterministic node sharing, and reachability checks that reject dead nodes.

You can load a graph directly into the analysis/run commands:

```sh
_build/default/bin/main.exe check --graph /tmp/basic.graph.json    # Graph OK: 6 definitions
_build/default/bin/main.exe hash  --graph /tmp/basic.graph.json
_build/default/bin/main.exe eval  --graph /tmp/basic.graph.json --entry main
```

### Round-trip and migration

The graph round-trips back to canonical text, and can be explicitly migrated to the
current graph format:

```sh
_build/default/bin/main.exe canon --from-graph /tmp/basic.graph.json     # -> canonical text
_build/default/bin/main.exe canon --migrate-graph /tmp/basic.graph.json  # validate + re-emit
```

`--migrate-graph` first validates the input graph, then re-emits exact current canonical
JSON. Graph loading rejects non-canonical JSON, including unknown fields and
non-canonical ordering.

## Converting between formats

`protoss convert --to pt|ptc|ptb <file>`:

```sh
_build/default/bin/main.exe convert --to ptc examples/basic.protoss > /tmp/basic.ptc
_build/default/bin/main.exe convert --to ptb examples/basic.ptc      > /tmp/basic.ptb
_build/default/bin/main.exe convert --to pt  examples/basic.ptc      > /tmp/basic.pt
_build/default/bin/main.exe convert --from-graph --to pt /tmp/basic.graph.json > /tmp/from-graph.pt
```

All conversions preserve the program hash; that is the whole point of canonical formats.

## Reading the graph from a store

A built project stores content-addressed graph objects. List and read them by hash:

```sh
_build/default/bin/main.exe store graphs <project>             # list graph object hashes
_build/default/bin/main.exe store graph  <project> <hash>      # read + validate one object
_build/default/bin/main.exe graph --store-graph <project> <hash> --stats
_build/default/bin/main.exe eval  --store-graph <project> <hash> --entry <name>
```

This is how the golden `patch-demo` and `migration-demo` scenarios evaluate a patched
program from its post-patch graph object without touching source — see
[patches.md](patches.md).
