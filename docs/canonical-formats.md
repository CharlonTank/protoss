# Protoss Canonical Formats

Protoss currently accepts three source views for the same checked program:

- `.pt` is the human-source view. In the prototype it accepts the existing
  S-expression syntax and the supported Elm-like surface subset.
- `.ptc` is canonical text. It is a single `protoss-canon-v2` S-expression
  produced by `protoss canon <file>` or `protoss convert --to ptc <file>`.
- `.ptb` is canonical binary. Version 1 is a deterministic container:
  `PROTOSS-PTB\0\1`, a 32-bit big-endian payload length, then the exact `.ptc`
  payload bytes.

The `.ptc` payload is not a pretty source format. Definition bodies use
canonical terms, global references are explicit `ref <DefId>` atoms, and local
binders are De Bruijn indices such as `#0`. Local variable names are therefore
not semantic data in `.ptc`.

Canonical text validation is strict:

- the file must contain exactly one canonical program form;
- every referenced `DefId` must resolve to a definition in the same program;
- every declared `DefId` must match the canonical hash of its type and body;
- canonical graph JSON must round-trip to exact canonical serialization;
- `.ptb` payload length and magic/version must match before decoding.

Record and variant fields are serialized in canonical order by the AST and
kernel serializers. Equivalent `.pt`, `.ptc`, `.ptb`, and graph views should
therefore have the same program hash:

```sh
protoss compare examples/basic.pt examples/basic.ptc
protoss compare examples/basic.pt examples/basic.ptb
```
