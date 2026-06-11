# Protoss MCP server

`protoss mcp serve` runs a JSON-RPC 2.0 server over stdio (`lib/mcp_server.ml`).
It is the MCP-first editing surface: an agent inspects the content-addressed
graph and **mutates state only through validated patches**, never by editing
source files. The server is a thin contract layer — every tool delegates to the
same trusted modules the CLI uses (`Workspace`, `Patch`, `Agent_protocol`,
`Harness`, `Canonical_ir`, `Runtime`), so validation cannot be bypassed through
MCP.

## Protocol

Standard MCP JSON-RPC:

- `initialize` → returns `protocolVersion` and server info.
- `tools/list` → returns the tool catalog (name, title, description, JSON schema).
- `tools/call` `{name, arguments}` → runs a tool; the result carries
  `structuredContent` on success and `isError: true` with a message on failure.

A call to an unknown method returns a JSON-RPC `-32601` error; an unknown tool or
a tool that raises returns a `tools/call` result with `isError: true` and a
structured message (the public error taxonomy code), never an uncaught crash.

## Tools

| Tool | Purpose | Mutates store |
|---|---|---|
| `protoss.query` | Query the canonical graph (definitions, deps, …) | no |
| `protoss.readNode` | Read a canonical graph node by ref | no |
| `protoss.renderView` | Render the deterministic web view | no |
| `protoss.explain` | Explain a definition | no |
| `protoss.normalize` | Normalize an entry (source / graph / store graph) | no |
| `protoss.diff` | Structural store diff | no |
| `protoss.proposePatch` | Derive a patch candidate from a text diff | no |
| `protoss.checkPatch` | Validate a patch candidate (type, totality, caps, policies, harnesses) | no |
| `protoss.applyPatch` | Apply a patch through the validated commit path | yes |
| `protoss.runHarness` | Run a `.pth` harness for a project/store | no |
| `protoss.rollback` | Plan a rollback against the audit chain | no |

## Why check cannot be bypassed

`protoss.applyPatch` does not write the store directly: it delegates to
`Agent_protocol.commit_patch_json`, which is intentionally stricter than a bare
`patch apply` — it requires at least one harness and rejects failing harness
reports before mutating the store. So the only way state changes is a patch that
already passed checking (parse, canonicalization, typing, totality/productivity,
capabilities, secrets, policies, affected harnesses). An invalid patch submitted
through `applyPatch` is refused with a structured error and leaves the
`UniverseRoot` unchanged.

## Verification

- `protoss doctor --v1` runs the `mcp-contract` proof: `initialize` advertises a
  protocol version, `tools/list` exposes the core tools, and a bad `tools/call`
  returns a structured error rather than crashing.
- The core test section drives `Mcp_server.handle_message` with real JSON-RPC
  messages (`initialize`, `tools/list`, `protoss.query`, `protoss.runHarness`,
  and a rejected `applyPatch`), asserting the contract end to end.
