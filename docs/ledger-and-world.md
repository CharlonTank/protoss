# Processes, worlds, and the ledger

Effectful Protoss programs are typed `Process` values. Running one **suspends** at each
external effect as a typed request; you supply a response and **resume**. Every step is
recorded in a content-addressed, append-only world/event ledger that replays
deterministically. This page documents the run/resume cycle and the ledger commands,
verified against the build.

## Processes suspend; they do not perform effects

A `Process A` either completes (`done`), performs a typed request, or sequences with
`bind`. The runtime never reaches out to the host on its own — it stops at the request
and hands control back. That is what makes runs deterministic and replayable.

The golden `human-ask` program:

```scheme
(capabilities Human.ask)
(def askName (Process String) (Human.ask "What is your name?"))
```

## `run` — start a process, suspend at the first effect

```sh
_build/default/bin/main.exe run examples/golden/human-ask/src/main.protoss --entry askName
```

```
ProcessEvalKey p2:041aa01d7ad858c19f0bf078d5cddffc14ebb5a78ece067c2db6301b53d5c17b
WorldRef p2:abd4903197b43841d8bdb351159816dfde6b58ff2179906c6bf6ed20d382fe13
RequestId p2:29081582b8fa2d5d8901d648463042d3c72160af73c69c9004042da4fe94dd77
Request (AskHuman "What is your name?")
ContinuationId p2:3c42ed6dc25c4120fe0124e047c9e053cd720ae3e578ac3fb82e97e1e211a74a
CapScope Human.ask
CapScopeRef p2:accc3b1ee6ae4b9b3c454d792b1f305ee8fed9a8e50632a5b825a1b51a84af66
CapabilityRef p2:9ee297e976494ab993560bcd8738b84272b4346ba3bdc47dea1364834d8de2b9
RequestSignatureRef p2:ed108ad5edb08fe7b5539b85542ef45b9d329dbf7b92133a0c2fdcd5561e1eb4
Event p2:f562ef887cbbcd8070b95070307d1d0bee939b709c58be9ad40d69c33552c955
NextWorldRef p2:0875a4b892a35c93615d61b87a87defc9e716ee70c92c98ae1badef23df7ff1d
```

Every ref is deterministic — no wall clock, no randomness. The `Event` ref is what you
pass to `resume`. `run` accepts `--ledger <root>` to choose the ledger directory
(default `target/ledger`); it also works on a graph (`--graph`) or store graph
(`--store-graph`).

A pure-effect example: the golden `process-clock` `now` definition suspends as a
`ReadClock` request without reading any clock:

```sh
_build/default/bin/main.exe run examples/golden/process-clock/src/main.protoss --entry now
```

```
Request ReadClock
CapScope Clock.read
CapScopeRef p2:408d834b5736aa42a931cf5f9ba1b640b87aa6ba0d2e2e1b443949c59a5a1440
```

## `resume` — supply a typed response, continue

Pass the `Event` ref from `run` and a typed `--response`. Response syntax is
`Type:value`, e.g. `String:Ada`:

```sh
_build/default/bin/main.exe resume examples/golden/human-ask/src/main.protoss \
  --entry askName --event p2:f562ef887cbbcd8070b95070307d1d0bee939b709c58be9ad40d69c33552c955 \
  --response String:Ada
```

```
WorldRef p2:0875a4b892a35c93615d61b87a87defc9e716ee70c92c98ae1badef23df7ff1d
ResumeEvent p2:200ef5f3bce51fe71ec381bad0db3b53fd12cc03a6fedda208d58853d600c816
NextWorldRef p2:cbf2ce2973eb207b32489f65cb7b5eed64ac88bf6559fe0443948f5cb12812c7
Done "Ada"
```

`Done "Ada"` is the completed result. Resume validates the typed response against the
suspended request and **rejects a wrong response tag**.

> Both `run` and `resume` use the same default ledger root (`target/ledger`). To chain
> them, either omit `--ledger` on both, or pass the same `--ledger <root>` to both.

## The world/event Merkle-DAG

Worlds and events form a content-addressed Merkle-DAG:

- Each **event** (request / resume / external-error / merge) is hashed.
- Each **world** points at its previous world (and, for merges, both parents).
- Every non-initial world points at exactly one explicit event.

Initialize a fresh world root:

```sh
_build/default/bin/main.exe world init
# p2:abd4903197b43841d8bdb351159816dfde6b58ff2179906c6bf6ed20d382fe13   (the initial world ref)
```

## `ledger` — inspect, replay, branch, merge

The `ledger` subcommands operate on the default ledger root `target/ledger`.

```sh
_build/default/bin/main.exe ledger inspect <WorldRefOrEventRef>
_build/default/bin/main.exe ledger world   <WorldRef>
_build/default/bin/main.exe ledger event   <EventRef>
_build/default/bin/main.exe ledger replay  <WorldRef>
_build/default/bin/main.exe ledger diff    <WorldRefA> <WorldRefB>
_build/default/bin/main.exe ledger fork    feature <WorldRef>
_build/default/bin/main.exe ledger simulate feature <WorldRef> "try alternate host response"
_build/default/bin/main.exe ledger compare-branches <ledger-root> harness/smoke.pth feature control
_build/default/bin/main.exe ledger merge   <ledger-root> <WorldRefA> <WorldRefB>
_build/default/bin/main.exe ledger reject  <ledger-root> <WorldRef> <EventRef> HOST_TIMEOUT "host timed out"
```

**Inspect** a world validates its content hashes and prints its links:

```sh
_build/default/bin/main.exe ledger inspect p2:0875a4b892a35c93615d61b87a87defc9e716ee70c92c98ae1badef23df7ff1d
```

```
previous=p2:abd4903197b43841d8bdb351159816dfde6b58ff2179906c6bf6ed20d382fe13
event=p2:f562ef887cbbcd8070b95070307d1d0bee939b709c58be9ad40d69c33552c955
```

**Replay** a world re-derives every recorded event with its full typed metadata:

```sh
_build/default/bin/main.exe ledger replay p2:0875a4b892a35c93615d61b87a87defc9e716ee70c92c98ae1badef23df7ff1d
```

```
Event p2:f562ef887cbbcd8070b95070307d1d0bee939b709c58be9ad40d69c33552c955
world=p2:abd4903197b43841d8bdb351159816dfde6b58ff2179906c6bf6ed20d382fe13
kind=request
request-id=p2:29081582b8fa2d5d8901d648463042d3c72160af73c69c9004042da4fe94dd77
request=AskHuman:What is your name?
capability=Human.ask
capability-ref=p2:9ee297e976494ab993560bcd8738b84272b4346ba3bdc47dea1364834d8de2b9
request-tag=AskHuman
request-signature-ref=p2:ed108ad5edb08fe7b5539b85542ef45b9d329dbf7b92133a0c2fdcd5561e1eb4
request-payload-type=(Record (prompt String))
```

## What the ledger validates

Request events record and validate `capability`, `capability-ref`, `request-tag`,
`request-signature-ref`, `request-payload-type`, `response-type`, `host-codec-version`,
`request-codec-ref`, `response-codec-ref`, request/continuation ids, the suspended
request payload, `cap-scope`, and `cap-scope-ref` — both at insertion and during
inspection. Resume events validate the typed host response against the suspended
request. `external-error` events link to a request, validate the same typed response
metadata, and record `error-code` / `error-message`. Merge worlds replay both branches
with shared ancestors de-duplicated.

### Optional event signing

If `PROTOSS_LEDGER_SIGN_KEY` is set, newly recorded events carry `sha256-shared-key`
signature fields. Inspection verifies them with `PROTOSS_LEDGER_VERIFY_KEY` (or the sign
key in the same process).

## Replay determinism as an invariant

The run/resume cycle is one of the load-bearing executable invariants:

```sh
_build/default/bin/main.exe invariants process examples/golden/human-ask/src/main.protoss \
  --entry askName --response String:Ada
```

```
Invariants OK
kind=process
entry=askName
result=Done "Ada"
```

```sh
_build/default/bin/main.exe invariants ledger examples/golden/human-ask/src/main.protoss \
  --entry askName --response String:Ada --ledger /tmp/protoss-ledger-invariant
```

These verify typed `Process` resume and typed ledger request/resume events end to end.
See [release-verification.md](release-verification.md) for the full invariant set.
