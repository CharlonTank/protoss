# Protoss/H — the Elm-like human surface

Protoss/H is the Elm-like surface syntax. It is converted to the canonical S-expression
AST before checking, so **an Elm-like file and its S-expression equivalent produce the
identical canonical graph and hash**. You write whichever surface you prefer; identity
is the same.

Files use `.pt` (official human source) or `.protoss`. The parser auto-detects the
surface: if the text looks Elm-like it routes through the indentation-aware Elm lowering,
otherwise it parses S-expressions directly.

> The authoritative grammar is `protoss grammar human` (versioned). It documents both
> accepted human views (S-expression and Elm-like). This page is a worked tour.

## A complete Protoss/H program

`examples/web/site_vitrine` is a full Protoss/H web app. Excerpts:

```elm
type alias Model =
    { lead : String
    , status : String
    }

type Msg
    = Contact
    | Portfolio

init : Process Model
init =
    done
        { lead = "bonjour@studioclair.fr"
        , status = "Audit express disponible cette semaine"
        }

update : Msg -> Model -> Process Model
update msg model =
    case msg of
        Contact ->
            done
                { lead = model.lead
                , status = "Contact: bonjour@studioclair.fr"
                }

        Portfolio ->
            done
                { lead = model.lead
                , status = "References disponibles sur demande"
                }

actionItems : MsgViews
actionItems =
    [ button "Demander un audit" contactMsg
    , button "Voir les offres" portfolioMsg
    ]

view : Model -> MsgView
view model =
    page
```

It app-checks as a Process-architecture web app:

```sh
_build/default/bin/main.exe app check examples/web/site_vitrine
```

```
App OK model=(Record (lead String) (status String)) msg=(Variant (Contact Unit) (Portfolio Unit)) architecture=process
```

## What the Elm-like surface supports

From `protoss grammar human` (the `elm_program` section):

- Top-level `name : Type` signatures paired with `name = expr` / `name arg = expr`
  value declarations.
- `module Name exposing (...)` and `import "path" exposing (...)`.
- Top-level `capabilities Cap.name ...`.
- `type alias Name = ...` and indented `type Name = A | B | ...` union types.
- Record type/value literals `{ field : T, ... }` / `{ field = e, ... }` and record
  updates `{ model | count = next }`.
- List literals `[ a, b, c ]` (when an expected `List A` is available).
- Field access `model.count`.
- Lambdas `\x y -> expr`.
- `if ... then ... else ...`, `let ... in`, `case ... of` (with significant
  indentation for nested blocks).
- Pipelines `value |> f |> g`.
- Nat arithmetic `a + b` and comparisons `==`, `/=`, `<`, `<=`, `>`, `>=`; boolean
  `not`, `&&`, `||`.
- Process scopes in signatures, e.g. `Process { Human.ask } String` and
  `Cmd { ... } Msg`.
- Whitespace application `f x y`.

```elm
add a b = a + b                         -- signature-free Nat addition
inc = \x -> succ x                      -- lambda
greeting = "hello" |> identity         -- pipeline
total = if ready then count else 0     -- if/then/else
```

### Effect signatures with capability scopes

```elm
askName : Process { Human.ask } String
askName =
    request (AskHuman "What is your name?")
```

`Process { Cap.name } A` is the Protoss/H spelling of the S-expression
`(Process (capabilities Cap.name) A)`. The bare `Process A` form is the legacy
unconstrained annotation.

Full-stack apps send typed messages to the backend with `sendToBackend e`, which
parses as an ordinary application and lowers to the same `(sendToBackend e)`
canonical node as the S-expression surface (no special Protoss/H rule):

```elm
update msg model =
    case msg of
        BumpShared _ ->
            bind (sendToBackend (Bump unit)) (\m -> done model)
        GotShared n -> done { model | shared = Nat.toString n }
```

`sendToBackend e : Process BackendModel` is typed against the program's
`ToBackend`/`BackendModel` (read from `updateBackend`). The symmetric
`broadcast e` (which lowers to `(broadcast e)`) is the server→client push:
`updateBackend` returns it in its command slot to fan a typed `ToFrontend` value
out to every connected client over `GET /__events`, and the optional
`fromBackend : ToFrontend -> Msg` maps a received broadcast to a frontend `Msg`
(so the `GotShared` case above runs when another client bumps the shared
counter). `broadcast` and `fromBackend` live in the S-expression backend half
(the Protoss/H surface cannot yet spell a nested `Cmd { caps }` type). See
[backend-architecture.md](backend-architecture.md).

## Why the hashes match

Protoss/H lowers to the same canonical S-expression AST, so equivalent programs hash
identically. You can prove this in one step. `examples/basic.pt` and
`examples/basic.protoss` are the same program:

```sh
_build/default/bin/main.exe compare examples/basic.pt examples/basic.protoss
```

```
same
hash=p2:de5374465e4aa71a71bbcf9b21ce08f7a99f60e669706888a680388bcc381718
```

## Rendering Protoss/H from any program (`fmt --human`)

`protoss fmt --human <file>` renders the Elm-like projection of a parsed program. It is
**hash-round-trip safe**: re-parsing the rendered text reconstructs the identical
canonical hash, or the command refuses to emit (rather than emit text that would parse
differently).

```sh
_build/default/bin/main.exe fmt --human examples/basic.protoss
```

The S-expression `examples/basic.protoss` renders to Elm-like text — note `case` over
`Bool` becomes `if/then/else`, `(foldNat 2 0 ...)` becomes `2 + 0`, `(get rec count)`
becomes `rec.count`:

```elm
one : Nat
one = succ 0

two : Nat
two = 2 + 0

choose : Bool
choose = true

main : Nat
main = if choose then two else one

rec : { count : Nat, ok : Bool }
rec = { count = main, ok = true }

readCount : Nat
readCount = rec.count
```

Hashing the rendered output reproduces the original hash exactly — verified:

```sh
_build/default/bin/main.exe fmt --human examples/basic.protoss > /tmp/rendered.pt
_build/default/bin/main.exe hash /tmp/rendered.pt
# p2:de5374465e4aa71a71bbcf9b21ce08f7a99f60e669706888a680388bcc381718  (identical)
```

### Forms with no Protoss/H projection

`fmt --human` deliberately **rejects** (with an explicit error, not silent
mis-rendering) forms that have no faithful Elm-like projection, including:

- `defpoly` / `defrecpoly` definitions,
- `defcap` whose type lacks the matching explicit `Process { ... }` scope,
- the single-atom `(Clock.read)` request,
- a `case` / `let` in inline (non-tail) position,
- function types in expression position,
- lowercase dotted names that would re-parse as field access.

This is the view/canon guard: the emitter never produces text that re-parses to a
different canonical term.

## S-expression projection (`fmt`)

`protoss fmt <file>` (without `--human`) prints the trusted **S-expression** AST
projection of any parseable human source. This is the canonical-surface view of an
Elm-like file. For `examples/web/site_vitrine/src/site.protoss`:

```sh
_build/default/bin/main.exe fmt examples/web/site_vitrine/src/site.protoss
```

```scheme
(type Model (Record (lead String) (status String)))
(type Msg (Variant (Contact Unit) (Portfolio Unit)))
(def init (Process Model) (done (record (lead "bonjour@studioclair.fr") (status "Audit express disponible cette semaine"))))
(def update (-> Msg (-> Model (Process Model))) (lambda msg (lambda model (case msg ...))))
...
```

Both `fmt` and `fmt --human` are idempotent (`fmt(fmt(x)) = fmt(x)`) and preserve
DefIds after canonicalization. `--check` makes either fail (nonzero exit) if the file is
not already in that projection.

## Editor support

Cursor/VS Code syntax highlighting and Ctrl+Click go-to-definition for `.protoss` and
`.pt` files live in `editors/cursor/protoss-syntax` (see the repo `README.md` for the
install path).
