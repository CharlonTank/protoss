# Protoss/S — the S-expression surface

Protoss/S is the S-expression surface syntax. It maps closely onto the canonical core,
so it is the most direct way to write Protoss and the form the kernel grammar is written
in. The Elm-like surface ([Protoss/H](syntax-human.md)) lowers to the same canonical
graph, so an S-expression program and its Protoss/H equivalent hash identically.

Files use the `.protoss` (prototype) or `.pt` (official human source) extension.

> The authoritative grammar is `protoss grammar kernel` (executable, versioned). This
> page is a worked tour; run that command for the exact production rules.

## The executable kernel grammar

```sh
_build/default/bin/main.exe grammar kernel
```

```
protoss-kernel-grammar-v1
program ::= declaration*
declaration ::= (capabilities Capability*) | (module Name) | (export Name*)
declaration ::= (type Name type-params? type) | (record Name type-params? field*)
declaration ::= (variant Name type-params? variant-case*)
declaration ::= (def Name type expr)
declaration ::= (defpoly Name (params TypeName*) type expr)
declaration ::= (defcap Name (capabilities Capability*) type expr)
declaration ::= (defpolycap Name (params TypeName*) (capabilities Capability*) type expr)
declaration ::= (defrec Name type-params? type recursion-body)
...
type ::= Unit | Bool | Nat | String | (-> type type) | (List type) | (Stream type)
type ::= (Process type) | (Process (capabilities Capability*) type)
type ::= (Record field*) | (Variant variant-case*) | (Named Name type*) | TypeName
expr ::= unit | true | false | Nat | String | Name | (lambda binder+ expr)
expr ::= (let binding expr) | (record field*) | (get expr Field)
expr ::= (variant type Constructor expr) | (inst Name type*)
expr ::= (case expr branch*) | (foldNat ...) | (foldList ...) | (foldVariant ...)
expr ::= (Nil type) | (Cons type expr expr) | (caseList expr expr Name Name expr)
expr ::= (done expr) | (request request) | (bind expr binder expr)
request ::= (AskHuman String) | (HttpGet String) | ReadClock | (SaveLocal ...) | ...
branch ::= (true expr) | (false expr) | (Constructor binder? expr) | (_ expr)
binder ::= Name | (Name type)
```

(Output abbreviated; run the command for the full list including streams, automata,
`Cmd`, and the `View` constructors.)

## Definitions

A program is a sequence of declarations. The simplest is a typed definition:

```scheme
(def main String "hello, world")
```

`(def Name Type Expr)`: the name, its type, and its body. Types are explicit in the
core; surface sugar can infer some of them (see "Inference" below).

Worked pure definitions (from `examples/basic.protoss`, which checks with 6 defs):

```scheme
(def one Nat (succ 0))
(def two Nat (foldNat 2 0 (lambda (x Nat) (succ x))))
(def choose Bool true)
(def main Nat (case choose (true two) (false one)))
(def rec (Record (ok Bool) (count Nat)) (record (count main) (ok true)))
(def readCount Nat (get rec count))
```

```sh
_build/default/bin/main.exe check examples/basic.protoss   # OK: 6 definitions
```

## Types

Built-in types: `Unit`, `Bool`, `Nat`, `String`. Constructors:

- `(-> A B)` — function from `A` to `B`. Curry for multiple args: `(-> A (-> B C))`.
- `(List A)` — homogeneous list.
- `(Record (field Type) ...)` — anonymous record type.
- `(Variant (Constructor Type) ...)` — anonymous sum type; payloads are `Unit` for
  nullary constructors.
- `(Process A)` — an effectful computation producing `A` (see
  [ledger-and-world.md](ledger-and-world.md)); `(Process (capabilities Cap ...) A)`
  pins the exact effect scope.
- `(Stream A)`, `(Automaton State Output)` — productive corecursion.
- `(View msg)`, `(Attr msg)` — the typed UI tree.

### Named types, records, variants

`type` defines an alias; `record` and `variant` are named-type sugar. Aliases are
expanded before canonical hashing, so the alias name never affects the DefId.

```scheme
(type Maybe (A) (Variant (None Unit) (Some A)))
(record Point (x Nat) (y Nat))
(record Pair (params A B) (first A) (second B))
(variant Step (Stay Unit) (Move Nat))
```

Recursive named variants are allowed when recursion is guarded by a constructor:

```scheme
(variant Tree (params A) (Leaf A) (Node (Named Tree A) (Named Tree A)))
```

(See `examples/recursive_tree.protoss`, which checks.)

## Expressions

### Values and binding

```scheme
unit          ; the Unit value
true  false   ; Bool
0  1  42      ; Nat literals
"text"        ; String
(succ n)      ; Nat successor (builtin)
(let (x Nat 1) (succ x))     ; typed local let: (let (name Type rhs) body)
(strict expr)                ; force the value at binding time (recorded as Strict)
```

### Records and field access

```scheme
(record (x 0) (y 0))     ; record value
(get point x)            ; field access
```

Record fields are serialized in canonical order, so field order in source does not
affect the hash.

### Variants and case

```scheme
(variant (Variant (None Unit) (Some Nat)) Some 4)   ; explicit variant type
(case s
  (Stay 0)        ; nullary payload: binder omitted (Unit payload)
  (Move n n))     ; bound payload
```

`case` branches: `(true e)`/`(false e)` for `Bool`, `(Constructor binder? e)` for
variants, and `(_ e)` as a wildcard for missing branches (rejected when unreachable).

### Lists

```scheme
(Cons Nat 1 (Cons Nat 2 (Nil Nat)))    ; explicit item type
(caseList xs
  (Nil "empty")
  (Cons head tail "non-empty"))         ; non-recursive list elimination
```

### Functions and application

```scheme
(lambda (x Nat) (succ x))         ; annotated lambda
(lambda (x A) (lambda (y B) x))   ; curried
(f arg)                            ; application
```

### Structural recursion (`foldNat` / `foldList` / `defrec`)

The core has no general recursion; recursion is structural folds.

```scheme
(def double (-> Nat Nat)
  (lambda (n Nat) (foldNat n 0 (lambda (acc Nat) (succ (succ acc))))))
```

`defrec` / `defrecpoly` are sugar that desugar to `foldNat` / `foldList` / `foldVariant`
for Nat / List / Variant structural recursion. Non-structural `recur` is rejected by the
totality checker (code `TERM001`).

### Polymorphism

`defpoly` defines polymorphic values; `inst` applies type arguments.

```scheme
(defpoly identity (params A) (-> A A) (lambda (x A) x))
(def idNat Nat ((inst identity Nat) 9))
```

The shipped `examples/pure-library` golden project exercises `defpoly`, `const`, Nat
folds, a named record, a named variant with its eliminator, and worked examples — it
checks, builds, locks, and packages.

### Effects (`Process`)

Effects are explicit. A `Process` either completes (`done`), performs a typed request,
or sequences with `bind`:

```scheme
(def now (Process String) (Clock.read))                 ; capability sugar -> ReadClock
(def askName (Process String) (Human.ask "What is your name?"))
(defcap askTwice (capabilities Human.ask) (Process String)
  (bind (Human.ask "First name?")
    (lambda (first String) (Human.ask "Last name?"))))
```

`Clock.read`, `Human.ask`, `Http.get`, `Local.save`, `Local.load`, and server requests
are capability-named effects. They require a matching capability in scope; see
[capabilities.md](capabilities.md). The underlying canonical requests are `ReadClock`,
`AskHuman`, `HttpGet`, `SaveLocal`, `LoadLocal`, and `ServerRequest`.

## Sugar that does not change the hash

These surface forms **elaborate** to the core; they do not introduce new canonical node
kinds, and the sugared form hashes identically to the desugared one:

| Sugar | Lowers to | Example file (checks) |
|---|---|---|
| `match` (Bool/variant/list/record/tuple) | `case` / `caseList` / `letRecord` | `examples/pattern_match.protoss` |
| `(Tuple A B)` / `(tuple a b)` | record type `(_1 A) (_2 B)` / record value | — |
| `letRecord` | one record `let` + `get` accesses | `examples/record_destructure.protoss` |
| inferred lambda annotations | annotated `lambda` | `examples/inferred_lambdas.protoss` |
| inferred type application | explicit `inst` | `examples/polymorphic_inference.protoss` |
| inferred variant type | explicit `(variant T C e)` | `examples/inferred_variants.protoss` |
| list item-type omission under `List A` | `(Cons A ...)` / `(Nil A)` | — |
| `(type ...)` / `record` / `variant` aliases | expanded before hashing | `examples/stdlib_generics.protoss` |
| `(module ...)` / `(export ...)` | namespace-qualified defs | `examples/modules/app.protoss` |

When adding a surface form, the project rule is: it must lower to existing canonical
nodes, and there must be a test asserting the sugared and desugared forms hash the same.

## Modules

```scheme
(module Math)
(export add sub)
(def add (-> Nat (-> Nat Nat)) ...)
```

Module-local definitions and type aliases are namespace-qualified. Imports may reference
only exported symbols. `examples/modules/app.protoss` checks (743 definitions, the full
modular prelude).

## Checking your syntax

- `protoss check <file>` — parse + typecheck a single file.
- `protoss fmt <file>` — print the trusted S-expression AST projection (idempotent).
- `protoss grammar kernel` — the exact accepted grammar.

See [syntax-human.md](syntax-human.md) for the Elm-like surface and
[canonical-and-formats.md](canonical-and-formats.md) for the canonical views.
