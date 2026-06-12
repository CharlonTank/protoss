(* Deterministic robustness fuzzer for Protoss input surfaces.

   Goal (G3): exercise the four untrusted input decoders plus the type checker
   and the reference evaluator, and assert that for ANY input — valid, mutated,
   or hostile — that surface
   either succeeds or fails through the project's structured error mechanism. A
   raw, unstructured crash (Stack_overflow, a bare Failure from int_of_string,
   Not_found, Invalid_argument, Match_failure, ...) is a robustness bug and is
   reported here.

   The seven targets and the exceptions treated as STRUCTURED (i.e. a clean,
   acceptable failure) for each:

   1. S-expression parser
        Protoss.Parser.parse_string : string -> Ast.program
        Protoss.Sexp.parse          : string -> Sexp.t list
      structured: Parser.Error | Sexp.Error | Elm_syntax.Error | Kernel.Error
      (parse_string routes through Elm_syntax when looks_like is true and lowers
       Elm/Sexp errors to Parser.Error; qualification can surface Kernel.Error.)

   2. Elm-like surface lowering
        Protoss.Elm_syntax.to_sexp_source : string -> string  (then re-parsed)
      structured: Elm_syntax.Error | Sexp.Error | Parser.Error | Kernel.Error

   3. Canonical .ptb binary decoder
        Protoss.Canonical_binary.decode_canonical : string -> string
        Protoss.Canonical_binary.checked_of_binary : string -> Kernel.checked
      structured: Kernel.Error | Sexp.Error
      (decode_canonical fails via Kernel.fail; the canonical re-parser reaches
       Sexp.parse via Kernel.single_sexp, so a malformed payload yields Sexp.Error
       — see the loader-translation FINDING below.)

   4. Patch JSON decoder
        Protoss.Patch.parse_ops_json : string -> Patch.t list
        Protoss.Patch.parse_json     : string -> Patch.t
        Protoss.Json.parse           : string -> Json.t
      structured: Patch.Error | Json.Error | Kernel.Error | Sexp.Error | Harness.Error
      (parse_*_json lowers Json.Error to Patch.Error; type/expr embedded "source"
       strings go through Kernel.single_sexp -> Sexp.parse so Sexp.Error/Kernel.Error
       can surface; validate_capabilities can raise Kernel.Error. Harness.Error is
       defensive — only reachable from apply/check, not from parse_ops_json itself.)

   5. Type checker
        Protoss.Parser.parse_string  then  Protoss.Kernel.check_program
      structured: Parser.Error | Sexp.Error | Elm_syntax.Error | Kernel.Error
      (a parsable program is elaborated; the checker is total and the generated
       inputs are depth-bounded, so it terminates. Its only intended elaboration
       failure is a located Kernel.Error, so any other exception is a totality or
       robustness bug.)

   6. Reference evaluator
        Protoss.Kernel.check_program  then  Protoss.Runtime.normalize_all
      structured: Parser.Error | Sexp.Error | Elm_syntax.Error | Kernel.Error
      (a well-typed program is normalized, forcing every definition to a value;
       the language is total so evaluation terminates on bounded inputs.
       Runtime.fail = Kernel.fail, so an eval error is a structured Kernel.Error
       and any other exception is a real runtime robustness bug.)

   7. Canonical round-trip invariant (a CORRECTNESS property, not just no-crash)
        Protoss.Kernel.check_program then a graph round-trip via Canonical_ir
      structured: Parser.Error | Sexp.Error | Elm_syntax.Error | Kernel.Error
      (parse/check failures are out of scope. On a VALID program the central
       content-addressing invariant must hold: its canonical graph round-trips
       back to the same canonical serialization, and re-deriving a checked
       program from the graph preserves the program hash. A mismatch raises
       Invariant_violation, which is NOT structured, so the fuzzer reports it.)

   Determinism: all randomness comes from a single Random.State seeded with a
   fixed integer (overridable via argv). Two runs with the same seed test the
   exact same inputs in the same order. No wall-clock, no global Random.

   Size/depth bounds: generated S-expressions and JSON are bounded in depth
   (max_gen_depth) and breadth (max_children) so that a *legitimate* deep-nesting
   Stack_overflow on a pathological input is not misreported. Mutations of seed
   corpus are bounded in count. These bounds are documented as constants below.
   The native decoders are NOT naturally recursion-safe on adversarial nesting,
   so the bounds are deliberately small.

   Regression corpus: test/fuzz-corpus/<target>/ holds curated inputs replayed
   verbatim before the random phase. Files named "crash_*" are KNOWN, already
   reported library bugs (see below) that currently produce an UNSTRUCTURED crash;
   the harness expects them to still crash and does NOT fail the run on them by
   default (they are documented, out-of-scope-to-fix here). Files named "clean_*"
   are hostile inputs that MUST fail structurally; if one ever crashes that is a
   new bug and fails the run. Pass --strict (or PROTOSS_FUZZ_STRICT=1) to make
   EVERY unstructured crash — including the known ones — fail the run; that is the
   mode to use once the library bugs are fixed so the crash_* files flip to guards.

   KNOWN UNSTRUCTURED CRASHES discovered while building this fuzzer (all are a bare
   `Failure "int_of_string"` leaking past the project's structured error layer —
   the CLI reports them as the generic ERROR001 fallback instead of a located
   SYN001/LOAD001/CHECK001):
     - Parser.parse_type, lib/parser.ml:143  `TVar (int_of_string n)`
       and :144 `TForall (int_of_string n, ...)` — reachable from the S-expression
       surface (e.g. `(def m (TVar abc) 0)`) AND from patch JSON via an embedded
       {"source":"(TVar abc)"} type/expr.
     - Kernel.type_of_canonical_sexp, lib/kernel.ml:3536 `TVar (int_of_string i)`
       and :3538 `TForall (int_of_string arity, ...)` — reachable from a framed
       but corrupted .ptb payload.
     - Kernel.cterm_of_canonical_sexp, lib/kernel.ml:3579 `CVar (int_of_string i)`
       for a `#<non-int>` atom — reachable from a corrupted .ptb payload.
   Fix shape (for the orchestrator, NOT done here): route these through the
   existing parse_nat_atom / a guarded int parse and `fail`/`Kernel.fail` on a
   non-integer, the way the rest of those functions already do.

   SEPARATE (lower-severity) FINDING — a structured exception that the loader
   fails to translate: a .ptb with valid framing but a malformed S-expression
   payload makes Canonical_binary.checked_of_binary raise Sexp.Error (from
   Kernel.single_sexp -> Sexp.parse). That IS a structured project exception, so
   this fuzzer accepts it for the .ptb target; but Loader.check_canonical_binary_file
   (lib/loader.ml:751) only catches Kernel.Error, so at the CLI the Sexp.Error
   escapes as `INTERNAL001 internal error: unexpected exception` instead of a
   located LOAD001/SYN001. Fix shape: also catch Sexp.Error in that loader (and in
   check_canonical_text_file) and route through `fail (locate path msg)`. The
   .ptb corpus carries clean_sexp_error_payload.ptb to pin this behavior.

   The SAME translation gap exists on the patch side: a patch with an embedded
   {"source":"(Nat"} (malformed S-expression) makes Patch.parse_ops_json raise
   Sexp.Error from Kernel.single_sexp -> Sexp.parse, which parse_patch_type/expr do
   not catch (they only catch Parser.Error), so the CLI reports
   `INTERNAL001 internal error: unexpected exception`. Structured at the library
   level (accepted here); fix shape: catch Sexp.Error (and Kernel.Error) in
   parse_patch_type/parse_patch_expr — or in single_sexp's callers — and re-raise
   as Patch.Error. The patch corpus carries clean_sexp_error_source.json. *)

open Protoss

(* ------------------------------------------------------------------ *)
(* Tuning constants (documented bounds)                               *)
(* ------------------------------------------------------------------ *)

(* Default number of fuzz iterations PER target when not overridden by argv. *)
let default_iterations = 2000

(* Default PRNG seed when not overridden by argv. Fixed for determinism. *)
let default_seed = 0x50524F54 (* "PROT" *)

(* Maximum nesting depth of a generated S-expression / JSON value. Kept small
   because the native recursive-descent decoders are not stack-safe against
   adversarial nesting; deeper trees would risk a *legitimate* Stack_overflow
   that is not a decoder bug. *)
let max_gen_depth = 6

(* Maximum number of children in a generated list / array / object. *)
let max_children = 5

(* Maximum length of a generated atom / string token. *)
let max_token_len = 6

(* Maximum number of byte-level edits applied when mutating a seed input. *)
let max_mutations = 8

(* Maximum length of a fully random byte blob fed to a decoder. *)
let max_blob_len = 64

(* ------------------------------------------------------------------ *)
(* Crash classification                                               *)
(* ------------------------------------------------------------------ *)

(* An exception is STRUCTURED for a target if it is one of that target's declared
   clean-failure exceptions. Everything else is an UNSTRUCTURED crash (a bug). *)
type outcome =
  | Ok_success
  | Ok_structured of string (* exception name : message *)
  | Crash of string (* exception name : message *)

let exn_label exn =
  match exn with
  | Parser.Error m -> ("Parser.Error", m)
  | Sexp.Error m -> ("Sexp.Error", m)
  | Elm_syntax.Error m -> ("Elm_syntax.Error", m)
  | Kernel.Error m -> ("Kernel.Error", m)
  | Patch.Error m -> ("Patch.Error", m)
  | Json.Error m -> ("Json.Error", m)
  | Harness.Error m -> ("Harness.Error", m)
  | Failure m -> ("Failure", m)
  | Not_found -> ("Not_found", "")
  | Invalid_argument m -> ("Invalid_argument", m)
  | Stack_overflow -> ("Stack_overflow", "")
  | Out_of_memory -> ("Out_of_memory", "")
  | Division_by_zero -> ("Division_by_zero", "")
  | Sys_error m -> ("Sys_error", m)
  | e -> (Printexc.to_string e, "")

(* Run [f input] and classify the result. [structured] is the predicate that
   decides, given an exception, whether it is an acceptable structured failure
   for this target. *)
let classify structured (f : string -> unit) (input : string) : outcome =
  match f input with
  | () -> Ok_success
  | exception exn ->
      let name, msg = exn_label exn in
      if structured exn then Ok_structured (name ^ ": " ^ msg)
      else Crash (name ^ ": " ^ msg)

(* Structured predicates per target. *)

let structured_sexp = function
  | Parser.Error _ | Sexp.Error _ | Elm_syntax.Error _ | Kernel.Error _ -> true
  | _ -> false

let structured_elm = function
  | Elm_syntax.Error _ | Sexp.Error _ | Parser.Error _ | Kernel.Error _ -> true
  | _ -> false

(* The .ptb decoder's documented structured failure is Kernel.Error (via
   Kernel.fail in decode_canonical and the canonical re-parser). However the
   canonical re-parser reaches Sexp.parse through Kernel.single_sexp, so a framed
   but malformed S-expression payload legitimately surfaces Sexp.Error — still a
   located, structured project exception, so it is accepted here as a clean
   failure. (Note: the loader's check_canonical_binary_file only catches
   Kernel.Error, so this Sexp.Error currently escapes to the CLI as INTERNAL001 —
   a separate translation gap reported in this file's header, NOT a raw crash.) *)
let structured_ptb = function Kernel.Error _ | Sexp.Error _ -> true | _ -> false

(* The patch decoder lowers Json.Error to Patch.Error, and validate_capabilities'
   Kernel.Error to Patch.Error; embedded type/expr "source" strings are parsed by
   Parser.parse_type/expr (Parser.Error, caught) but FIRST tokenized by
   Kernel.single_sexp -> Sexp.parse, whose Sexp.Error is NOT caught by
   parse_patch_type/expr and so escapes parse_ops_json. Sexp.Error and Kernel.Error
   are located, structured project exceptions, so they are accepted here. Harness.Error
   is defensive (only reachable from apply/check, not from parse_ops_json itself).
   (Note: like the .ptb case, this Sexp.Error currently escapes to the CLI as
   INTERNAL001 — a translation gap reported in this file's header, NOT a raw crash.) *)
let structured_patch = function
  | Patch.Error _ | Json.Error _ | Kernel.Error _ | Sexp.Error _ | Harness.Error _ ->
      true
  | _ -> false

(* The checker runs the parser first (Parser.Error/Sexp.Error/Elm_syntax.Error)
   and then elaborates; its only intended elaboration failure is a located
   Kernel.Error. Anything else (Stack_overflow, Not_found, Match_failure,
   Invalid_argument...) on parsable input is a real totality/robustness bug. *)
let structured_check = function
  | Parser.Error _ | Sexp.Error _ | Elm_syntax.Error _ | Kernel.Error _ -> true
  | _ -> false

(* The canonical-roundtrip target checks a correctness property, not just the
   absence of a crash: parse/check failures are structured (the program is out
   of scope), but a graph round-trip or hash mismatch on a VALID program is a
   content-addressing bug, raised below as [Invariant_violation] -- which is NOT
   in this set, so the fuzzer reports it. *)
let structured_roundtrip = function
  | Parser.Error _ | Sexp.Error _ | Elm_syntax.Error _ | Kernel.Error _ -> true
  | _ -> false

(* ------------------------------------------------------------------ *)
(* Target runners (the exact functions under test)                    *)
(* ------------------------------------------------------------------ *)

let run_sexp_parser (input : string) : unit =
  (* Fuzz both the raw S-expression reader and the full surface parser. *)
  ignore (Sexp.parse input);
  ignore (Parser.parse_string input)

let run_elm (input : string) : unit =
  (* to_sexp_source is the load-bearing lowering; re-parse the result like the
     real pipeline does so the whole Elm path is covered. *)
  if Elm_syntax.looks_like input then (
    let lowered = Elm_syntax.to_sexp_source input in
    ignore (Parser.parse_string lowered))
  else
    (* Force the Elm lowering even when the heuristic would route to S-exprs, so
       the Elm tokenizer/parser still gets adversarial bytes. *)
    ignore (Elm_syntax.to_sexp_source input)

let run_ptb (input : string) : unit =
  (* decode_canonical validates the framing; checked_of_binary additionally
     re-parses the canonical payload (where unguarded int_of_string lives). *)
  ignore (Canonical_binary.decode_canonical input);
  ignore (Canonical_binary.checked_of_binary input)

let run_patch (input : string) : unit =
  (* parse_ops_json subsumes parse_json (single-op fall-through) and also walks
     the embedded type/expr "source" strings through the kernel. Also exercise
     the raw JSON reader on the same bytes. *)
  ignore (Json.parse input);
  ignore (Patch.parse_ops_json input)

let run_check (input : string) : unit =
  (* Drive the type checker past the parser: a parsable program is elaborated by
     Kernel.check_program, so the kernel's inference/normalization/capability
     pass must never raise a non-structured exception on parsable input. The
     checker is total and the generated inputs are depth-bounded, so it
     terminates. *)
  let program = Parser.parse_string input in
  ignore (Kernel.check_program program)

let run_eval (input : string) : unit =
  (* Drive the reference evaluator past the checker: a well-typed program is
     normalized, forcing every definition to a value. The language is total, so
     on the depth-bounded seeds this terminates. Runtime.fail = Kernel.fail, so
     an evaluation error is a structured Kernel.Error; any other exception
     (Stack_overflow, Match_failure, Not_found...) is a real runtime bug. *)
  let checked = Kernel.check_program (Parser.parse_string input) in
  ignore (Runtime.normalize_all checked)

(* A correctness-property violation on a valid program (not a structured parse/
   check failure), reported by the canonical-roundtrip target as a crash. *)
exception Invariant_violation of string

let run_roundtrip (input : string) : unit =
  (* Parse/check errors propagate structured. Below the program is VALID, so the
     central content-addressing invariant must hold: its canonical graph must
     round-trip back to the same canonical serialization, and re-deriving a
     checked program from the graph must preserve the program hash. A mismatch
     is a determinism/content-addressing bug, not an acceptable failure. *)
  let checked = Kernel.check_program (Parser.parse_string input) in
  let canonical = Kernel.serialize_checked_program checked in
  let graph = Canonical_ir.serialize_graph checked in
  if not (String.equal canonical (Canonical_ir.graph_to_program graph)) then
    raise (Invariant_violation "canonical graph round-trip mismatch");
  let graph_checked = Canonical_ir.checked_of_graph graph in
  if not (String.equal (Kernel.hash_program checked) (Kernel.hash_program graph_checked)) then
    raise (Invariant_violation "graph re-check program hash mismatch")

(* ------------------------------------------------------------------ *)
(* Seed corpus (embedded, so the fuzzer is self-contained)            *)
(* ------------------------------------------------------------------ *)

(* Valid-ish S-expression / Protoss source seeds. *)
let seed_sexp =
  [
    "(def main Nat 1)";
    "(def main (-> Nat Nat) (lambda (x Nat) (succ x)))";
    "(def value (Variant (None Unit) (Some Nat)) (variant (Variant (None Unit) \
     (Some Nat)) Some 7))";
    "(def out Nat (case value (None u 0) (Some x x)))";
    "(def xs (List Nat) (Cons 1 (Cons 2 Nil)))";
    "(def first Nat (caseList xs (Nil 0) (Cons head tail head)))";
    "(capabilities Human.ask)\n(def askName (Process String) (Human.ask \"Name?\"))";
    "(type Pair (params a b) (Tuple a b))";
    "(record Point (params) (x Nat) (y Nat))";
    "(def r (Record (x Nat) (y Nat)) (record (x 1) (y 2)))";
    "(def f (-> (TVar 0) (TVar 0)) (lambda (x (TVar 0)) x))";
    "(defrec sum (-> Nat Nat) (nat n) (zero 0) (step acc (succ acc)))";
    (* The full Lamdera loop: drives the sendToBackend/broadcast effect nodes
       through the checker, evaluator, and canonical-roundtrip targets. *)
    "(capabilities Server.request) (def initBackend (Record (n Nat)) (record (n 0))) (def updateBackend (-> (Variant (B Unit)) (-> (Record (n Nat)) (Tuple (Record (n Nat)) (Cmd (capabilities) (Variant (S Nat)))))) (lambda (m (Variant (B Unit))) (lambda (md (Record (n Nat))) (tuple (record (n (succ (get md n)))) (broadcast (S (succ (get md n)))))))) (def go (Process (Record (n Nat))) (sendToBackend (B unit)))";
    (* A whole view program: drives the column/input/list/button check_elab paths
       and the input/list handler-lambda inference under the checker target. *)
    "(def w (-> String (View (Variant (Set String) (Clear Unit)))) (lambda (s String) (column (Cons (View (Variant (Set String) (Clear Unit))) (input s (lambda (t String) (variant (Variant (Set String) (Clear Unit)) Set t))) (Cons (View (Variant (Set String) (Clear Unit))) (list (Cons String s (Nil String)) (lambda (i String) (text i))) (Nil (View (Variant (Set String) (Clear Unit)))))))))";
  ]

(* Valid-ish Elm-like seeds. *)
let seed_elm =
  [
    "main : Nat\nmain = 1\n";
    "inc : Nat -> Nat\ninc x = x + 1\n";
    "module Demo exposing (main)\n\nmain : Nat\nmain = 1\n";
    "type Color = Red | Green | Blue\n";
    "type alias Point = { x : Nat, y : Nat }\n";
    "double : Nat -> Nat\ndouble n =\n  let\n    twice = n + n\n  in\n  twice\n";
    "pick : Nat -> Nat\npick n =\n  case n of\n    Zero -> 0\n    _ -> n\n";
    "andList : Bool\nandList = True && False\n";
    "f : Nat -> Nat\nf x = x |> inc\n";
    "import \"math.pt\" exposing (base)\n\nmain : Nat\nmain = base\n";
  ]

(* Valid-ish patch JSON seeds. *)
let seed_patch =
  [
    "{ \"op\":\"AddDef\", \"name\":\"two\", \"deps\":[], \"type\":\"Nat\", \
     \"expr\":[\"succ\",1] }";
    "{ \"ops\": [ { \"op\":\"AddDef\", \"name\":\"two\", \"deps\":[], \
     \"type\":\"Nat\", \"expr\":[\"succ\",1] }, { \"op\":\"ReplaceDef\", \
     \"name\":\"two\", \"deps\":[], \"type\":\"Nat\", \"expr\":[\"succ\",2] } ] }";
    "{ \"op\":\"DeleteDef\", \"name\":\"two\", \"deps\":[] }";
    "{ \"op\":\"RenameDef\", \"name\":\"two\", \"newName\":\"deux\", \"deps\":[] }";
    "{ \"op\":\"AddField\", \"name\":\"r\", \"field\":\"z\", \"deps\":[], \
     \"fieldType\":\"Nat\", \"expr\":3 }";
    "{ \"op\":\"AddCapability\", \"name\":\"x\", \"deps\":[], \
     \"capabilities\":[\"Http.get\"] }";
    "{ \"op\":\"AddHarness\", \"name\":\"h\", \"deps\":[], \"source\":\"\" }";
    "{ \"op\":\"AddDef\", \"name\":\"x\", \"deps\":[], \
     \"type\":{\"source\":\"Nat\"}, \"expr\":{\"source\":\"1\"} }";
    "{ \"op\":\"Extract\", \"name\":\"helper\", \"from\":\"main\", \"deps\":[], \
     \"type\":\"Nat\", \"expr\":1 }";
  ]

(* ------------------------------------------------------------------ *)
(* On-disk corpus discovery (optional; augments the embedded seeds)   *)
(* ------------------------------------------------------------------ *)

let read_file path =
  let ic = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () ->
      let len = in_channel_length ic in
      really_input_string ic len)

let dir_files dir suffix =
  if Sys.file_exists dir && Sys.is_directory dir then
    Sys.readdir dir |> Array.to_list |> List.sort String.compare
    |> List.filter (fun f ->
           let ls = String.length f and lf = String.length suffix in
           ls >= lf && String.sub f (ls - lf) lf = suffix)
    |> List.map (fun f -> Filename.concat dir f)
  else []

(* Locate a project subdirectory (examples/, patches/) by walking up from a
   starting directory. The test binary runs from _build, so the source tree may
   be several levels up; we also honor PROTOSS_FUZZ_CORPUS_DIR and an argv base.
   Failure to find anything is fine — the embedded seeds keep the run useful. *)
let find_up start name =
  let rec loop dir depth =
    if depth > 12 then None
    else
      let candidate = Filename.concat dir name in
      if Sys.file_exists candidate && Sys.is_directory candidate then Some candidate
      else
        let parent = Filename.dirname dir in
        if String.equal parent dir then None else loop parent (depth + 1)
  in
  loop start 0

let safe_read path = try Some (read_file path) with _ -> None

let load_disk_seeds bases =
  (* Returns (sexp_seeds, elm_seeds, ptb_seeds, patch_seeds) gathered from disk. *)
  let collect name suffix =
    bases
    |> List.filter_map (fun base -> find_up base name)
    |> List.sort_uniq String.compare
    |> List.concat_map (fun dir -> dir_files dir suffix)
    |> List.filter_map safe_read
  in
  let protoss_srcs = collect "examples" ".protoss" in
  let pt_srcs = collect "examples" ".pt" in
  let ptc_srcs = collect "examples" ".ptc" in
  let ptb_bins = collect "examples" ".ptb" in
  let patch_jsons = collect "patches" ".json" in
  (* .protoss / .pt files contain both S-expr and (some) Elm-like sources. The
     .ptc files are canonical text (good Sexp seeds too). *)
  let sexp_seeds = protoss_srcs @ pt_srcs @ ptc_srcs in
  let elm_seeds = protoss_srcs @ pt_srcs in
  (sexp_seeds, elm_seeds, ptb_bins, patch_jsons)

(* ------------------------------------------------------------------ *)
(* Random helpers (all driven by the seeded state)                    *)
(* ------------------------------------------------------------------ *)

let rnd st n = Random.State.int st (max 1 n)

(* Safe pick: never indexes an empty array (that would raise Invalid_argument
   and be mis-reported as a crash). Empty arrays fall back to "". *)
let pick st arr =
  let n = Array.length arr in
  if n = 0 then "" else arr.(Random.State.int st n)

let printable_chars =
  "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789._-+:>|\\/(){}[],\" \t\n"

let rand_char st = printable_chars.[rnd st (String.length printable_chars)]

let rand_byte st = Char.chr (Random.State.int st 256)

let rand_token st =
  let len = 1 + rnd st max_token_len in
  String.init len (fun _ -> rand_char st)

(* ------------------------------------------------------------------ *)
(* Mutators: produce a hostile variant of a seed string               *)
(* ------------------------------------------------------------------ *)

let bytes_of = Bytes.of_string

let mutate_once st (b : bytes) : bytes =
  let len = Bytes.length b in
  if len = 0 then Bytes.of_string (String.make 1 (rand_byte st))
  else
    match rnd st 7 with
    | 0 ->
        (* flip one byte to a random byte *)
        let b = Bytes.copy b in
        Bytes.set b (rnd st len) (rand_byte st);
        b
    | 1 ->
        (* flip one byte to a structural char *)
        let b = Bytes.copy b in
        let chars = "()\"{}[],:|\\;" in
        Bytes.set b (rnd st len) chars.[rnd st (String.length chars)];
        b
    | 2 ->
        (* truncate at a random point *)
        Bytes.sub b 0 (rnd st len)
    | 3 ->
        (* drop one byte *)
        let i = rnd st len in
        Bytes.cat (Bytes.sub b 0 i) (Bytes.sub b (i + 1) (len - i - 1))
    | 4 ->
        (* insert one byte *)
        let i = rnd st (len + 1) in
        let ins = Bytes.make 1 (rand_char st) in
        Bytes.cat (Bytes.sub b 0 i) (Bytes.cat ins (Bytes.sub b i (len - i)))
    | 5 ->
        (* insert an unbalanced bracket/quote run *)
        let i = rnd st (len + 1) in
        let openers = [| "("; ")"; "\""; "{"; "}"; "["; "]" |] in
        let run = String.concat "" (List.init (1 + rnd st 4) (fun _ -> pick st openers)) in
        Bytes.cat (Bytes.sub b 0 i) (Bytes.cat (bytes_of run) (Bytes.sub b i (len - i)))
    | _ ->
        (* duplicate a slice (grow toward stress without unbounded blowup) *)
        let i = rnd st len in
        let j = i + rnd st (len - i) + 1 in
        let j = min j len in
        Bytes.cat (Bytes.sub b 0 j) (Bytes.sub b i (len - i))

let mutate st (seed : string) : string =
  let n = 1 + rnd st max_mutations in
  let rec loop b k = if k <= 0 then b else loop (mutate_once st b) (k - 1) in
  Bytes.to_string (loop (bytes_of seed) n)

(* ------------------------------------------------------------------ *)
(* Generators: bounded random S-expression text and JSON text         *)
(* ------------------------------------------------------------------ *)

(* Random S-expression-ish text. We bias toward keywords the parser recognizes
   so we reach deep into the elaborator, but keep arbitrary atoms too. *)
let sexp_keywords =
  [|
    "def"; "defcap"; "defpoly"; "defrec"; "lambda"; "let"; "letRecord"; "record";
    "recordUpdate"; "tuple"; "get"; "variant"; "inst"; "case"; "match"; "foldNat";
    "foldVariant"; "foldList"; "caseList"; "Cons"; "Nil"; "succ"; "->"; "Nat";
    "Bool"; "String"; "Unit"; "List"; "View"; "Process"; "Cmd"; "TVar"; "Forall";
    "Record"; "Variant"; "Tuple"; "capabilities"; "module"; "export"; "import";
    "type"; "alias"; "node"; "attr"; "on"; "done"; "bind"; "strict"; "params";
  |]

let rec gen_sexp st depth buf =
  if depth <= 0 || Random.State.int st 3 = 0 then (
    (* leaf: keyword, atom, number, or string *)
    match rnd st 4 with
    | 0 -> Buffer.add_string buf (pick st sexp_keywords)
    | 1 -> Buffer.add_string buf (rand_token st)
    | 2 -> Buffer.add_string buf (string_of_int (Random.State.int st 1000))
    | _ ->
        Buffer.add_char buf '"';
        Buffer.add_string buf (rand_token st);
        Buffer.add_char buf '"')
  else (
    Buffer.add_char buf '(';
    let n = rnd st max_children in
    for i = 0 to n do
      if i > 0 then Buffer.add_char buf ' ';
      gen_sexp st (depth - 1) buf
    done;
    Buffer.add_char buf ')')

let gen_sexp_text st =
  let buf = Buffer.create 64 in
  let forms = 1 + rnd st 3 in
  for i = 0 to forms - 1 do
    if i > 0 then Buffer.add_char buf '\n';
    gen_sexp st max_gen_depth buf
  done;
  Buffer.contents buf

(* Random JSON text (for the patch/json target). Biased toward patch field
   names so the patch decoder is reached past the JSON layer. *)
let patch_keys =
  [|
    "op"; "name"; "newName"; "deps"; "type"; "expr"; "field"; "fieldType"; "from";
    "inline"; "source"; "capabilities"; "ops"; "AddDef"; "ReplaceDef"; "DeleteDef";
    "RenameDef"; "AddField"; "RemoveField"; "Inline"; "Extract"; "AddHarness";
    "AddCapability"; "MigrateType"; "succ"; "Nat"; "Bool";
  |]

let rec gen_json st depth buf =
  if depth <= 0 || Random.State.int st 3 = 0 then (
    match rnd st 5 with
    | 0 -> Buffer.add_string buf "null"
    | 1 -> Buffer.add_string buf (if Random.State.bool st then "true" else "false")
    | 2 -> Buffer.add_string buf (string_of_int (Random.State.int st 1000 - 500))
    | 3 ->
        Buffer.add_char buf '"';
        Buffer.add_string buf (pick st patch_keys);
        Buffer.add_char buf '"'
    | _ ->
        Buffer.add_char buf '"';
        Buffer.add_string buf (rand_token st);
        Buffer.add_char buf '"')
  else if Random.State.bool st then (
    (* array *)
    Buffer.add_char buf '[';
    let n = rnd st max_children in
    for i = 0 to n do
      if i > 0 then Buffer.add_char buf ',';
      gen_json st (depth - 1) buf
    done;
    Buffer.add_char buf ']')
  else (
    (* object *)
    Buffer.add_char buf '{';
    let n = rnd st max_children in
    for i = 0 to n do
      if i > 0 then Buffer.add_char buf ',';
      Buffer.add_char buf '"';
      Buffer.add_string buf (pick st patch_keys);
      Buffer.add_char buf '"';
      Buffer.add_char buf ':';
      gen_json st (depth - 1) buf
    done;
    Buffer.add_char buf '}')

let gen_json_text st =
  let buf = Buffer.create 64 in
  gen_json st max_gen_depth buf;
  Buffer.contents buf

(* Random raw byte blob (mainly for the .ptb decoder). *)
let gen_blob st =
  let len = rnd st max_blob_len in
  String.init len (fun _ -> rand_byte st)

(* A blob that begins with the real .ptb magic so the framing check passes and
   we drive deeper into the canonical re-parser. magic is private to
   Canonical_binary, so we reconstruct the documented prefix here. *)
let ptb_magic = "PROTOSS-PTB\000\001"

let gen_ptb_framed _st payload =
  (* magic ^ uint32_be(len) ^ payload, optionally corrupted afterwards. *)
  let len = String.length payload in
  let u32 =
    String.init 4 (fun i ->
        Char.chr ((len lsr ((3 - i) * 8)) land 0xff))
  in
  ptb_magic ^ u32 ^ payload

(* A canonical-type S-expression with random atoms in the integer-bearing slots
   (TVar i / Forall arity / a #i cterm atom). This lets the RANDOM phase, not just
   the curated corpus, rediscover the unguarded int_of_string crashes. It mixes
   numeric and non-numeric atoms so it exercises both the success and crash paths
   of those slots. *)
let gen_canonical_atom st =
  if Random.State.bool st then string_of_int (Random.State.int st 5)
  else rand_token st (* very likely non-numeric -> int_of_string crash slot *)

let gen_canonical_type st =
  match rnd st 6 with
  | 0 -> "Nat"
  | 1 -> "Bool"
  | 2 -> Printf.sprintf "(TVar %s)" (gen_canonical_atom st)
  | 3 -> Printf.sprintf "(Forall %s Nat)" (gen_canonical_atom st)
  | 4 -> "(List Nat)"
  | _ -> Printf.sprintf "(Fun Nat %s)" (if Random.State.bool st then "Nat" else "Bool")

let gen_canonical_body st =
  match rnd st 5 with
  | 0 -> "0"
  | 1 -> "true"
  | 2 -> "#" ^ gen_canonical_atom st (* CVar slot: #<atom> *)
  | 3 -> "@" ^ rand_token st (* CGlobal slot *)
  | _ -> string_of_int (Random.State.int st 5)

let gen_canonical_program st =
  let ndefs = 1 + rnd st 2 in
  let defs =
    List.init ndefs (fun i ->
        Printf.sprintf "(def n%d id%d %s %s)" i i (gen_canonical_type st)
          (gen_canonical_body st))
  in
  Printf.sprintf "(protoss-canon-v2 (program (caps) (defs %s)))"
    (String.concat " " defs)

(* A SURFACE def whose type uses the surface type grammar, including
   (TVar <atom>)/(Forall <atom> ...) which drive Parser.parse_type's unguarded
   int_of_string. Lets the random sexp phase rediscover that surface-side crash
   (random gen_sexp rarely lands the exact TVar shape on its own). *)
let gen_surface_type st =
  match rnd st 6 with
  | 0 -> "Nat"
  | 1 -> "Bool"
  | 2 -> Printf.sprintf "(TVar %s)" (gen_canonical_atom st)
  | 3 -> Printf.sprintf "(Forall %s Nat)" (gen_canonical_atom st)
  | 4 -> "(List Nat)"
  | _ -> "(-> Nat Nat)"

let gen_sexp_def_with_type st =
  Printf.sprintf "(def m %s 0)" (gen_surface_type st)

(* ------------------------------------------------------------------ *)
(* Per-target input streams                                            *)
(* ------------------------------------------------------------------ *)

(* Build an input for [target] from the seeded state, mixing strategies. *)

let next_sexp_input st seeds =
  match rnd st 6 with
  | 0 -> mutate st (pick st seeds)
  | 1 -> gen_sexp_text st
  | 2 -> pick st seeds (* feed a clean seed too: success path must stay clean *)
  | 3 -> mutate st (gen_sexp_text st)
  | 4 -> gen_sexp_def_with_type st (* surface type slots, incl. TVar/Forall *)
  | _ -> gen_blob st

let next_elm_input st seeds =
  match rnd st 4 with
  | 0 -> mutate st (pick st seeds)
  | 1 -> pick st seeds
  | 2 -> mutate st (gen_sexp_text st)
  | _ -> gen_blob st

let next_ptb_input st seeds =
  match rnd st 8 with
  | 0 -> mutate st (pick st seeds) (* corrupt a real .ptb *)
  | 1 -> pick st seeds (* a valid .ptb should round-trip cleanly *)
  | 2 -> gen_ptb_framed st (gen_sexp_text st) (* valid framing, junk payload *)
  | 3 -> gen_ptb_framed st (gen_blob st)
  | 4 -> mutate st (gen_ptb_framed st (gen_sexp_text st))
  | 5 -> gen_ptb_framed st (gen_canonical_program st) (* program-shaped payload *)
  | 6 -> mutate st (gen_ptb_framed st (gen_canonical_program st))
  | _ -> gen_blob st (* random bytes: framing check should reject *)

(* A well-formed AddDef patch whose embedded type/expr "source" carries a random
   surface type/expr — including (TVar <atom>)/(Forall <atom> ...) which drive
   Parser.parse_type's unguarded int_of_string. Lets the random phase rediscover
   that patch-side crash. The JSON itself is valid so we reach the kernel. *)
let json_escape s =
  let b = Buffer.create (String.length s + 2) in
  String.iter
    (fun c ->
      match c with
      | '"' -> Buffer.add_string b "\\\""
      | '\\' -> Buffer.add_string b "\\\\"
      | '\n' -> Buffer.add_string b "\\n"
      | '\t' -> Buffer.add_string b "\\t"
      | '\r' -> Buffer.add_string b "\\r"
      | c -> Buffer.add_char b c)
    s;
  Buffer.contents b

let gen_patch_with_source st =
  let ty = gen_canonical_type st in
  let expr = if Random.State.bool st then "1" else "(TVar " ^ rand_token st ^ ")" in
  Printf.sprintf
    "{ \"op\":\"AddDef\", \"name\":\"x\", \"deps\":[], \"type\":{\"source\":\"%s\"}, \"expr\":{\"source\":\"%s\"} }"
    (json_escape ty) (json_escape expr)

let next_patch_input st seeds =
  match rnd st 7 with
  | 0 -> mutate st (pick st seeds)
  | 1 -> gen_json_text st
  | 2 -> pick st seeds
  | 3 -> mutate st (gen_json_text st)
  | 4 -> gen_patch_with_source st
  | 5 -> mutate st (gen_patch_with_source st)
  | _ -> gen_blob st

(* ------------------------------------------------------------------ *)
(* Driver                                                              *)
(* ------------------------------------------------------------------ *)

type target = {
  name : string;
  structured : exn -> bool;
  run : string -> unit;
  next : Random.State.t -> string array -> string;
  seeds : string array;
}

type found_crash = {
  target_name : string;
  origin : string; (* "iter=N" or "corpus=<file>" *)
  detail : string;
  input : string;
  known : bool; (* true = a documented crash_* regression, not counted by default *)
}

let crashes : found_crash list ref = ref []

let corpus_dir = ref None (* where to persist newly found cases, if writable *)

(* strict = treat known crash_* regressions as failures too (use after the
   library bugs are fixed). Default off so the run stays green while loudly
   reporting the documented bugs. *)
let strict = ref false

let sanitize name =
  String.map
    (fun c ->
      match c with
      | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' | '_' | '.' -> c
      | _ -> '_')
    name

let persist_crash crash =
  match !corpus_dir with
  | None -> ()
  | Some dir -> (
      try
        if not (Sys.file_exists dir) then Sys.mkdir dir 0o755;
        let fname =
          Printf.sprintf "crash-%s-%s.input"
            (sanitize crash.target_name) (sanitize crash.origin)
        in
        let path = Filename.concat dir fname in
        let oc = open_out_bin path in
        Fun.protect
          ~finally:(fun () -> close_out_noerr oc)
          (fun () -> output_string oc crash.input)
      with _ -> ())

let hex_preview s =
  let n = min (String.length s) 96 in
  let buf = Buffer.create (n * 2) in
  for i = 0 to n - 1 do
    Buffer.add_string buf (Printf.sprintf "%02x" (Char.code s.[i]))
  done;
  if String.length s > n then Buffer.add_string buf "...";
  Buffer.contents buf

let printable_preview s =
  let n = min (String.length s) 200 in
  let buf = Buffer.create n in
  for i = 0 to n - 1 do
    let c = s.[i] in
    if Char.code c >= 0x20 && Char.code c < 0x7f then Buffer.add_char buf c
    else Buffer.add_char buf '.'
  done;
  if String.length s > n then Buffer.add_string buf "...";
  Buffer.contents buf

(* A crash signature collapses volatile detail (line:col numbers, the offending
   token echoed in the message) so that the SAME underlying bug rediscovered by
   random fuzzing matches the signature registered from a curated crash_* corpus
   file. It keeps the exception name and the leading words of the message. The
   signature is scoped to the target so a known bug in one decoder does not mask a
   genuinely new crash with the same exception name in another. *)
let normalize_detail detail =
  (* drop everything after the first colon-separated message head, then strip
     digits so "1:10: foo bar baz" and "Failure: int_of_string" reduce to stable
     keys. We keep up to the exn name + first message word. *)
  let head =
    match String.index_opt detail ':' with
    | None -> detail
    | Some i ->
        let exn = String.sub detail 0 i in
        let rest =
          String.sub detail (i + 1) (String.length detail - i - 1) |> String.trim
        in
        (* first whitespace-delimited word of the message, digits removed *)
        let word =
          match String.index_opt rest ' ' with
          | None -> rest
          | Some j -> String.sub rest 0 j
        in
        exn ^ ":" ^ word
  in
  String.map (fun c -> if c >= '0' && c <= '9' then '#' else c) head

let crash_signature target_name detail =
  target_name ^ "|" ^ normalize_detail detail

let known_signatures : (string, unit) Hashtbl.t = Hashtbl.create 16

let register_known_signature target_name detail =
  Hashtbl.replace known_signatures (crash_signature target_name detail) ()

let is_known_signature target_name detail =
  Hashtbl.mem known_signatures (crash_signature target_name detail)

let record_crash ?(known = false) target_name origin detail input =
  (* A crash counts as known if it was flagged so (a crash_* corpus file) OR its
     signature was registered by such a file earlier in this run. *)
  let known = known || is_known_signature target_name detail in
  let crash = { target_name; origin; detail; input; known } in
  crashes := crash :: !crashes;
  (if not known then persist_crash crash);
  Printf.printf
    "  %s target=%s %s\n    exn: %s\n    input (printable): %s\n    input (hex): %s\n"
    (if known then "KNOWN-CRASH" else "CRASH")
    target_name origin detail (printable_preview input) (hex_preview input)

(* ------------------------------------------------------------------ *)
(* Phase 1: replay the curated regression corpus verbatim.            *)
(* ------------------------------------------------------------------ *)

let is_prefix p s =
  let lp = String.length p in
  String.length s >= lp && String.sub s 0 lp = p

let replay_corpus (t : target) corpus_root =
  let dir = Filename.concat corpus_root t.name in
  let files =
    if Sys.file_exists dir && Sys.is_directory dir then
      Sys.readdir dir |> Array.to_list |> List.sort String.compare
      |> List.map (fun f -> (f, Filename.concat dir f))
    else []
  in
  let n_clean_ok = ref 0 and n_known = ref 0 and n_new = ref 0 in
  List.iter
    (fun (base, path) ->
      match safe_read path with
      | None -> ()
      | Some input -> (
          (* crash_* files document a known pre-existing library crash; clean_*
             files must fail structurally (or succeed). *)
          let expect_known_crash = is_prefix "crash_" base in
          match classify t.structured t.run input with
          | Ok_success | Ok_structured _ ->
              if expect_known_crash then (
                (* The bug was fixed (or no longer reproduces): surface it so the
                   crash_* file can be reclassified into a guard. *)
                incr n_new;
                Printf.printf
                  "  NOTE target=%s corpus=%s: crash_* input no longer crashes \
                   (reclassify as a guard / move out of crash_*)\n"
                  t.name base)
              else incr n_clean_ok
          | Crash detail ->
              if expect_known_crash then (
                incr n_known;
                (* register so random-phase rediscovery of the same bug is also
                   classified as known, not as a new regression. *)
                register_known_signature t.name detail;
                record_crash ~known:true t.name ("corpus=" ^ base) detail input)
              else (
                incr n_new;
                record_crash t.name ("corpus=" ^ base) detail input)))
    files;
  if files <> [] then
    Printf.printf
      "  %-28s corpus: clean-ok=%-4d known-crash=%-4d new=%d\n" t.name !n_clean_ok
      !n_known !n_new

(* ------------------------------------------------------------------ *)
(* Phase 2: random fuzzing.                                            *)
(* ------------------------------------------------------------------ *)

let run_target seed iterations (t : target) =
  let st = Random.State.make [| seed; Hashtbl.hash t.name |] in
  let n_success = ref 0 and n_structured = ref 0 in
  let n_known = ref 0 and n_new = ref 0 in
  (* Only print the first occurrence of each distinct crash signature per target
     to keep output bounded and deterministic; counts still reflect every hit. *)
  let printed = Hashtbl.create 16 in
  for i = 1 to iterations do
    let input = t.next st t.seeds in
    match classify t.structured t.run input with
    | Ok_success -> incr n_success
    | Ok_structured _ -> incr n_structured
    | Crash detail ->
        let known = is_known_signature t.name detail in
        if known then incr n_known else incr n_new;
        let sig_ = crash_signature t.name detail in
        if not (Hashtbl.mem printed sig_) then (
          Hashtbl.replace printed sig_ ();
          record_crash t.name (Printf.sprintf "iter=%d seed=%d" i seed) detail input)
        else
          (* still record (for the final tally) but without re-printing *)
          crashes :=
            {
              target_name = t.name;
              origin = Printf.sprintf "iter=%d seed=%d" i seed;
              detail;
              input;
              known;
            }
            :: !crashes
  done;
  Printf.printf
    "  %-28s random: success=%-6d structured=%-6d new-crash=%-4d known-crash=%d\n"
    t.name !n_success !n_structured !n_new !n_known

(* ------------------------------------------------------------------ *)
(* argv parsing:  fuzz [--strict] [seed] [iterations] [persist_dir]    *)
(*   - positional args are seed, iterations, persist_dir (in order)    *)
(*   - --strict toggles strict mode (known crashes fail the run)       *)
(*   - env: PROTOSS_FUZZ_{SEED,ITERATIONS,STRICT,CORPUS_DIR}           *)
(* ------------------------------------------------------------------ *)

let truthy s =
  match String.lowercase_ascii (String.trim s) with
  | "1" | "true" | "yes" | "on" -> true
  | _ -> false

let parse_args () =
  let seed = ref default_seed in
  let iters = ref default_iterations in
  (match Sys.getenv_opt "PROTOSS_FUZZ_CORPUS_DIR" with
  | Some d when d <> "" -> corpus_dir := Some d
  | _ -> ());
  (match Sys.getenv_opt "PROTOSS_FUZZ_SEED" with
  | Some s -> ( try seed := int_of_string (String.trim s) with _ -> ())
  | None -> ());
  (match Sys.getenv_opt "PROTOSS_FUZZ_ITERATIONS" with
  | Some s -> ( try iters := int_of_string (String.trim s) with _ -> ())
  | None -> ());
  (match Sys.getenv_opt "PROTOSS_FUZZ_STRICT" with
  | Some s when truthy s -> strict := true
  | _ -> ());
  (* Positional args, skipping any --flag tokens. *)
  let positional = ref [] in
  Array.iteri
    (fun i a ->
      if i = 0 then ()
      else if a = "--strict" then strict := true
      else if is_prefix "--" a then () (* ignore unknown flags *)
      else positional := a :: !positional)
    Sys.argv;
  (match List.rev !positional with
  | s :: rest -> (
      (try seed := int_of_string s with _ -> ());
      match rest with
      | it :: rest2 -> (
          (try iters := int_of_string it with _ -> ());
          match rest2 with d :: _ -> corpus_dir := Some d | [] -> ())
      | [] -> ())
  | [] -> ());
  (!seed, !iters)

let () =
  let seed, iterations = parse_args () in
  (* Base directories to search for an on-disk corpus: cwd and the binary's
     directory (so it works whether run from the repo root or from _build). *)
  let bases =
    [
      Sys.getcwd ();
      (try Filename.dirname Sys.executable_name with _ -> Sys.getcwd ());
    ]
    |> List.sort_uniq String.compare
  in
  let disk_sexp, disk_elm, disk_ptb, disk_patch = load_disk_seeds bases in
  let combine embedded disk = Array.of_list (embedded @ disk) in
  let sexp_seeds = combine seed_sexp disk_sexp in
  let elm_seeds = combine seed_elm disk_elm in
  (* For .ptb we need at least one valid binary seed; synthesize one if no disk
     .ptb is available, by serializing a tiny checked program. This keeps the
     "valid input must succeed" coverage even without fixtures. *)
  let synth_ptb =
    try
      let checked = Parser.parse_string "(def main Nat 1)" |> Kernel.check_program in
      [ Canonical_binary.checked_to_binary checked ]
    with _ -> []
  in
  let ptb_seeds = Array.of_list (synth_ptb @ disk_ptb) in
  let patch_seeds = combine seed_patch disk_patch in

  let targets =
    [
      {
        name = "sexp-parser";
        structured = structured_sexp;
        run = run_sexp_parser;
        next = next_sexp_input;
        seeds = sexp_seeds;
      };
      {
        name = "elm-syntax";
        structured = structured_elm;
        run = run_elm;
        next = next_elm_input;
        seeds = elm_seeds;
      };
      {
        name = "canonical-binary-ptb";
        structured = structured_ptb;
        run = run_ptb;
        next = next_ptb_input;
        seeds = ptb_seeds;
      };
      {
        name = "patch-json";
        structured = structured_patch;
        run = run_patch;
        next = next_patch_input;
        seeds = patch_seeds;
      };
      {
        name = "checker";
        structured = structured_check;
        run = run_check;
        next = next_sexp_input;
        seeds = sexp_seeds;
      };
      {
        (* Eval failures are Kernel.Error too (Runtime.fail = Kernel.fail), so the
           checker's structured predicate applies unchanged. *)
        name = "evaluator";
        structured = structured_check;
        run = run_eval;
        next = next_sexp_input;
        seeds = sexp_seeds;
      };
      {
        name = "canonical-roundtrip";
        structured = structured_roundtrip;
        run = run_roundtrip;
        next = next_sexp_input;
        seeds = sexp_seeds;
      };
    ]
  in

  Printf.printf "protoss fuzz: seed=%d iterations/target=%d targets=%d strict=%b\n"
    seed iterations (List.length targets) !strict;
  Printf.printf "  seeds: sexp=%d elm=%d ptb=%d patch=%d (embedded+disk)\n"
    (Array.length sexp_seeds) (Array.length elm_seeds) (Array.length ptb_seeds)
    (Array.length patch_seeds);

  (* Phase 1: replay the curated regression corpus, if found. The corpus lives at
     test/fuzz-corpus in the source tree; the binary may run from the repo root,
     from test/, or from _build, so try both "test/fuzz-corpus" and "fuzz-corpus"
     while walking up from each base (and honor an explicit override). *)
  let corpus_root =
    let candidates = [ Filename.concat "test" "fuzz-corpus"; "fuzz-corpus" ] in
    bases
    |> List.concat_map (fun b ->
           List.filter_map (fun name -> find_up b name) candidates)
    |> List.sort_uniq String.compare
  in
  (match corpus_root with
  | [] ->
      Printf.printf
        "  (no test/fuzz-corpus directory found; replay phase skipped)\n"
  | root :: _ ->
      Printf.printf "  replaying corpus from %s\n" root;
      List.iter (fun t -> replay_corpus t root) targets);

  (* Phase 2: random fuzzing. *)
  List.iter (run_target seed iterations) targets;

  (* Exit code: new (undocumented) crashes always fail; known crashes fail only
     under --strict. The per-signature rollup keeps the summary deterministic and
     bounded even when one bug fires thousands of times. *)
  let all = List.rev !crashes in
  let new_crashes = List.filter (fun c -> not c.known) all in
  let known_crashes = List.filter (fun c -> c.known) all in
  (* Group by (target, normalized signature); keep first-seen origin + sample. *)
  let summarize crashes =
    let tbl : (string, int ref * string * string) Hashtbl.t = Hashtbl.create 16 in
    let order = ref [] in
    List.iter
      (fun c ->
        let key = crash_signature c.target_name c.detail in
        match Hashtbl.find_opt tbl key with
        | Some (count, _, _) -> incr count
        | None ->
            Hashtbl.replace tbl key (ref 1, c.origin, c.detail);
            order := key :: !order)
      crashes;
    List.rev !order
    |> List.map (fun key ->
           let count, origin, detail = Hashtbl.find tbl key in
           (key, !count, origin, detail))
  in
  Printf.printf
    "protoss fuzz: %d iterations/target, %d new crash hit(s), %d known crash hit(s)\n"
    iterations (List.length new_crashes) (List.length known_crashes);
  let print_group label groups =
    if groups <> [] then (
      Printf.printf "  %s (%d distinct signature(s)):\n" label (List.length groups);
      List.iter
        (fun (_key, count, origin, detail) ->
          Printf.printf "    - %s  [x%d, first at %s]\n" detail count origin)
        groups)
  in
  print_group "known (documented, pre-existing) crash signatures"
    (summarize known_crashes);
  print_group "NEW crash signatures (robustness regressions)"
    (summarize new_crashes);
  let fail = new_crashes <> [] || (!strict && known_crashes <> []) in
  if fail then exit 1 else exit 0
