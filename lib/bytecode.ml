(* Protoss VM bytecode format (goal G4).

   ============================================================================
   ARCHITECTURE: stack machine (not register machine). Justification.
   ============================================================================

   Protoss's executable core ([Kernel.cterm]) is a pure, total, call-by-need
   functional IR with De Bruijn variables. For such a language a *stack machine*
   is the natural and simplest target, for several concrete reasons:

   1. De Bruijn locals map directly to stack discipline. [CVar i] is "the i-th
      enclosing binder"; on a stack machine that is a single [Access i] that
      reaches [i] slots down the environment/stack. There is no register
      allocation problem to solve, and no need to invent a virtual-register
      naming scheme that would itself have to be made canonical/deterministic.

   2. Determinism is free. A register machine forces a register-allocation pass,
      and any allocator choice (which value lands in which register) leaks into
      the emitted code. Two source programs that are alpha/eta-equal canonical
      graphs could then produce *different* register assignments and therefore
      different bytecode bytes — exactly the kind of non-determinism this project
      treats as a bug. A stack machine has no such freedom: the instruction
      sequence is a deterministic fold over the (already canonical) [cterm], so
      identical canonical terms always yield identical bytecode.

   3. The evaluator the VM will replace ([Runtime.eval_app], a tree-walk over
      [cterm]) is itself stack-shaped (it threads an environment list and
      substitutes De Bruijn terms). A stack VM is the smallest possible step
      away from that interpreter, which keeps G5 (the executor) tractable and
      keeps the translation auditable.

   4. The structured / "tree" nodes of the IR (records, list literals, the whole
      [View] sub-language: text/image/button/input/column/row/list/when/node/
      attr/on, and the [Process] nodes done/bind/request) are most naturally
      built by pushing their already-evaluated children and then emitting a
      single "construct this node" opcode. That is the canonical stack-machine
      idiom (push args, apply constructor) and avoids materialising intermediate
      named temporaries that a register target would require.

   We therefore compile each [cterm] into a flat [instruction array] that, when
   executed against an environment stack, leaves exactly one value (the term's
   result) on top of the operand stack. Binders ([CLambda], [CLet], the [CVar]
   bound by [CCaseList]/[CBind], and the implicit binders introduced by folds
   via [CRecur]) push onto the environment; [CVar i] reads slot [i] counted from
   the innermost binder, matching the kernel's De Bruijn convention exactly (see
   [Kernel.canonical_expr]: [CLet] pushes the bound value, [CCaseList] pushes
   head then tail so the cons body sees [#0 = head], [#1 = tail], and [CBind]
   pushes the awaited value).

   This module defines ONLY the format, its deterministic (de)serialization, a
   content hash, and the [cterm -> instruction list] compiler. There is no
   executor here; running bytecode is goal G5.

   ============================================================================
   ENCODING: deterministic, portable, no Marshal.
   ============================================================================

   Everything is encoded into a [string] (= immutable bytes) with:
     - integers as fixed-width big-endian (u8 / u16 / u32 / i64 BE),
     - strings length-prefixed (u32 BE length, then the raw UTF-8/byte content),
     - lists length-prefixed (u32 BE count, then each element),
     - every variant tagged by a stable u8 opcode/tag byte.
   No host-endianness, no native word size, no [Marshal] (which is neither
   portable nor canonical). Field/element order is whatever the canonical
   [cterm] already fixes (record fields are kept in the order the canonicalizer
   produced — [Ast.sort_fields] in [Kernel.canonical_expr] — and case branches
   in the order [Kernel.canonical_branches] sorted them); we never re-sort here,
   so encode is a pure structural fold and decode is its exact inverse.

   The byte layout of every construct is documented inline next to its
   tag-byte constant below. encode/decode are inverse by construction: each
   encoder writes [tag :: payload] and the matching decoder reads the same. *)

(* We deliberately route every "form we do not (yet) lower" through
   [Kernel_error.fail] with the required "bytecode: unsupported form: ..."
   prefix, as mandated by the task. NOTE for integrators: [Kernel_error.Error]
   and [Kernel.Error] are *distinct* exceptions (kernel.ml declares its own
   [exception Error]); we use [Kernel_error] here per the task spec. The current
   compiler reaches none of these on the executable fragment — every [cterm] and
   [cbranch] constructor is really lowered — but they are kept as explicit,
   non-wildcard guards so that if the kernel grows a new node the match becomes
   non-exhaustive at compile time (caught by the build) rather than silently
   mis-compiling. *)
let unsupported what = Kernel_error.fail ("bytecode: unsupported form: " ^ what)

(* ------------------------------------------------------------------------- *)
(* Instruction set                                                            *)
(* ------------------------------------------------------------------------- *)

(* Operand-stack machine. Convention: after executing the code compiled from a
   [cterm], exactly one fresh value sits on top of the operand stack. Children
   are evaluated/pushed left-to-right (matching the kernel's structural order)
   and then a single constructor/eliminator opcode consumes them.

   The environment (lexical De Bruijn frame) is conceptually a separate stack of
   bound values; [PushVar] reads it, [EnterBinder]/[LeaveBinder] bracket the
   regions where [CLambda]/[CLet]/[CCaseList]/[CBind] (and fold step bodies)
   extend it. The executor (G5) decides the concrete representation; the
   bytecode only records the structure. *)

type instruction =
  (* Literals. *)
  | PushUnit                              (* CUnit *)
  | PushBool of bool                      (* CBool *)
  | PushNat of int                        (* CNat (non-negative kernel Nat) *)
  | PushString of string                  (* CString *)
  (* Variables / globals. *)
  | PushVar of int                        (* CVar i  -- De Bruijn index *)
  | PushGlobal of string                  (* CGlobal n, n not a builtin *)
  | PushBuiltin of string                 (* CGlobal n, n a kernel builtin *)
  | PushInst of string * Ast.typ list     (* CInst (n, type args): instantiate a
                                             polymorphic global at given types *)
  (* Functions. The body of a lambda is a nested instruction block compiled in
     an environment with one extra binder (its parameter). Keeping the body as
     its own block (rather than inlining with jumps) preserves the tree shape
     and keeps encoding a structural fold. *)
  | MakeClosure of Ast.typ * block        (* CLambda (param_ty, body) *)
  | Apply                                  (* CApp: stack [.. fn arg] -> [.. res] *)
  (* Sequencing / evaluation control. *)
  | Force                                  (* CStrict: force WHNF of top of stack *)
  | LetBind of block                       (* CLet: top of stack becomes new
                                             innermost binder while [block]
                                             (the let body) runs; result of
                                             [block] replaces it. *)
  (* Records. *)
  | MakeRecord of string list             (* CRecord: field labels, in canonical
                                             order; that many values are popped
                                             (top = last label) into a record *)
  | Project of string                     (* CField: project a labelled field *)
  (* Variants. *)
  | MakeVariant of Ast.typ * string       (* CVariant (ty, constructor): wrap top
                                             of stack as that constructor *)
  | Case of casebranch list               (* CCase: scrutinee on top of stack;
                                             dispatch on bool/constructor *)
  (* Nat recursion. NOTE on binders: the canonicalizer does NOT introduce a De
     Bruijn binder for fold arms (see [Kernel.canonical_expr]: zero/step are
     compiled in the SAME [env] as the fold). The running accumulator is threaded
     by the executor through [Recur], which is a structural marker, not a stack
     slot. So [zero]/[step] blocks run in the enclosing lexical environment with
     no extra binder; their [CVar] indices are unchanged. *)
  | FoldNat of block * block              (* CFoldNat (n, zero, step): n on stack;
                                             [zero] computes the base case, [step]
                                             the per-iteration body containing the
                                             [Recur] occurrence(s). *)
  (* Variant recursion (structural fold over a recursive variant). Branch bodies
     bind the constructor payload (one binder) like ordinary [Case] branches;
     recursive sub-results are reached via [Recur]. *)
  | FoldVariant of Ast.typ * Ast.typ * casebranch list
                                          (* CFoldVariant (target, result, scrut,
                                             branches): scrutinee on top. *)
  | Recur                                  (* CRecur: feed top-of-stack to the
                                             enclosing fold's recursive occurrence
                                             (executor-defined; G5) *)
  (* Lists. *)
  | Nil of Ast.typ                        (* CNil item_ty *)
  | Cons of Ast.typ                       (* CCons item_ty: stack [.. head tail]
                                             -> [.. (head :: tail)] *)
  | FoldList of block * block             (* CFoldList (xs, zero, step): xs on
                                             stack; [zero] base, [step] body (no
                                             extra binder, [Recur] threads the
                                             accumulator — same as FoldNat) *)
  | CaseList of block * block             (* CCaseList (xs, nil_body, cons_body):
                                             xs on stack; nil_body for [], or
                                             cons_body run with two fresh binders
                                             pushed (head then tail; cons body
                                             thus sees #0=head, #1=tail) *)
  (* Streams (productive coinduction). *)
  | Coiter of Ast.typ * Ast.typ           (* CCoiter (state_ty, item_ty, seed,
                                             step): stack [.. seed step] *)
  | StreamHead                             (* CStreamHead: stack [.. s] -> head *)
  | StreamTail                             (* CStreamTail: stack [.. s] -> tail *)
  | StreamTake                             (* CStreamTake: stack [.. count s] ->
                                             list of first [count] elements *)
  (* Mealy automata. *)
  | MakeAutomaton of Ast.typ * Ast.typ    (* CAutomaton (state_ty, output_ty,
                                             initial, transition): stack
                                             [.. initial transition] *)
  | AutomatonRun                           (* CAutomatonRun: stack [.. count a]
                                             -> list of outputs *)
  (* View sub-language. Each takes its already-evaluated children off the stack
     (left-to-right push order) and builds the corresponding view node. *)
  | ViewText                               (* CText:   [.. s] *)
  | ViewImage                              (* CImage:  [.. src alt] *)
  | ViewButton                             (* CButton: [.. label msg] *)
  | ViewInput                              (* CInput:  [.. value handler] *)
  | ViewColumn                             (* CColumn: [.. children] *)
  | ViewRow                                (* CRow:    [.. children] *)
  | ViewList                               (* CListView: [.. items render] *)
  | ViewWhen                               (* CWhenView: [.. cond view] *)
  | ViewNode                               (* CNode:   [.. tag attrs children] *)
  | ViewAttr                               (* CAttr:   [.. name value] *)
  | ViewOn                                 (* COn:     [.. event msg] *)
  (* Process sub-language. *)
  | ProcDone                               (* CDone:   [.. a] -> done a *)
  | ProcRequest of Ast.req                 (* CRequest req: a primitive effect *)
  | ProcBind of Ast.typ * block            (* CBind (p, awaited_ty, body): [p] on
                                             stack; its result is pushed as a
                                             fresh binder while [body] runs *)

(* A self-contained code block: a flat instruction sequence that nets +1 value
   on the operand stack. Blocks are used for lambda bodies, let/bind bodies, and
   fold base/step arms so the bytecode mirrors the [cterm] tree exactly. *)
and block = instruction array

(* Compiled case/fold branch. Mirrors [Kernel.cbranch]:
   - [BoolBranch (v, body)] for [CBBool (v, _)];
   - [VariantBranch (con, body)] for [CBVariant (con, _)]. The variant payload
     is bound as one fresh innermost binder visible to [body] (matching
     [Kernel.canonical_branches], which compiles [BVariant (con, x, e)] under
     [(x :: env)]). Bool branches bind nothing. *)
and casebranch =
  | BoolBranch of bool * block
  | VariantBranch of string * block

(* A compiled definition: name + def-id + declared type + compiled body block.
   Mirrors [Kernel.canonical_def] one-to-one so a [module_] can be produced
   directly from [Kernel.canonical_def list]. *)
type bc_def = {
  bc_name : string;
  bc_def_id : string;
  bc_typ : Ast.typ;
  bc_code : block;
}

(* A bytecode module: the format version tag, exported entry names (kept in the
   order given; we do not reorder so the caller controls canonical order), and
   the compiled definitions in their given order. *)
type module_ = {
  bc_version : string;
  bc_exports : string list;
  bc_defs : bc_def list;
}

(* Program-level alias: a Protoss "program" lowers to exactly one bytecode
   module. Kept as a distinct name for clarity at call sites and to leave room
   for future multi-module linking without breaking the module type. *)
type program = module_

let bytecode_version = "protoss-bytecode-v1"

(* ------------------------------------------------------------------------- *)
(* Compiler: cterm -> block                                                   *)
(* ------------------------------------------------------------------------- *)

(* The compiler is a pure structural fold. It allocates no names and consults no
   environment: De Bruijn indices already encode binding, so a [CVar i] becomes
   [PushVar i] verbatim and binder-introducing nodes simply compile their body
   into a nested block. Determinism follows: the output is a function of the
   (canonical) input only.

   Each [compile_term] call returns the instruction list that evaluates the term
   and nets +1 on the operand stack. [compile_block] wraps that as a [block]. *)

let rec compile_term (t : Kernel.cterm) : instruction list =
  match t with
  (* Literals. *)
  | Kernel.CUnit -> [ PushUnit ]
  | Kernel.CBool b -> [ PushBool b ]
  | Kernel.CNat n -> [ PushNat n ]
  | Kernel.CString s -> [ PushString s ]
  (* Variables and globals. We split builtin vs. user global up front (mirroring
     [cterm_to_canonical_v2], which treats [is_builtin] specially) so the
     executor never has to re-test names at run time. *)
  | Kernel.CVar i -> [ PushVar i ]
  | Kernel.CGlobal n -> if Kernel.is_builtin n then [ PushBuiltin n ] else [ PushGlobal n ]
  | Kernel.CInst (name, type_args) -> [ PushInst (name, type_args) ]
  (* Functions. *)
  | Kernel.CLambda (param_ty, body) -> [ MakeClosure (param_ty, compile_block body) ]
  | Kernel.CApp (f, x) ->
      (* Push function, then argument, then apply: [.. f x] -> [.. (f x)]. *)
      compile_term f @ compile_term x @ [ Apply ]
  | Kernel.CStrict e -> compile_term e @ [ Force ]
  | Kernel.CLet (bound, body) ->
      (* Evaluate the bound term, then run the body as a block under one extra
         binder. [LetBind] brackets that scope. *)
      compile_term bound @ [ LetBind (compile_block body) ]
  (* Records. Fields are already in canonical order; push each value in order,
     then build the record from the (ordered) label list. *)
  | Kernel.CRecord fields ->
      let labels = List.map fst fields in
      List.concat_map (fun (_, e) -> compile_term e) fields @ [ MakeRecord labels ]
  | Kernel.CField (e, field) -> compile_term e @ [ Project field ]
  (* Variants. *)
  | Kernel.CVariant (ty, con, e) -> compile_term e @ [ MakeVariant (ty, con) ]
  | Kernel.CCase (scrut, branches) ->
      compile_term scrut @ [ Case (List.map compile_branch branches) ]
  (* Nat recursion: push the Nat to recurse on, then carry base/step as blocks.
     The [step] body refers to the running accumulator through [CRecur], which
     compiles to [Recur]; we do not flatten the iteration here (that is the
     executor's job in G5). *)
  | Kernel.CFoldNat (n, zero, step) ->
      compile_term n @ [ FoldNat (compile_block zero, compile_block step) ]
  | Kernel.CFoldVariant (target, result, scrut, branches) ->
      compile_term scrut @ [ FoldVariant (target, result, List.map compile_branch branches) ]
  | Kernel.CRecur e -> compile_term e @ [ Recur ]
  (* Lists. *)
  | Kernel.CNil item_ty -> [ Nil item_ty ]
  | Kernel.CCons (item_ty, head, tail) -> compile_term head @ compile_term tail @ [ Cons item_ty ]
  | Kernel.CFoldList (xs, zero, step) ->
      compile_term xs @ [ FoldList (compile_block zero, compile_block step) ]
  | Kernel.CCaseList (xs, nil_body, cons_body) ->
      (* [cons_body] is canonicalized under (head :: tail :: env) — i.e. it sees
         two fresh binders. We compile it as a block; [CaseList] introduces those
         two binders (head then tail) before running it. *)
      compile_term xs @ [ CaseList (compile_block nil_body, compile_block cons_body) ]
  (* Streams. *)
  | Kernel.CCoiter (state_ty, item_ty, seed, step) ->
      compile_term seed @ compile_term step @ [ Coiter (state_ty, item_ty) ]
  | Kernel.CStreamHead s -> compile_term s @ [ StreamHead ]
  | Kernel.CStreamTail s -> compile_term s @ [ StreamTail ]
  | Kernel.CStreamTake (count, s) -> compile_term count @ compile_term s @ [ StreamTake ]
  (* Automata. *)
  | Kernel.CAutomaton (state_ty, output_ty, initial, transition) ->
      compile_term initial @ compile_term transition @ [ MakeAutomaton (state_ty, output_ty) ]
  | Kernel.CAutomatonRun (count, automaton) ->
      compile_term count @ compile_term automaton @ [ AutomatonRun ]
  (* View nodes: push children left-to-right, then the node opcode. *)
  | Kernel.CText e -> compile_term e @ [ ViewText ]
  | Kernel.CImage (src, alt) -> compile_term src @ compile_term alt @ [ ViewImage ]
  | Kernel.CButton (label, msg) -> compile_term label @ compile_term msg @ [ ViewButton ]
  | Kernel.CInput (value, handler) -> compile_term value @ compile_term handler @ [ ViewInput ]
  | Kernel.CColumn children -> compile_term children @ [ ViewColumn ]
  | Kernel.CRow children -> compile_term children @ [ ViewRow ]
  | Kernel.CListView (items, render) -> compile_term items @ compile_term render @ [ ViewList ]
  | Kernel.CWhenView (cond, view) -> compile_term cond @ compile_term view @ [ ViewWhen ]
  | Kernel.CNode (tag, attrs, children) ->
      compile_term tag @ compile_term attrs @ compile_term children @ [ ViewNode ]
  | Kernel.CAttr (name, value) -> compile_term name @ compile_term value @ [ ViewAttr ]
  | Kernel.COn (event, msg) -> compile_term event @ compile_term msg @ [ ViewOn ]
  (* Process nodes. *)
  | Kernel.CDone e -> compile_term e @ [ ProcDone ]
  | Kernel.CRequest req -> [ ProcRequest req ]
  | Kernel.CBind (p, awaited_ty, body) ->
      compile_term p @ [ ProcBind (awaited_ty, compile_block body) ]

and compile_block (t : Kernel.cterm) : block = Array.of_list (compile_term t)

and compile_branch (b : Kernel.cbranch) : casebranch =
  match b with
  | Kernel.CBBool (v, body) -> BoolBranch (v, compile_block body)
  | Kernel.CBVariant (con, body) -> VariantBranch (con, compile_block body)

(* Public entry: compile a single canonical term to a block. *)
let compile (t : Kernel.cterm) : block = compile_block t

(* Compile one canonical def. *)
let compile_def (d : Kernel.canonical_def) : bc_def =
  {
    bc_name = d.Kernel.cname;
    bc_def_id = d.Kernel.cdef_id;
    bc_typ = d.Kernel.ctyp;
    bc_code = compile_block d.Kernel.cbody;
  }

(* Compile a whole program (the canonical defs + the program's exported names)
   into one bytecode module. Order is preserved exactly as given by the caller;
   we never reorder, so the caller (which already holds the canonical order from
   [Kernel.serialize_program] / [Kernel.checked]) controls determinism. *)
let compile_program ~(exports : string list) (defs : Kernel.canonical_def list) : module_ =
  { bc_version = bytecode_version; bc_exports = exports; bc_defs = List.map compile_def defs }

(* Convenience: build the module straight from a [Kernel.checked]. Uses the
   program's declared exports when present, else every def name in order. *)
let compile_checked (c : Kernel.checked) : module_ =
  let defs = Kernel.canonical_defs_of_checked c in
  let exports =
    match c.Kernel.program.Ast.exports with
    | Some names -> names
    | None -> List.map (fun (d : Kernel.canonical_def) -> d.Kernel.cname) defs
  in
  compile_program ~exports defs

(* ------------------------------------------------------------------------- *)
(* Deterministic binary encoding                                              *)
(* ------------------------------------------------------------------------- *)

(* --- primitive writers (all big-endian, fixed width) --- *)

let put_u8 buf n =
  (* 0 <= n < 256 *)
  Buffer.add_char buf (Char.chr (n land 0xff))

let put_u32 buf n =
  (* n is a non-negative OCaml int that fits in 32 bits. Big-endian. *)
  Buffer.add_char buf (Char.chr ((n lsr 24) land 0xff));
  Buffer.add_char buf (Char.chr ((n lsr 16) land 0xff));
  Buffer.add_char buf (Char.chr ((n lsr 8) land 0xff));
  Buffer.add_char buf (Char.chr (n land 0xff))

(* Signed 64-bit big-endian. Used for [CNat]: kernel Nats are non-negative
   OCaml ints, but a full i64 keeps the format width-stable regardless of the
   host's int size and round-trips any value [int_of_*] can hold. *)
let put_i64 buf (n : int) =
  let n64 = Int64.of_int n in
  for i = 0 to 7 do
    let shift = (7 - i) * 8 in
    Buffer.add_char buf (Char.chr (Int64.(to_int (logand (shift_right_logical n64 shift) 0xffL))))
  done

(* Length-prefixed string: u32 BE byte length, then the raw bytes. *)
let put_str buf s =
  put_u32 buf (String.length s);
  Buffer.add_string buf s

(* Length-prefixed list: u32 BE count, then each element via [f]. *)
let put_list buf f xs =
  put_u32 buf (List.length xs);
  List.iter (f buf) xs

(* --- type tag bytes (mirror the [Ast.typ] constructors) ---

   Layout of an encoded type is [tag :: payload]:
     0x00 TUnit                ()                       -> (no payload)
     0x01 TBool                ()                       -> (no payload)
     0x02 TNat                 ()                       -> (no payload)
     0x03 TString              ()                       -> (no payload)
     0x04 TFun (a,b)           -> type a, type b
     0x05 TRecord fields       -> list[ str label, type ]
     0x06 TVariant cases       -> list[ str con,   type ]
     0x07 TList t              -> type t
     0x08 TView t              -> type t
     0x09 TAttr t              -> type t
     0x0a TStream t            -> type t
     0x0b TAutomaton (s,o)     -> type s, type o
     0x0c TProcess (caps,t)    -> opt(list str) caps, type t
     0x0d TCmd (caps,t)        -> opt(list str) caps, type t
     0x0e TSecretRef (scope,t) -> str scope, type t
     0x0f TVar i               -> u32 i
     0x10 TForall (arity,body) -> u32 arity, type body
     0x11 TNamed (n,args)      -> str n, list[ type ]
   The [caps] option is encoded as: 0x00 = None, 0x01 then list[str] = Some. *)

let put_caps_opt buf = function
  | None -> put_u8 buf 0x00
  | Some caps ->
      put_u8 buf 0x01;
      put_list buf put_str caps

let rec put_typ buf (ty : Ast.typ) =
  match ty with
  | Ast.TUnit -> put_u8 buf 0x00
  | Ast.TBool -> put_u8 buf 0x01
  | Ast.TNat -> put_u8 buf 0x02
  | Ast.TString -> put_u8 buf 0x03
  | Ast.TFun (a, b) ->
      put_u8 buf 0x04;
      put_typ buf a;
      put_typ buf b
  | Ast.TRecord fields ->
      put_u8 buf 0x05;
      put_list buf (fun b (label, t) -> put_str b label; put_typ b t) fields
  | Ast.TVariant cases ->
      put_u8 buf 0x06;
      put_list buf (fun b (con, t) -> put_str b con; put_typ b t) cases
  | Ast.TList t ->
      put_u8 buf 0x07;
      put_typ buf t
  | Ast.TView t ->
      put_u8 buf 0x08;
      put_typ buf t
  | Ast.TAttr t ->
      put_u8 buf 0x09;
      put_typ buf t
  | Ast.TStream t ->
      put_u8 buf 0x0a;
      put_typ buf t
  | Ast.TAutomaton (s, o) ->
      put_u8 buf 0x0b;
      put_typ buf s;
      put_typ buf o
  | Ast.TProcess (caps, t) ->
      put_u8 buf 0x0c;
      put_caps_opt buf caps;
      put_typ buf t
  | Ast.TCmd (caps, t) ->
      put_u8 buf 0x0d;
      put_caps_opt buf caps;
      put_typ buf t
  | Ast.TSecretRef (scope, t) ->
      put_u8 buf 0x0e;
      put_str buf scope;
      put_typ buf t
  | Ast.TVar i ->
      put_u8 buf 0x0f;
      put_u32 buf i
  | Ast.TForall (arity, body) ->
      put_u8 buf 0x10;
      put_u32 buf arity;
      put_typ buf body
  | Ast.TNamed (n, args) ->
      put_u8 buf 0x11;
      put_str buf n;
      put_list buf put_typ args

(* --- request tag bytes (mirror [Ast.req]) ---

   The constructor argument order is preserved exactly as declared in ast.ml:
     0x00 AskHuman prompt              -> str prompt
     0x01 HttpGet url                  -> str url
     0x02 ReadClock                    -> (no payload)
     0x03 SaveLocal (key,value)        -> str key, str value
     0x04 LoadLocal key                -> str key
     0x05 ServerRequest (route,payload)-> str route, str payload
   (Note: [ServerRequest of string * string] is (route, payload) in ast.ml —
   we keep that field order, independent of the sorted record payload type.) *)

let put_req buf (r : Ast.req) =
  match r with
  | Ast.AskHuman prompt ->
      put_u8 buf 0x00;
      put_str buf prompt
  | Ast.HttpGet url ->
      put_u8 buf 0x01;
      put_str buf url
  | Ast.ReadClock -> put_u8 buf 0x02
  | Ast.SaveLocal (key, value) ->
      put_u8 buf 0x03;
      put_str buf key;
      put_str buf value
  | Ast.LoadLocal key ->
      put_u8 buf 0x04;
      put_str buf key
  | Ast.ServerRequest (route, payload) ->
      put_u8 buf 0x05;
      put_str buf route;
      put_str buf payload

(* --- instruction opcodes ---

   Each instruction is [opcode :: operands]. Opcodes are stable; do not
   renumber (that would change every module's bytes and hash). Nested blocks are
   length-prefixed (u32 instruction count, then each instruction). *)

let op_push_unit       = 0x00
let op_push_bool       = 0x01
let op_push_nat        = 0x02
let op_push_string     = 0x03
let op_push_var        = 0x04
let op_push_global     = 0x05
let op_push_builtin    = 0x06
let op_push_inst       = 0x07
let op_make_closure    = 0x08
let op_apply           = 0x09
let op_force           = 0x0a
let op_let_bind        = 0x0b
let op_make_record     = 0x0c
let op_project         = 0x0d
let op_make_variant    = 0x0e
let op_case            = 0x0f
let op_fold_nat        = 0x10
let op_fold_variant    = 0x11
let op_recur           = 0x12
let op_nil             = 0x13
let op_cons            = 0x14
let op_fold_list       = 0x15
let op_case_list       = 0x16
let op_coiter          = 0x17
let op_stream_head     = 0x18
let op_stream_tail     = 0x19
let op_stream_take     = 0x1a
let op_make_automaton  = 0x1b
let op_automaton_run   = 0x1c
let op_view_text       = 0x1d
let op_view_image      = 0x1e
let op_view_button     = 0x1f
let op_view_input      = 0x20
let op_view_column     = 0x21
let op_view_row        = 0x22
let op_view_list       = 0x23
let op_view_when       = 0x24
let op_view_node       = 0x25
let op_view_attr       = 0x26
let op_view_on         = 0x27
let op_proc_done       = 0x28
let op_proc_request    = 0x29
let op_proc_bind       = 0x2a

(* Case-branch tag bytes. *)
let br_bool    = 0x00
let br_variant = 0x01

let rec put_instr buf (i : instruction) =
  match i with
  | PushUnit -> put_u8 buf op_push_unit
  | PushBool b ->
      put_u8 buf op_push_bool;
      put_u8 buf (if b then 1 else 0)
  | PushNat n ->
      put_u8 buf op_push_nat;
      put_i64 buf n
  | PushString s ->
      put_u8 buf op_push_string;
      put_str buf s
  | PushVar idx ->
      put_u8 buf op_push_var;
      put_u32 buf idx
  | PushGlobal n ->
      put_u8 buf op_push_global;
      put_str buf n
  | PushBuiltin n ->
      put_u8 buf op_push_builtin;
      put_str buf n
  | PushInst (n, type_args) ->
      put_u8 buf op_push_inst;
      put_str buf n;
      put_list buf put_typ type_args
  | MakeClosure (param_ty, body) ->
      put_u8 buf op_make_closure;
      put_typ buf param_ty;
      put_block buf body
  | Apply -> put_u8 buf op_apply
  | Force -> put_u8 buf op_force
  | LetBind body ->
      put_u8 buf op_let_bind;
      put_block buf body
  | MakeRecord labels ->
      put_u8 buf op_make_record;
      put_list buf put_str labels
  | Project field ->
      put_u8 buf op_project;
      put_str buf field
  | MakeVariant (ty, con) ->
      put_u8 buf op_make_variant;
      put_typ buf ty;
      put_str buf con
  | Case branches ->
      put_u8 buf op_case;
      put_list buf put_branch branches
  | FoldNat (zero, step) ->
      put_u8 buf op_fold_nat;
      put_block buf zero;
      put_block buf step
  | FoldVariant (target, result, branches) ->
      put_u8 buf op_fold_variant;
      put_typ buf target;
      put_typ buf result;
      put_list buf put_branch branches
  | Recur -> put_u8 buf op_recur
  | Nil item_ty ->
      put_u8 buf op_nil;
      put_typ buf item_ty
  | Cons item_ty ->
      put_u8 buf op_cons;
      put_typ buf item_ty
  | FoldList (zero, step) ->
      put_u8 buf op_fold_list;
      put_block buf zero;
      put_block buf step
  | CaseList (nil_body, cons_body) ->
      put_u8 buf op_case_list;
      put_block buf nil_body;
      put_block buf cons_body
  | Coiter (state_ty, item_ty) ->
      put_u8 buf op_coiter;
      put_typ buf state_ty;
      put_typ buf item_ty
  | StreamHead -> put_u8 buf op_stream_head
  | StreamTail -> put_u8 buf op_stream_tail
  | StreamTake -> put_u8 buf op_stream_take
  | MakeAutomaton (state_ty, output_ty) ->
      put_u8 buf op_make_automaton;
      put_typ buf state_ty;
      put_typ buf output_ty
  | AutomatonRun -> put_u8 buf op_automaton_run
  | ViewText -> put_u8 buf op_view_text
  | ViewImage -> put_u8 buf op_view_image
  | ViewButton -> put_u8 buf op_view_button
  | ViewInput -> put_u8 buf op_view_input
  | ViewColumn -> put_u8 buf op_view_column
  | ViewRow -> put_u8 buf op_view_row
  | ViewList -> put_u8 buf op_view_list
  | ViewWhen -> put_u8 buf op_view_when
  | ViewNode -> put_u8 buf op_view_node
  | ViewAttr -> put_u8 buf op_view_attr
  | ViewOn -> put_u8 buf op_view_on
  | ProcDone -> put_u8 buf op_proc_done
  | ProcRequest req ->
      put_u8 buf op_proc_request;
      put_req buf req
  | ProcBind (awaited_ty, body) ->
      put_u8 buf op_proc_bind;
      put_typ buf awaited_ty;
      put_block buf body

and put_branch buf (b : casebranch) =
  match b with
  | BoolBranch (v, body) ->
      put_u8 buf br_bool;
      put_u8 buf (if v then 1 else 0);
      put_block buf body
  | VariantBranch (con, body) ->
      put_u8 buf br_variant;
      put_str buf con;
      put_block buf body

and put_block buf (blk : block) =
  put_u32 buf (Array.length blk);
  Array.iter (put_instr buf) blk

let put_def buf (d : bc_def) =
  put_str buf d.bc_name;
  put_str buf d.bc_def_id;
  put_typ buf d.bc_typ;
  put_block buf d.bc_code

(* A module is: magic "PBC1", then version string, exports list, defs list. The
   4-byte ASCII magic makes mis-decodes of foreign bytes fail fast. *)
let magic = "PBC1"

let encode_module (m : module_) : string =
  let buf = Buffer.create 4096 in
  Buffer.add_string buf magic;
  put_str buf m.bc_version;
  put_list buf put_str m.bc_exports;
  put_list buf put_def m.bc_defs;
  Buffer.contents buf

(* ------------------------------------------------------------------------- *)
(* Deterministic binary decoding (exact inverse of the encoders above)        *)
(* ------------------------------------------------------------------------- *)

(* A tiny cursor over the source string. Every reader advances [pos]; bounds are
   checked so a truncated/corrupt buffer fails with a clear error rather than an
   [Invalid_argument] from [String.get]. *)
type cursor = { src : string; mutable pos : int }

let need cur n =
  if cur.pos + n > String.length cur.src then
    Kernel_error.fail "bytecode: truncated input while decoding"

let get_u8 cur =
  need cur 1;
  let c = Char.code (String.get cur.src cur.pos) in
  cur.pos <- cur.pos + 1;
  c

let get_u32 cur =
  need cur 4;
  let b0 = Char.code (String.get cur.src cur.pos) in
  let b1 = Char.code (String.get cur.src (cur.pos + 1)) in
  let b2 = Char.code (String.get cur.src (cur.pos + 2)) in
  let b3 = Char.code (String.get cur.src (cur.pos + 3)) in
  cur.pos <- cur.pos + 4;
  (b0 lsl 24) lor (b1 lsl 16) lor (b2 lsl 8) lor b3

let get_i64 cur =
  need cur 8;
  let acc = ref 0L in
  for i = 0 to 7 do
    let b = Int64.of_int (Char.code (String.get cur.src (cur.pos + i))) in
    acc := Int64.logor (Int64.shift_left !acc 8) b
  done;
  cur.pos <- cur.pos + 8;
  Int64.to_int !acc

let get_str cur =
  let len = get_u32 cur in
  need cur len;
  let s = String.sub cur.src cur.pos len in
  cur.pos <- cur.pos + len;
  s

let get_list cur f =
  let n = get_u32 cur in
  (* Build left-to-right via an explicit cursor loop so element order matches the
     encoder's [List.iter] order exactly. *)
  let rec loop i acc = if i >= n then List.rev acc else loop (i + 1) (f cur :: acc) in
  loop 0 []

let get_caps_opt cur =
  match get_u8 cur with
  | 0x00 -> None
  | 0x01 -> Some (get_list cur get_str)
  | tag -> Kernel_error.fail ("bytecode: bad capability-option tag " ^ string_of_int tag)

let rec get_typ cur : Ast.typ =
  match get_u8 cur with
  | 0x00 -> Ast.TUnit
  | 0x01 -> Ast.TBool
  | 0x02 -> Ast.TNat
  | 0x03 -> Ast.TString
  | 0x04 ->
      let a = get_typ cur in
      let b = get_typ cur in
      Ast.TFun (a, b)
  | 0x05 ->
      let fields = get_list cur (fun c -> let label = get_str c in let t = get_typ c in (label, t)) in
      Ast.TRecord fields
  | 0x06 ->
      let cases = get_list cur (fun c -> let con = get_str c in let t = get_typ c in (con, t)) in
      Ast.TVariant cases
  | 0x07 -> Ast.TList (get_typ cur)
  | 0x08 -> Ast.TView (get_typ cur)
  | 0x09 -> Ast.TAttr (get_typ cur)
  | 0x0a -> Ast.TStream (get_typ cur)
  | 0x0b ->
      let s = get_typ cur in
      let o = get_typ cur in
      Ast.TAutomaton (s, o)
  | 0x0c ->
      let caps = get_caps_opt cur in
      let t = get_typ cur in
      Ast.TProcess (caps, t)
  | 0x0d ->
      let caps = get_caps_opt cur in
      let t = get_typ cur in
      Ast.TCmd (caps, t)
  | 0x0e ->
      let scope = get_str cur in
      let t = get_typ cur in
      Ast.TSecretRef (scope, t)
  | 0x0f -> Ast.TVar (get_u32 cur)
  | 0x10 ->
      let arity = get_u32 cur in
      let body = get_typ cur in
      Ast.TForall (arity, body)
  | 0x11 ->
      let n = get_str cur in
      let args = get_list cur get_typ in
      Ast.TNamed (n, args)
  | tag -> Kernel_error.fail ("bytecode: bad type tag " ^ string_of_int tag)

let get_req cur : Ast.req =
  match get_u8 cur with
  | 0x00 -> Ast.AskHuman (get_str cur)
  | 0x01 -> Ast.HttpGet (get_str cur)
  | 0x02 -> Ast.ReadClock
  | 0x03 ->
      let key = get_str cur in
      let value = get_str cur in
      Ast.SaveLocal (key, value)
  | 0x04 -> Ast.LoadLocal (get_str cur)
  | 0x05 ->
      let route = get_str cur in
      let payload = get_str cur in
      Ast.ServerRequest (route, payload)
  | tag -> Kernel_error.fail ("bytecode: bad request tag " ^ string_of_int tag)

let rec get_instr cur : instruction =
  let op = get_u8 cur in
  if op = op_push_unit then PushUnit
  else if op = op_push_bool then PushBool (get_u8 cur <> 0)
  else if op = op_push_nat then PushNat (get_i64 cur)
  else if op = op_push_string then PushString (get_str cur)
  else if op = op_push_var then PushVar (get_u32 cur)
  else if op = op_push_global then PushGlobal (get_str cur)
  else if op = op_push_builtin then PushBuiltin (get_str cur)
  else if op = op_push_inst then (
    let n = get_str cur in
    let type_args = get_list cur get_typ in
    PushInst (n, type_args))
  else if op = op_make_closure then (
    let param_ty = get_typ cur in
    let body = get_block cur in
    MakeClosure (param_ty, body))
  else if op = op_apply then Apply
  else if op = op_force then Force
  else if op = op_let_bind then LetBind (get_block cur)
  else if op = op_make_record then MakeRecord (get_list cur get_str)
  else if op = op_project then Project (get_str cur)
  else if op = op_make_variant then (
    let ty = get_typ cur in
    let con = get_str cur in
    MakeVariant (ty, con))
  else if op = op_case then Case (get_list cur get_branch)
  else if op = op_fold_nat then (
    let zero = get_block cur in
    let step = get_block cur in
    FoldNat (zero, step))
  else if op = op_fold_variant then (
    let target = get_typ cur in
    let result = get_typ cur in
    let branches = get_list cur get_branch in
    FoldVariant (target, result, branches))
  else if op = op_recur then Recur
  else if op = op_nil then Nil (get_typ cur)
  else if op = op_cons then Cons (get_typ cur)
  else if op = op_fold_list then (
    let zero = get_block cur in
    let step = get_block cur in
    FoldList (zero, step))
  else if op = op_case_list then (
    let nil_body = get_block cur in
    let cons_body = get_block cur in
    CaseList (nil_body, cons_body))
  else if op = op_coiter then (
    let state_ty = get_typ cur in
    let item_ty = get_typ cur in
    Coiter (state_ty, item_ty))
  else if op = op_stream_head then StreamHead
  else if op = op_stream_tail then StreamTail
  else if op = op_stream_take then StreamTake
  else if op = op_make_automaton then (
    let state_ty = get_typ cur in
    let output_ty = get_typ cur in
    MakeAutomaton (state_ty, output_ty))
  else if op = op_automaton_run then AutomatonRun
  else if op = op_view_text then ViewText
  else if op = op_view_image then ViewImage
  else if op = op_view_button then ViewButton
  else if op = op_view_input then ViewInput
  else if op = op_view_column then ViewColumn
  else if op = op_view_row then ViewRow
  else if op = op_view_list then ViewList
  else if op = op_view_when then ViewWhen
  else if op = op_view_node then ViewNode
  else if op = op_view_attr then ViewAttr
  else if op = op_view_on then ViewOn
  else if op = op_proc_done then ProcDone
  else if op = op_proc_request then ProcRequest (get_req cur)
  else if op = op_proc_bind then (
    let awaited_ty = get_typ cur in
    let body = get_block cur in
    ProcBind (awaited_ty, body))
  else Kernel_error.fail ("bytecode: bad opcode " ^ string_of_int op)

and get_branch cur : casebranch =
  match get_u8 cur with
  | t when t = br_bool ->
      let v = get_u8 cur <> 0 in
      let body = get_block cur in
      BoolBranch (v, body)
  | t when t = br_variant ->
      let con = get_str cur in
      let body = get_block cur in
      VariantBranch (con, body)
  | tag -> Kernel_error.fail ("bytecode: bad branch tag " ^ string_of_int tag)

and get_block cur : block =
  let n = get_u32 cur in
  (* Read instructions in explicit increasing-index order, then freeze to an
     array. We do NOT use [Array.init] with a side-effecting function: even
     though OCaml 5 fixes its evaluation order, an order-dependent init function
     is fragile, so we build the list cursor-step by cursor-step instead. *)
  let rec loop i acc = if i >= n then List.rev acc else loop (i + 1) (get_instr cur :: acc) in
  Array.of_list (loop 0 [])

let get_def cur : bc_def =
  let bc_name = get_str cur in
  let bc_def_id = get_str cur in
  let bc_typ = get_typ cur in
  let bc_code = get_block cur in
  { bc_name; bc_def_id; bc_typ; bc_code }

let decode_module (s : string) : module_ =
  let cur = { src = s; pos = 0 } in
  need cur (String.length magic);
  let got_magic = String.sub s 0 (String.length magic) in
  if not (String.equal got_magic magic) then
    Kernel_error.fail "bytecode: bad magic (not a Protoss bytecode module)";
  cur.pos <- String.length magic;
  let bc_version = get_str cur in
  let bc_exports = get_list cur get_str in
  let bc_defs = get_list cur get_def in
  if cur.pos <> String.length s then
    Kernel_error.fail "bytecode: trailing bytes after module";
  { bc_version; bc_exports; bc_defs }

(* ------------------------------------------------------------------------- *)
(* Content-addressed hash of a bytecode module                                *)
(* ------------------------------------------------------------------------- *)

(* The module hash is the project's standard sha256 content ref ("p2:"-prefixed)
   over the canonical encoded bytes. Because [encode_module] is a deterministic,
   platform-independent fold, identical modules hash identically everywhere.
   [Hashcons.hash] is the exact function used for every other content ref in the
   project (it dispatches to the hardware/pure SHA-256 and adds the "p2:"
   prefix). *)
let module_bytes = encode_module

let hash_module (m : module_) : string = Hashcons.hash (encode_module m)

(* Round-trip helper kept for callers/tests: encode then decode must be the
   identity on the [module_] structure (and re-encoding is byte-identical, which
   is what determinism guarantees). Not used internally; provided so the
   integrator can wire a round-trip invariant cheaply. *)
let decode_then_encode (s : string) : string = encode_module (decode_module s)
