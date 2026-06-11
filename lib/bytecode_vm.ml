(* Protoss VM bytecode executor (goal G5).

   ============================================================================
   WHAT THIS IS
   ============================================================================

   [bytecode.ml] (goal G4) defines the bytecode *format* and the compiler
   [Kernel.cterm -> Bytecode.block]; it contains no executor. This module is the
   executor: it really runs a [Bytecode.module_] on an operand-stack + De Bruijn
   environment machine and produces a value that is semantically identical to the
   reference tree-walking interpreter [Runtime.eval_cterm]/[Runtime.eval_app].

   Parity is the whole point, and parity here is judged through
   [Runtime.value_to_canonical] (see [vm_canonical] below). That canonical
   projection serializes, for a closure, BOTH its captured environment and its
   *body as a [Kernel.cterm]* (via [Kernel.cterm_to_string]); for a suspended
   [Process] it serializes the request + the (cterm-bearing) continuation. The
   bytecode compiler, being a structural fold, throws none of that away — it just
   re-shapes it into instructions. So to land on the byte-identical canonical
   string we must reconstruct the original values, cterm bodies included.

   We therefore:

   1. Use [Runtime.value] itself as the VM's value type. This is the single most
      important decision for parity: every value the VM ever materialises is a
      genuine [Runtime.value], so [Runtime.value_to_canonical] applies verbatim
      and there is no second, parallel notion of "canonical form" to keep in
      sync. Closures are [Runtime.VClosure (ty, body_cterm, env, cap_scope)] and
      process suspensions are [Runtime.VProcessRequest {req; cont; cap_scope}],
      exactly as the interpreter builds them.

   2. Execute the *bytecode*, not the cterm. Dispatch is a real loop over the
      [Bytecode.instruction array] with an explicit operand stack and an explicit
      environment list (De Bruijn frame). [PushVar i] reaches [i] slots into the
      environment (and forces, like [Runtime.nth_env]); binder opcodes
      ([MakeClosure]/[LetBind]/[CaseList]/[Case]/[FoldVariant]/[ProcBind]) extend
      it for the region of their nested block, matching the kernel's De Bruijn
      convention that [bytecode.ml] documents.

   3. Bridge the one place where a value must carry a cterm. [MakeClosure] only
      ships the compiled body *block*; the resulting [VClosure] needs the body
      *cterm* for its canonical form. We recover it with [decompile], the exact
      inverse of [Bytecode.compile_term] (the compiler appends one terminal
      opcode after its children's code in a fixed order, so a symbolic-stack pass
      rebuilds the tree deterministically: [decompile (compile_block c)] is
      structurally [c], hence [cterm_to_string] agrees). When the VM later
      *applies* such a closure it re-compiles that body cterm with the public
      [Bytecode.compile] and runs the resulting block — so application is still
      bytecode execution, never a hand-off to the interpreter.

   ============================================================================
   SEMANTIC FIDELITY (every rule below is copied from [Runtime])
   ============================================================================

   - Globals: [PushBuiltin "succ"] -> [VBuiltinSucc]; [PushBuiltin "prim.*"] ->
     [VClosure (TUnit, CGlobal name, [], [])] (the exact partial-application seed
     the interpreter uses, see [Runtime.eval_cterm]'s [CGlobal] case). Other
     [PushGlobal]/[PushBuiltin] and [PushInst] resolve against the program.
   - Application reproduces [Runtime.apply_value] case-for-case: [VBuiltinSucc],
     every [prim.Nat.*]/[prim.String.*]/[prim.List.*]/[prim.Assoc.*] arity-step,
     and the generic closure rule
       [eval_with_cap_scope cap_scope (fun () -> eval (av :: closure_env) body)].
   - [cap_scope] is a mutable scope threaded exactly as in [Runtime]: it starts
     at the entry def's declared capabilities, [CLambda] captures it, generic
     closure application restores+merges it, [CInst] runs the instantiated body
     under the referenced def's capabilities, and [CRequest]/[CBind] stamp it
     onto the suspension / continuation.
   - [foldNat]/[foldList] evaluate the step term to a *function value* and apply
     it (no [Recur]); [foldVariant] pushes a recur frame and [Recur] feeds the
     enclosing fold — precisely [Runtime]'s [recur_stack] discipline.
   - [let] is the one spot where the format is eager where the interpreter is
     lazy: [Bytecode.compile_term] emits [code(bound) @ [LetBind body]], so the
     bound term is evaluated to a value before [LetBind] binds it, whereas
     [Runtime] binds a [VThunk]. On the total executable fragment this never
     changes the result, and [value_to_canonical] forces thunks anyway, so the
     canonical strings coincide. (Documented; not a divergence in observable
     output.)

   ============================================================================
   PUBLIC API
   ============================================================================

   - [vm_canonical : Kernel.checked -> string -> string]
       Compile the checked program to bytecode, execute the named def on the VM,
       and return [Runtime.value_to_canonical] of the result. The parity test is:
         Bytecode_vm.vm_canonical checked name
           = Runtime.value_to_canonical (fst (Runtime.normalize_def checked name))
   - [exec_checked : Kernel.checked -> string -> Runtime.value]
       Same execution, returning the raw value (so tests can compare values or
       canonical forms themselves).
   - [exec_module : Bytecode.module_ -> string -> Runtime.value]
       Execute a bare bytecode module (no [checked] context). Globals resolve
       among the module's own defs; capability scope starts empty and [CInst]
       type arguments are applied by decompiling the referenced def. Provided for
       completeness; [vm_canonical]/[exec_checked] are the parity entry points
       because only the [checked] carries declared capabilities. *)

(* All "should never happen on the executable fragment" guards go through
   [Kernel_error.fail] with a "bytecode-vm: ..." prefix, never a silent wrong
   value — matching the project's no-faux-vert rule and [Bytecode]'s own
   convention of failing loudly on forms it cannot handle. *)
let fail what = Kernel_error.fail ("bytecode-vm: " ^ what)

(* ------------------------------------------------------------------------- *)
(* decompile : Bytecode.block -> Kernel.cterm                                 *)
(* ------------------------------------------------------------------------- *)

(* Exact inverse of [Bytecode.compile_term]. The compiler is a deterministic
   postfix encoding of the cterm tree: every node emits its children's code
   left-to-right and then one terminal opcode that consumes them. We reconstruct
   by running a symbolic stack of cterms over the instruction array: child
   opcodes push leaves/blocks, combinator opcodes pop their operands (in the
   compiler's push order) and push the rebuilt parent. A well-formed block left
   by the compiler reduces to exactly one cterm.

   Nested blocks (lambda/let/bind/fold/caseList bodies, case/fold branches) were
   stored whole by the compiler, so we recurse with [decompile_block]. *)

let rec decompile_block (blk : Bytecode.block) : Kernel.cterm =
  let stack : Kernel.cterm list ref = ref [] in
  let push t = stack := t :: !stack in
  let pop1 () =
    match !stack with
    | x :: rest -> stack := rest; x
    | [] -> fail "decompile: operand stack underflow"
  in
  (* Pop two operands. The compiler pushed [a] then [b] (so [b] is on top); we
     return them in source order [(a, b)]. *)
  let pop2 () =
    let b = pop1 () in
    let a = pop1 () in
    (a, b)
  in
  let pop3 () =
    let c = pop1 () in
    let b = pop1 () in
    let a = pop1 () in
    (a, b, c)
  in
  Array.iter
    (fun (instr : Bytecode.instruction) ->
      match instr with
      | Bytecode.PushUnit -> push Kernel.CUnit
      | Bytecode.PushBool b -> push (Kernel.CBool b)
      | Bytecode.PushNat n -> push (Kernel.CNat n)
      | Bytecode.PushString s -> push (Kernel.CString s)
      | Bytecode.PushVar i -> push (Kernel.CVar i)
      (* [CGlobal n] was split at compile time into builtin/global; both decode
         back to the same [CGlobal n] the compiler started from. *)
      | Bytecode.PushGlobal n -> push (Kernel.CGlobal n)
      | Bytecode.PushBuiltin n -> push (Kernel.CGlobal n)
      | Bytecode.PushInst (n, type_args) -> push (Kernel.CInst (n, type_args))
      | Bytecode.MakeClosure (param_ty, body) ->
          push (Kernel.CLambda (param_ty, decompile_block body))
      | Bytecode.Apply ->
          let f, x = pop2 () in
          push (Kernel.CApp (f, x))
      | Bytecode.Force -> push (Kernel.CStrict (pop1 ()))
      | Bytecode.LetBind body ->
          let bound = pop1 () in
          push (Kernel.CLet (bound, decompile_block body))
      | Bytecode.MakeRecord labels ->
          (* The compiler pushed the field values in label order; pop that many
             and re-pair with the labels (preserving order). *)
          let n = List.length labels in
          let rec take k acc = if k = 0 then acc else take (k - 1) (pop1 () :: acc) in
          let values = take n [] in
          push (Kernel.CRecord (List.combine labels values))
      | Bytecode.Project field -> push (Kernel.CField (pop1 (), field))
      | Bytecode.MakeVariant (ty, con) -> push (Kernel.CVariant (ty, con, pop1 ()))
      | Bytecode.Case branches ->
          let scrut = pop1 () in
          push (Kernel.CCase (scrut, List.map decompile_branch branches))
      | Bytecode.FoldNat (zero, step) ->
          let n = pop1 () in
          push (Kernel.CFoldNat (n, decompile_block zero, decompile_block step))
      | Bytecode.FoldVariant (target, result, branches) ->
          let scrut = pop1 () in
          push (Kernel.CFoldVariant (target, result, scrut, List.map decompile_branch branches))
      | Bytecode.Recur -> push (Kernel.CRecur (pop1 ()))
      | Bytecode.Nil item_ty -> push (Kernel.CNil item_ty)
      | Bytecode.Cons item_ty ->
          let head, tail = pop2 () in
          push (Kernel.CCons (item_ty, head, tail))
      | Bytecode.FoldList (zero, step) ->
          let xs = pop1 () in
          push (Kernel.CFoldList (xs, decompile_block zero, decompile_block step))
      | Bytecode.CaseList (nil_body, cons_body) ->
          let xs = pop1 () in
          push (Kernel.CCaseList (xs, decompile_block nil_body, decompile_block cons_body))
      | Bytecode.Coiter (state_ty, item_ty) ->
          let seed, step = pop2 () in
          push (Kernel.CCoiter (state_ty, item_ty, seed, step))
      | Bytecode.StreamHead -> push (Kernel.CStreamHead (pop1 ()))
      | Bytecode.StreamTail -> push (Kernel.CStreamTail (pop1 ()))
      | Bytecode.StreamTake ->
          let count, s = pop2 () in
          push (Kernel.CStreamTake (count, s))
      | Bytecode.MakeAutomaton (state_ty, output_ty) ->
          let initial, transition = pop2 () in
          push (Kernel.CAutomaton (state_ty, output_ty, initial, transition))
      | Bytecode.AutomatonRun ->
          let count, a = pop2 () in
          push (Kernel.CAutomatonRun (count, a))
      | Bytecode.ViewText -> push (Kernel.CText (pop1 ()))
      | Bytecode.ViewImage ->
          let src, alt = pop2 () in
          push (Kernel.CImage (src, alt))
      | Bytecode.ViewButton ->
          let label, msg = pop2 () in
          push (Kernel.CButton (label, msg))
      | Bytecode.ViewInput ->
          let value, handler = pop2 () in
          push (Kernel.CInput (value, handler))
      | Bytecode.ViewColumn -> push (Kernel.CColumn (pop1 ()))
      | Bytecode.ViewRow -> push (Kernel.CRow (pop1 ()))
      | Bytecode.ViewList ->
          let items, render = pop2 () in
          push (Kernel.CListView (items, render))
      | Bytecode.ViewWhen ->
          let cond, view = pop2 () in
          push (Kernel.CWhenView (cond, view))
      | Bytecode.ViewNode ->
          let tag, attrs, children = pop3 () in
          push (Kernel.CNode (tag, attrs, children))
      | Bytecode.ViewAttr ->
          let name, value = pop2 () in
          push (Kernel.CAttr (name, value))
      | Bytecode.ViewOn ->
          let event, msg = pop2 () in
          push (Kernel.COn (event, msg))
      | Bytecode.ProcDone -> push (Kernel.CDone (pop1 ()))
      | Bytecode.ProcRequest req -> push (Kernel.CRequest req)
      | Bytecode.ProcBind (awaited_ty, body) ->
          let p = pop1 () in
          push (Kernel.CBind (p, awaited_ty, decompile_block body)))
    blk;
  match !stack with
  | [ t ] -> t
  | [] -> fail "decompile: empty block"
  | _ -> fail "decompile: block did not reduce to a single term"

and decompile_branch (b : Bytecode.casebranch) : Kernel.cbranch =
  match b with
  | Bytecode.BoolBranch (v, body) -> Kernel.CBBool (v, decompile_block body)
  | Bytecode.VariantBranch (con, body) -> Kernel.CBVariant (con, decompile_block body)

(* ------------------------------------------------------------------------- *)
(* VM state                                                                   *)
(* ------------------------------------------------------------------------- *)

(* A resolver maps a global name (def name or def-id) to the information the VM
   needs to evaluate a reference: its compiled body block, its declared
   capabilities, and its body as a cterm (needed for [CInst] type substitution).
   [vm_canonical]/[exec_checked] build this from the [Kernel.checked] (which has
   capabilities and cterms); [exec_module] builds a degraded one from the module
   alone (capabilities default to [], cterm via [decompile]). *)
type global_entry = {
  ge_block : Bytecode.block;
  ge_caps : string list;
  ge_body : Kernel.cterm;
}

type machine = {
  resolve : string -> global_entry option;
  (* Memoised value of each fully-evaluated global, keyed by the name used to
     reach it — mirrors [Runtime]'s [def_cache] (evaluation is pure, so caching
     cannot change a result; it only avoids recomputation, e.g. of recursive
     references). *)
  globals : (string, Runtime.value) Hashtbl.t;
  mutable cap_scope : string list;
  (* Stack of enclosing [foldVariant] recursion functions; [Recur] applies the
     innermost. Mirrors [Runtime.recur_stack] (we only need [recur_apply]). *)
  mutable recur_stack : (Runtime.value -> Runtime.value) list;
}

(* [eval_with_cap_scope], byte-for-byte as in [Runtime]: a no-op when the extra
   caps are already covered (empty, or a subset of the current scope), otherwise
   merge for the dynamic extent and restore afterwards. *)
let with_cap_scope m caps f =
  if caps == [] || Runtime.subset_sorted caps m.cap_scope then f ()
  else begin
    let previous = m.cap_scope in
    m.cap_scope <- Runtime.merge_caps m.cap_scope caps;
    Fun.protect ~finally:(fun () -> m.cap_scope <- previous) f
  end

(* ------------------------------------------------------------------------- *)
(* Execution                                                                  *)
(* ------------------------------------------------------------------------- *)

(* [exec_block m env blk] runs [blk] in environment [env] (innermost binder
   first, De Bruijn slot 0) and returns the single net value it leaves on the
   operand stack.

   We keep an explicit operand stack [ostack] as a [Runtime.value list] (top of
   list = top of stack). Children of a node are executed earlier in [blk] and so
   land on [ostack] before the combinator opcode that consumes them, exactly as
   the compiler arranged. Binder-introducing opcodes execute their nested block
   with an extended [env] and push that block's result. *)
let rec exec_block (m : machine) (env : Runtime.value list) (blk : Bytecode.block) :
    Runtime.value =
  let ostack = ref [] in
  let push v = ostack := v :: !ostack in
  let pop () =
    match !ostack with
    | x :: rest -> ostack := rest; x
    | [] -> fail "operand stack underflow"
  in
  let pop2 () =
    let b = pop () in
    let a = pop () in
    (a, b)
  in
  let pop3 () =
    let c = pop () in
    let b = pop () in
    let a = pop () in
    (a, b, c)
  in
  Array.iter (fun instr -> exec_instr m env push pop pop2 pop3 instr) blk;
  match !ostack with
  | [ v ] -> v
  | [] -> fail "block produced no value"
  | _ -> fail "block left more than one value on the operand stack"

and exec_instr m env push pop pop2 pop3 (instr : Bytecode.instruction) : unit =
  match instr with
  (* Literals. *)
  | Bytecode.PushUnit -> push Runtime.VUnit
  | Bytecode.PushBool b -> push (Runtime.VBool b)
  | Bytecode.PushNat n -> push (Runtime.VNat n)
  | Bytecode.PushString s -> push (Runtime.VString s)
  (* De Bruijn variable: read slot [i], forcing thunks (= [Runtime.nth_env]). *)
  | Bytecode.PushVar i -> push (Runtime.force_value (nth_env env i))
  (* Globals. The builtin split was already decided at compile time. *)
  | Bytecode.PushBuiltin n -> push (builtin_value n)
  | Bytecode.PushGlobal n -> push (eval_global m n)
  | Bytecode.PushInst (n, type_args) -> push (eval_inst m n type_args)
  (* Closure: capture the current env and cap-scope; recover the body cterm so
     the value's canonical form matches the interpreter's. *)
  | Bytecode.MakeClosure (param_ty, body) ->
      push (Runtime.VClosure (param_ty, decompile_block body, env, m.cap_scope))
  | Bytecode.Apply ->
      let fv, av = pop2 () in
      push (apply_value m fv av)
  | Bytecode.Force -> push (Runtime.force_value (pop ()))
  | Bytecode.LetBind body ->
      (* The bound value is already on the stack (the compiler emitted the bound
         term's code before [LetBind]); bind it as the new innermost slot and run
         the body block. *)
      let bound = pop () in
      push (exec_block m (bound :: env) body)
  (* Records: pop the field values (top = last label) and pair with labels in
     order, then sort like [Runtime] does. *)
  | Bytecode.MakeRecord labels ->
      let n = List.length labels in
      let rec take k acc = if k = 0 then acc else take (k - 1) (pop () :: acc) in
      let values = take n [] in
      push (Runtime.VRecord (Ast.sort_fields (List.combine labels values)))
  | Bytecode.Project field -> push (project_field field (pop ()))
  | Bytecode.MakeVariant (ty, con) -> push (Runtime.VVariant (ty, con, pop ()))
  | Bytecode.Case branches -> push (exec_case m env (pop ()) branches)
  | Bytecode.FoldNat (zero, step) -> push (exec_fold_nat m env (pop ()) zero step)
  | Bytecode.FoldVariant (_, _, branches) ->
      push (exec_fold_variant m env (pop ()) branches)
  | Bytecode.Recur -> (
      let v = pop () in
      match m.recur_stack with
      | frame :: _ -> push (frame v)
      | [] -> fail "recur outside foldVariant")
  | Bytecode.Nil item_ty -> push (Runtime.VList (item_ty, []))
  | Bytecode.Cons item_ty -> (
      let head, tail = pop2 () in
      match tail with
      | Runtime.VList (_, xs) -> push (Runtime.VList (item_ty, head :: xs))
      | v -> fail ("Cons tail on non-List: " ^ Runtime.value_to_string v))
  | Bytecode.FoldList (zero, step) -> push (exec_fold_list m env (pop ()) zero step)
  | Bytecode.CaseList (nil_body, cons_body) ->
      push (exec_case_list m env (pop ()) nil_body cons_body)
  | Bytecode.Coiter (state_ty, item_ty) ->
      let seed, step = pop2 () in
      push (Runtime.VStream (state_ty, item_ty, seed, step))
  | Bytecode.StreamHead -> (
      match Runtime.force_value (pop ()) with
      | Runtime.VStream (_, _, state, step) ->
          let head, _ = stream_step m state step in
          push head
      | v -> fail ("streamHead on non-Stream: " ^ Runtime.value_to_string v))
  | Bytecode.StreamTail -> (
      match Runtime.force_value (pop ()) with
      | Runtime.VStream (state_ty, item_ty, state, step) ->
          let _, next_state = stream_step m state step in
          push (Runtime.VStream (state_ty, item_ty, next_state, step))
      | v -> fail ("streamTail on non-Stream: " ^ Runtime.value_to_string v))
  | Bytecode.StreamTake -> (
      let count, s = pop2 () in
      match (Runtime.force_value count, Runtime.force_value s) with
      | Runtime.VNat n, Runtime.VStream (_, item_ty, state, step) ->
          let rec loop remaining state acc =
            if remaining <= 0 then Runtime.VList (item_ty, List.rev acc)
            else
              let head, next_state = stream_step m state step in
              loop (remaining - 1) next_state (head :: acc)
          in
          push (loop n state [])
      | Runtime.VNat _, v -> fail ("streamTake on non-Stream: " ^ Runtime.value_to_string v)
      | v, _ -> fail ("streamTake count on non-Nat: " ^ Runtime.value_to_string v))
  | Bytecode.MakeAutomaton (state_ty, output_ty) ->
      let initial, transition = pop2 () in
      push (Runtime.VAutomaton (state_ty, output_ty, initial, transition))
  | Bytecode.AutomatonRun -> (
      let count, a = pop2 () in
      match (Runtime.force_value count, Runtime.force_value a) with
      | Runtime.VNat n, Runtime.VAutomaton (_, output_ty, state, transition) ->
          let rec loop remaining state acc =
            if remaining <= 0 then Runtime.VList (output_ty, List.rev acc)
            else
              let output, next_state = automaton_step m state transition in
              loop (remaining - 1) next_state (output :: acc)
          in
          push (loop n state [])
      | Runtime.VNat _, v -> fail ("automatonRun on non-Automaton: " ^ Runtime.value_to_string v)
      | v, _ -> fail ("automatonRun count on non-Nat: " ^ Runtime.value_to_string v))
  (* View nodes. *)
  | Bytecode.ViewText -> (
      match pop () with
      | Runtime.VString s -> push (Runtime.VView (Runtime.VText s))
      | v -> fail ("text on non-String: " ^ Runtime.value_to_string v))
  | Bytecode.ViewImage -> (
      let src, alt = pop2 () in
      match (src, alt) with
      | Runtime.VString src, Runtime.VString alt -> push (Runtime.VView (Runtime.VImage (src, alt)))
      | Runtime.VString _, v -> fail ("image alt on non-String: " ^ Runtime.value_to_string v)
      | v, _ -> fail ("image src on non-String: " ^ Runtime.value_to_string v))
  | Bytecode.ViewButton -> (
      let label, msg = pop2 () in
      match label with
      | Runtime.VString s -> push (Runtime.VView (Runtime.VButton (s, msg)))
      | v -> fail ("button label on non-String: " ^ Runtime.value_to_string v))
  | Bytecode.ViewInput -> (
      let value, handler = pop2 () in
      match value with
      | Runtime.VString s -> push (Runtime.VView (Runtime.VInput (s, handler)))
      | v -> fail ("input value on non-String: " ^ Runtime.value_to_string v))
  | Bytecode.ViewColumn -> (
      match pop () with
      | Runtime.VList (_, items) -> push (Runtime.VView (Runtime.VColumn (List.map expect_view items)))
      | v -> fail ("column on non-List: " ^ Runtime.value_to_string v))
  | Bytecode.ViewRow -> (
      match pop () with
      | Runtime.VList (_, items) -> push (Runtime.VView (Runtime.VRow (List.map expect_view items)))
      | v -> fail ("row on non-List: " ^ Runtime.value_to_string v))
  | Bytecode.ViewList -> (
      let items, render = pop2 () in
      match items with
      | Runtime.VList (_, items) ->
          push
            (Runtime.VView
               (Runtime.VColumn (List.map (fun item -> expect_view (apply_value m render item)) items)))
      | v -> fail ("list view on non-List: " ^ Runtime.value_to_string v))
  | Bytecode.ViewWhen -> (
      let cond, view = pop2 () in
      match cond with
      | Runtime.VBool true -> push view
      | Runtime.VBool false -> push (Runtime.VView (Runtime.VColumn []))
      | v -> fail ("when condition on non-Bool: " ^ Runtime.value_to_string v))
  | Bytecode.ViewNode -> (
      let tag, attrs, children = pop3 () in
      match tag with
      | Runtime.VString tag -> (
          match (attrs, children) with
          | Runtime.VList (_, attr_items), Runtime.VList (_, child_items) ->
              push
                (Runtime.VView
                   (Runtime.VNode
                      (tag, List.map expect_attr attr_items, List.map expect_view child_items)))
          | Runtime.VList _, v -> fail ("node children on non-List: " ^ Runtime.value_to_string v)
          | v, _ -> fail ("node attributes on non-List: " ^ Runtime.value_to_string v))
      | v -> fail ("node tag on non-String: " ^ Runtime.value_to_string v))
  | Bytecode.ViewAttr -> (
      let name, value = pop2 () in
      match (name, value) with
      | Runtime.VString name, Runtime.VString value ->
          push (Runtime.VAttribute (Runtime.VAttr (name, value)))
      | Runtime.VString _, v -> fail ("attr value on non-String: " ^ Runtime.value_to_string v)
      | v, _ -> fail ("attr name on non-String: " ^ Runtime.value_to_string v))
  | Bytecode.ViewOn -> (
      let event, msg = pop2 () in
      match event with
      | Runtime.VString event -> push (Runtime.VAttribute (Runtime.VOn (event, msg)))
      | v -> fail ("on event on non-String: " ^ Runtime.value_to_string v))
  (* Process nodes. We never run the effect: a request becomes a suspension,
     exactly like [Runtime.VProcessRequest]/[VProcessDone]. *)
  | Bytecode.ProcDone -> push (Runtime.VProcessDone (pop ()))
  | Bytecode.ProcRequest req ->
      push
        (Runtime.VProcessRequest
           { Runtime.req; Runtime.cont = Runtime.KDone; Runtime.cap_scope = m.cap_scope })
  | Bytecode.ProcBind (_, body) -> push (exec_bind m env (pop ()) body)

(* --- environment access (= Runtime.nth_env, without the trace hook) --- *)
and nth_env env i =
  match (env, i) with
  | v :: _, 0 -> v
  | _ :: rest, n when n > 0 -> nth_env rest (n - 1)
  | _ -> fail ("unbound canonical variable #" ^ string_of_int i)

(* --- globals / builtins --- *)

(* [PushBuiltin]: reproduce [Runtime.eval_cterm]'s [CGlobal] builtin handling.
   "succ" is the dedicated [VBuiltinSucc]; every prim becomes the zero-arg
   closure seed the interpreter uses so partial application matches. *)
and builtin_value n =
  match n with
  | "succ" -> Runtime.VBuiltinSucc
  | "prim.Nat.add" | "prim.Nat.mul" | "prim.Nat.pred" | "prim.Nat.sub" | "prim.Nat.eq"
  | "prim.Nat.lte" | "prim.Nat.lt" | "prim.Nat.gte" | "prim.Nat.gt" | "prim.Nat.toString"
  | "prim.String.concat" | "prim.String.eq" | "prim.String.length" | "prim.String.slice"
  | "prim.String.charAt" ->
      Runtime.VClosure (Ast.TUnit, Kernel.CGlobal n, [], [])
  | _ ->
      (* Any other builtin name should have been a real global; the compiler only
         emits [PushBuiltin] for [Kernel.is_builtin] names, so this is unreachable
         on compiler output. Fail loudly rather than guess. *)
      fail ("unknown builtin: " ^ n)

(* [PushGlobal n] (a non-builtin global). Mirrors [Runtime.eval_def]: look the
   def up, evaluate its body once in the empty env under the def's capabilities,
   memoise. *)
and eval_global m n =
  match Hashtbl.find_opt m.globals n with
  | Some v -> v
  | None -> (
      match m.resolve n with
      | None -> fail ("unknown definition: " ^ n)
      | Some ge ->
          let v = with_cap_scope m ge.ge_caps (fun () -> exec_block m [] ge.ge_block) in
          Hashtbl.replace m.globals n v;
          v)

(* [PushInst (n, type_args)]: mirror [Runtime.eval_cterm]'s [CInst] on the
   default (no stdlib-fast-path) path used by [Runtime.normalize_def]: substitute
   the type arguments into the referenced def's body and evaluate it in the empty
   env under the def's capabilities. We execute the bytecode of the substituted
   body (compile the substituted cterm). Not memoised, since the value depends on
   [type_args] (matching the interpreter, which re-evaluates per instantiation). *)
and eval_inst m n type_args =
  match m.resolve n with
  | None -> fail ("unknown polymorphic definition: " ^ n)
  | Some ge ->
      let instantiated = Kernel.subst_type_in_cterm type_args ge.ge_body in
      with_cap_scope m ge.ge_caps (fun () -> exec_block m [] (Bytecode.compile instantiated))

(* --- application (= Runtime.apply_value, case for case) --- *)
and apply_value m fv av : Runtime.value =
  match (fv, av) with
  | Runtime.VBuiltinSucc, _ -> (
      match av with
      | Runtime.VNat n -> Runtime.VNat (n + 1)
      | v -> fail ("builtin on " ^ Runtime.value_to_string v))
  (* prim.Nat.* : first application stores the first Nat as a one-element closure
     env, second application computes. Exactly the interpreter's encoding. *)
  | Runtime.VClosure (_, Kernel.CGlobal "prim.Nat.add", cenv, _), _ -> (
      match (cenv, av) with
      | [], Runtime.VNat _ -> Runtime.VClosure (Ast.TNat, Kernel.CGlobal "prim.Nat.add", [ av ], [])
      | [ Runtime.VNat a ], Runtime.VNat b -> Runtime.VNat (a + b)
      | _ -> fail "prim.Nat.add expects Nat Nat")
  | Runtime.VClosure (_, Kernel.CGlobal "prim.Nat.mul", cenv, _), _ -> (
      match (cenv, av) with
      | [], Runtime.VNat _ -> Runtime.VClosure (Ast.TNat, Kernel.CGlobal "prim.Nat.mul", [ av ], [])
      | [ Runtime.VNat a ], Runtime.VNat b -> Runtime.VNat (a * b)
      | _ -> fail "prim.Nat.mul expects Nat Nat")
  | Runtime.VClosure (_, Kernel.CGlobal "prim.Nat.pred", cenv, _), _ -> (
      match (cenv, av) with
      | [], Runtime.VNat n -> Runtime.VNat (max 0 (n - 1))
      | _ -> fail "prim.Nat.pred expects Nat")
  | Runtime.VClosure (_, Kernel.CGlobal "prim.Nat.sub", cenv, _), _ -> (
      match (cenv, av) with
      | [], Runtime.VNat _ -> Runtime.VClosure (Ast.TNat, Kernel.CGlobal "prim.Nat.sub", [ av ], [])
      | [ Runtime.VNat a ], Runtime.VNat b -> Runtime.VNat (max 0 (a - b))
      | _ -> fail "prim.Nat.sub expects Nat Nat")
  | Runtime.VClosure (_, Kernel.CGlobal "prim.Nat.eq", cenv, _), _ -> (
      match (cenv, av) with
      | [], Runtime.VNat _ -> Runtime.VClosure (Ast.TNat, Kernel.CGlobal "prim.Nat.eq", [ av ], [])
      | [ Runtime.VNat a ], Runtime.VNat b -> Runtime.VBool (a = b)
      | _ -> fail "prim.Nat.eq expects Nat Nat")
  | Runtime.VClosure (_, Kernel.CGlobal "prim.Nat.lte", cenv, _), _ -> (
      match (cenv, av) with
      | [], Runtime.VNat _ -> Runtime.VClosure (Ast.TBool, Kernel.CGlobal "prim.Nat.lte", [ av ], [])
      | [ Runtime.VNat a ], Runtime.VNat b -> Runtime.VBool (a <= b)
      | _ -> fail "prim.Nat.lte expects Nat Nat")
  | Runtime.VClosure (_, Kernel.CGlobal "prim.Nat.lt", cenv, _), _ -> (
      match (cenv, av) with
      | [], Runtime.VNat _ -> Runtime.VClosure (Ast.TBool, Kernel.CGlobal "prim.Nat.lt", [ av ], [])
      | [ Runtime.VNat a ], Runtime.VNat b -> Runtime.VBool (a < b)
      | _ -> fail "prim.Nat.lt expects Nat Nat")
  | Runtime.VClosure (_, Kernel.CGlobal "prim.Nat.gte", cenv, _), _ -> (
      match (cenv, av) with
      | [], Runtime.VNat _ -> Runtime.VClosure (Ast.TBool, Kernel.CGlobal "prim.Nat.gte", [ av ], [])
      | [ Runtime.VNat a ], Runtime.VNat b -> Runtime.VBool (a >= b)
      | _ -> fail "prim.Nat.gte expects Nat Nat")
  | Runtime.VClosure (_, Kernel.CGlobal "prim.Nat.gt", cenv, _), _ -> (
      match (cenv, av) with
      | [], Runtime.VNat _ -> Runtime.VClosure (Ast.TBool, Kernel.CGlobal "prim.Nat.gt", [ av ], [])
      | [ Runtime.VNat a ], Runtime.VNat b -> Runtime.VBool (a > b)
      | _ -> fail "prim.Nat.gt expects Nat Nat")
  | Runtime.VClosure (_, Kernel.CGlobal "prim.Nat.toString", cenv, _), _ -> (
      match (cenv, av) with
      | [], Runtime.VNat n -> Runtime.VString (string_of_int n)
      | _ -> fail "prim.Nat.toString expects Nat")
  | Runtime.VClosure (_, Kernel.CGlobal "prim.String.concat", cenv, _), _ -> (
      match (cenv, av) with
      | [], Runtime.VString _ ->
          Runtime.VClosure (Ast.TString, Kernel.CGlobal "prim.String.concat", [ av ], [])
      | [ Runtime.VString a ], Runtime.VString b -> Runtime.VString (a ^ b)
      | _ -> fail "prim.String.concat expects String String")
  | Runtime.VClosure (_, Kernel.CGlobal "prim.String.eq", cenv, _), _ -> (
      match (cenv, av) with
      | [], Runtime.VString _ ->
          Runtime.VClosure (Ast.TString, Kernel.CGlobal "prim.String.eq", [ av ], [])
      | [ Runtime.VString a ], Runtime.VString b -> Runtime.VBool (String.equal a b)
      | _ -> fail "prim.String.eq expects String String")
  | Runtime.VClosure (_, Kernel.CGlobal "prim.String.length", cenv, _), _ -> (
      match (cenv, av) with
      | [], Runtime.VString s -> Runtime.VNat (String_prim.length s)
      | _ -> fail "prim.String.length expects String")
  | Runtime.VClosure (_, Kernel.CGlobal "prim.String.slice", cenv, _), _ -> (
      match (cenv, av) with
      | [], Runtime.VString _ ->
          Runtime.VClosure (Ast.TNat, Kernel.CGlobal "prim.String.slice", [ av ], [])
      | [ Runtime.VString _ ], Runtime.VNat _ ->
          Runtime.VClosure (Ast.TNat, Kernel.CGlobal "prim.String.slice", cenv @ [ av ], [])
      | [ Runtime.VString s; Runtime.VNat start ], Runtime.VNat count ->
          Runtime.VString (String_prim.slice s start count)
      | _ -> fail "prim.String.slice expects String Nat Nat")
  | Runtime.VClosure (_, Kernel.CGlobal "prim.String.charAt", cenv, _), _ -> (
      match (cenv, av) with
      | [], Runtime.VString _ ->
          Runtime.VClosure (Ast.TNat, Kernel.CGlobal "prim.String.charAt", [ av ], [])
      | [ Runtime.VString s ], Runtime.VNat index ->
          if index < 0 || index >= String_prim.length s then
            Runtime.VVariant (Runtime.maybe_type Ast.TString, "None", Runtime.VUnit)
          else
            Runtime.VVariant
              (Runtime.maybe_type Ast.TString, "Some", Runtime.VString (String_prim.slice s index 1))
      | _ -> fail "prim.String.charAt expects String Nat")
  (* prim.List.* *)
  | Runtime.VClosure (_, Kernel.CGlobal "prim.List.length", [], _), Runtime.VList (_, xs) ->
      Runtime.VNat (List.length xs)
  | Runtime.VClosure (item_typ, Kernel.CGlobal "prim.List.append", [], _), Runtime.VList (_, xs) ->
      Runtime.VClosure
        (item_typ, Kernel.CGlobal "prim.List.append", [ Runtime.VList (item_typ, xs) ], [])
  | ( Runtime.VClosure (item_typ, Kernel.CGlobal "prim.List.append", [ Runtime.VList (_, xs) ], _),
      Runtime.VList (_, ys) ) ->
      Runtime.VList (item_typ, xs @ ys)
  | Runtime.VClosure (item_typ, Kernel.CGlobal "prim.List.reverse", [], _), Runtime.VList (_, xs) ->
      Runtime.VList (item_typ, List.rev xs)
  | Runtime.VClosure (out_typ, Kernel.CGlobal "prim.List.map", [], _), Runtime.VList (_, xs) ->
      Runtime.VClosure (out_typ, Kernel.CGlobal "prim.List.map", [ Runtime.VList (Ast.TUnit, xs) ], [])
  | Runtime.VClosure (out_typ, Kernel.CGlobal "prim.List.map", [ Runtime.VList (_, xs) ], _), fn ->
      Runtime.VList (out_typ, List.map (fun item -> apply_value m fn item) xs)
  | Runtime.VClosure (_, Kernel.CGlobal "prim.List.any", [], _), Runtime.VList (_, xs) ->
      Runtime.VClosure (Ast.TUnit, Kernel.CGlobal "prim.List.any", [ Runtime.VList (Ast.TUnit, xs) ], [])
  | Runtime.VClosure (_, Kernel.CGlobal "prim.List.any", [ Runtime.VList (_, xs) ], _), pred ->
      Runtime.VBool
        (List.exists (fun item -> expect_bool "List.any predicate" (apply_value m pred item)) xs)
  | Runtime.VClosure (_, Kernel.CGlobal "prim.List.all", [], _), Runtime.VList (_, xs) ->
      Runtime.VClosure (Ast.TUnit, Kernel.CGlobal "prim.List.all", [ Runtime.VList (Ast.TUnit, xs) ], [])
  | Runtime.VClosure (_, Kernel.CGlobal "prim.List.all", [ Runtime.VList (_, xs) ], _), pred ->
      Runtime.VBool
        (List.for_all (fun item -> expect_bool "List.all predicate" (apply_value m pred item)) xs)
  | Runtime.VClosure (_, Kernel.CGlobal "prim.List.member", [], _), eq ->
      Runtime.VClosure (Ast.TUnit, Kernel.CGlobal "prim.List.member", [ eq ], [])
  | Runtime.VClosure (_, Kernel.CGlobal "prim.List.member", [ eq ], _), value ->
      Runtime.VClosure (Ast.TUnit, Kernel.CGlobal "prim.List.member", [ eq; value ], [])
  | ( Runtime.VClosure (_, Kernel.CGlobal "prim.List.member", [ eq; value ], _),
      Runtime.VList (_, xs) ) ->
      Runtime.VBool
        (List.exists
           (fun item ->
             expect_bool "List.member equality" (apply_value m (apply_value m eq item) value))
           xs)
  | Runtime.VClosure (item_typ, Kernel.CGlobal "prim.List.find", [], _), Runtime.VList (_, xs) ->
      Runtime.VClosure
        (item_typ, Kernel.CGlobal "prim.List.find", [ Runtime.VList (item_typ, xs) ], [])
  | ( Runtime.VClosure (item_typ, Kernel.CGlobal "prim.List.find", [ Runtime.VList (_, xs) ], _),
      pred ) -> (
      match
        List.find_opt (fun item -> expect_bool "List.find predicate" (apply_value m pred item)) xs
      with
      | Some item -> Runtime.VVariant (Runtime.maybe_type item_typ, "Some", item)
      | None -> Runtime.VVariant (Runtime.maybe_type item_typ, "None", Runtime.VUnit))
  (* prim.Assoc.* *)
  | Runtime.VClosure (pair_typ, Kernel.CGlobal "prim.Assoc.insert", [], _), key ->
      Runtime.VClosure (pair_typ, Kernel.CGlobal "prim.Assoc.insert", [ key ], [])
  | Runtime.VClosure (pair_typ, Kernel.CGlobal "prim.Assoc.insert", [ key ], _), value ->
      Runtime.VClosure (pair_typ, Kernel.CGlobal "prim.Assoc.insert", [ key; value ], [])
  | ( Runtime.VClosure (pair_typ, Kernel.CGlobal "prim.Assoc.insert", [ key; value ], _),
      Runtime.VList (_, entries) ) ->
      Runtime.VList
        (pair_typ, Runtime.VRecord [ ("first", key); ("second", value) ] :: entries)
  | Runtime.VClosure (value_typ, Kernel.CGlobal "prim.Assoc.get", [], _), eq ->
      Runtime.VClosure (value_typ, Kernel.CGlobal "prim.Assoc.get", [ eq ], [])
  | Runtime.VClosure (value_typ, Kernel.CGlobal "prim.Assoc.get", [ eq ], _), key ->
      Runtime.VClosure (value_typ, Kernel.CGlobal "prim.Assoc.get", [ eq; key ], [])
  | ( Runtime.VClosure (value_typ, Kernel.CGlobal "prim.Assoc.get", [ eq; key ], _),
      Runtime.VList (_, entries) ) -> (
      match
        List.find_opt
          (fun entry ->
            let entry_key = Runtime.record_field "first" entry in
            expect_bool "Assoc.get equality" (apply_value m (apply_value m eq entry_key) key))
          entries
      with
      | Some entry ->
          Runtime.VVariant (Runtime.maybe_type value_typ, "Some", Runtime.record_field "second" entry)
      | None -> Runtime.VVariant (Runtime.maybe_type value_typ, "None", Runtime.VUnit))
  | Runtime.VClosure (_, Kernel.CGlobal "prim.Assoc.contains", [], _), eq ->
      Runtime.VClosure (Ast.TUnit, Kernel.CGlobal "prim.Assoc.contains", [ eq ], [])
  | Runtime.VClosure (_, Kernel.CGlobal "prim.Assoc.contains", [ eq ], _), key ->
      Runtime.VClosure (Ast.TUnit, Kernel.CGlobal "prim.Assoc.contains", [ eq; key ], [])
  | ( Runtime.VClosure (_, Kernel.CGlobal "prim.Assoc.contains", [ eq; key ], _),
      Runtime.VList (_, entries) ) ->
      Runtime.VBool
        (List.exists
           (fun entry ->
             let entry_key = Runtime.record_field "first" entry in
             expect_bool "Assoc.contains equality" (apply_value m (apply_value m eq entry_key) key))
           entries)
  | Runtime.VClosure (key_typ, Kernel.CGlobal "prim.Assoc.keys", [], _), Runtime.VList (_, entries) ->
      Runtime.VList (key_typ, List.map (Runtime.record_field "first") entries)
  | ( Runtime.VClosure (value_typ, Kernel.CGlobal "prim.Assoc.values", [], _),
      Runtime.VList (_, entries) ) ->
      Runtime.VList (value_typ, List.map (Runtime.record_field "second") entries)
  (* Generic user closure: restore+merge the closure's captured cap-scope, bind
     the argument as the new innermost slot, and run the body. We execute the
     bytecode of the body cterm (re-compiled), keeping application on the VM. *)
  | Runtime.VClosure (_, body, cenv, cap_scope), _ ->
      with_cap_scope m cap_scope (fun () -> exec_block m (av :: cenv) (Bytecode.compile body))
  | v, _ -> fail ("application of non-function: " ^ Runtime.value_to_string v)

(* --- case / fold / list eliminators (= the matching Runtime.eval_cterm arms) --- *)
and exec_case m env scrut branches =
  match scrut with
  | Runtime.VBool b ->
      let body =
        List.find_map
          (function Bytecode.BoolBranch (b', body) when b = b' -> Some body | _ -> None)
          branches
      in
      exec_block m env (option_or_fail "missing Bool branch" body)
  | Runtime.VVariant (_, con, payload) ->
      let body =
        List.find_map
          (function
            | Bytecode.VariantBranch (con', body) when String.equal con con' -> Some body
            | _ -> None)
          branches
      in
      exec_block m (payload :: env) (option_or_fail ("missing Variant branch: " ^ con) body)
  | v -> fail ("case on invalid value: " ^ Runtime.value_to_string v)

and exec_fold_nat m env n zero step =
  match n with
  | Runtime.VNat count ->
      let step_value = exec_block m env step in
      let rec loop i acc = if i <= 0 then acc else loop (i - 1) (apply_value m step_value acc) in
      loop count (exec_block m env zero)
  | v -> fail ("foldNat on non-Nat: " ^ Runtime.value_to_string v)

and exec_fold_list m env xs zero step =
  match xs with
  | Runtime.VList (_, items) ->
      let step_value = exec_block m env step in
      List.fold_right
        (fun item acc -> apply_value m (apply_value m step_value item) acc)
        items (exec_block m env zero)
  | v -> fail ("foldList on non-List: " ^ Runtime.value_to_string v)

and exec_case_list m env xs nil_body cons_body =
  match xs with
  | Runtime.VList (_, []) -> exec_block m env nil_body
  | Runtime.VList (item_ty, head :: tail) ->
      (* cons body sees #0 = head, #1 = tail (head pushed last = innermost). *)
      exec_block m (head :: Runtime.VList (item_ty, tail) :: env) cons_body
  | v -> fail ("caseList on non-List: " ^ Runtime.value_to_string v)

(* foldVariant: structural recursion over a recursive variant. Push a recur
   frame ([fold] itself) for the dynamic extent of each branch body so a [Recur]
   inside reaches back into [fold]; identical to [Runtime]'s [recur_stack]. *)
and exec_fold_variant m env scrut branches =
  let rec fold value =
    match value with
    | Runtime.VVariant (_, con, payload) ->
        let body =
          List.find_map
            (function
              | Bytecode.VariantBranch (con', body) when String.equal con con' -> Some body
              | _ -> None)
            branches
        in
        let body = option_or_fail ("missing foldVariant branch: " ^ con) body in
        let previous = m.recur_stack in
        m.recur_stack <- fold :: previous;
        Fun.protect
          ~finally:(fun () -> m.recur_stack <- previous)
          (fun () -> exec_block m (payload :: env) body)
    | v -> fail ("foldVariant on non-Variant: " ^ Runtime.value_to_string v)
  in
  fold scrut

(* bind: [Runtime.eval_cterm]'s [CBind]. A [done] result feeds the body; a
   request threads the body into the continuation, stamping the current scope. *)
and exec_bind m env p body =
  match p with
  | Runtime.VProcessDone v -> exec_block m (v :: env) body
  | Runtime.VProcessRequest s ->
      Runtime.VProcessRequest
        { s with cont = Runtime.KBind (s.cont, decompile_block body, env, m.cap_scope) }
  | other -> fail ("bind on non-process: " ^ Runtime.value_to_string other)

(* --- stream / automaton stepping (= Runtime.stream_step/automaton_step) --- *)
and stream_step m state step =
  match Runtime.force_value (apply_value m step state) with
  | Runtime.VRecord _ as r ->
      (Runtime.record_field "head" r, Runtime.record_field "state" r)
  | value -> fail ("coiter step returned non-record: " ^ Runtime.value_to_string value)

and automaton_step m state transition =
  match Runtime.force_value (apply_value m transition state) with
  | Runtime.VRecord _ as r ->
      (Runtime.record_field "output" r, Runtime.record_field "state" r)
  | value -> fail ("automaton transition returned non-record: " ^ Runtime.value_to_string value)

(* --- small helpers (local copies of Runtime's, specialised to VM errors) --- *)
and project_field field = function
  | Runtime.VRecord fields -> (
      match Kernel.assoc_opt field fields with
      | Some v -> v
      | None -> fail ("unknown record field: " ^ field))
  | v -> fail ("field access on non-record: " ^ Runtime.value_to_string v)

and expect_view = function
  | Runtime.VView view -> view
  | v -> fail ("expected View value, got " ^ Runtime.value_to_string v)

and expect_attr = function
  | Runtime.VAttribute a -> a
  | v -> fail ("expected Attr value, got " ^ Runtime.value_to_string v)

and expect_bool context = function
  | Runtime.VBool value -> value
  | v -> fail (context ^ " returned non-Bool value: " ^ Runtime.value_to_string v)

and option_or_fail message = function Some v -> v | None -> fail message

(* ------------------------------------------------------------------------- *)
(* Public entry points                                                        *)
(* ------------------------------------------------------------------------- *)

(* Build a machine from a [Kernel.checked]: the resolver knows each def's
   compiled block (from the bytecode module), declared capabilities, and body
   cterm (for [CInst]). We index by both def name and def-id so [CGlobal]/[CInst]
   references resolve under either, matching [Runtime.def_by_ref]. *)
let machine_of_checked (checked : Kernel.checked) (m : Bytecode.module_) : machine =
  (* name/def-id -> compiled block, from the module. *)
  let block_by : (string, Bytecode.block) Hashtbl.t = Hashtbl.create 256 in
  List.iter
    (fun (d : Bytecode.bc_def) ->
      if not (Hashtbl.mem block_by d.Bytecode.bc_name) then
        Hashtbl.add block_by d.Bytecode.bc_name d.Bytecode.bc_code;
      if not (Hashtbl.mem block_by d.Bytecode.bc_def_id) then
        Hashtbl.add block_by d.Bytecode.bc_def_id d.Bytecode.bc_code)
    m.Bytecode.bc_defs;
  (* name/def-id -> (capabilities, body cterm), from the checked program. The
     body cterm is parsed from the canonical serialization, exactly as
     [Runtime.eval_def]/[CInst] do via [parse_serialized_def]. *)
  let meta_by : (string, string list * Kernel.cterm) Hashtbl.t = Hashtbl.create 256 in
  List.iter
    (fun (d : Kernel.checked_def) ->
      let body = (Kernel.parse_serialized_def d.Kernel.canonical).Kernel.cbody in
      let entry = (d.Kernel.capabilities, body) in
      if not (Hashtbl.mem meta_by d.Kernel.def.Ast.name) then
        Hashtbl.add meta_by d.Kernel.def.Ast.name entry;
      if not (Hashtbl.mem meta_by d.Kernel.def_id) then Hashtbl.add meta_by d.Kernel.def_id entry)
    checked.Kernel.defs;
  let resolve n =
    match (Hashtbl.find_opt block_by n, Hashtbl.find_opt meta_by n) with
    | Some block, Some (caps, body) -> Some { ge_block = block; ge_caps = caps; ge_body = body }
    | _ -> None
  in
  { resolve; globals = Hashtbl.create 256; cap_scope = []; recur_stack = [] }

(* Run the named def of a checked program on the VM and return the raw value.
   Mirrors [Runtime.eval_entry]: start from the def's declared capability scope
   (the empty initial scope merged with the def's caps), run its body in the
   empty environment. *)
let exec_checked (checked : Kernel.checked) (name : string) : Runtime.value =
  let m = Bytecode.compile_checked checked in
  let machine = machine_of_checked checked m in
  match machine.resolve name with
  | None -> fail ("unknown definition: " ^ name)
  | Some ge -> with_cap_scope machine ge.ge_caps (fun () -> exec_block machine [] ge.ge_block)

(* THE PARITY ENTRY POINT.

   The test asserts:
     Bytecode_vm.vm_canonical checked name
       = Runtime.value_to_canonical (fst (Runtime.normalize_def checked name))
*)
let vm_canonical (checked : Kernel.checked) (name : string) : string =
  Runtime.value_to_canonical (exec_checked checked name)

(* Execute a bare bytecode module (no [checked] context). Globals resolve among
   the module's own defs; capabilities are unknown so default to [] and [CInst]
   recovers the referenced body by decompiling its block. This suffices for pure
   (capability-free) modules; for capability-bearing programs prefer
   [exec_checked]/[vm_canonical], which carry the declared scopes. *)
let exec_module (m : Bytecode.module_) (name : string) : Runtime.value =
  let by : (string, Bytecode.block) Hashtbl.t = Hashtbl.create 256 in
  List.iter
    (fun (d : Bytecode.bc_def) ->
      if not (Hashtbl.mem by d.Bytecode.bc_name) then
        Hashtbl.add by d.Bytecode.bc_name d.Bytecode.bc_code;
      if not (Hashtbl.mem by d.Bytecode.bc_def_id) then
        Hashtbl.add by d.Bytecode.bc_def_id d.Bytecode.bc_code)
    m.Bytecode.bc_defs;
  let resolve n =
    match Hashtbl.find_opt by n with
    | Some block -> Some { ge_block = block; ge_caps = []; ge_body = decompile_block block }
    | None -> None
  in
  let machine = { resolve; globals = Hashtbl.create 256; cap_scope = []; recur_stack = [] } in
  match machine.resolve name with
  | None -> fail ("unknown definition: " ^ name)
  | Some ge -> exec_block machine [] ge.ge_block
