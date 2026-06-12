type t = Sexp.t

let parse = Sexp.parse

let to_string = Sexp.to_string

(* ------------------------------------------------------------------ *)
(* Protoss/H emitter: project a surface AST back to Elm-like text.     *)
(*                                                                     *)
(* The contract is hash round-trip: parsing the rendered text must     *)
(* produce a program whose canonical hash equals the original's. The   *)
(* Elm-like parser is the reference for what is expressible:           *)
(*  - parenthesized juxtaposition reproduces any S-expression form     *)
(*    (Elm parens are transparent), so special forms without human     *)
(*    sugar are emitted as plain applications;                         *)
(*  - sugar (if/case/let/records/lists/operators/field access) is      *)
(*    emitted only where re-parsing provably reconstructs the same     *)
(*    canonical term;                                                  *)
(*  - forms with no Protoss/H projection (defpoly, Clock.read, a       *)
(*    case/let in inline position, function types in expression        *)
(*    position) raise Unrenderable instead of emitting text that       *)
(*    would parse to something else.                                   *)
(* ------------------------------------------------------------------ *)

exception Unrenderable of string

let unrenderable msg = raise (Unrenderable msg)

let is_ident_start = function 'A' .. 'Z' | 'a' .. 'z' | '_' -> true | _ -> false

let is_ident_char = function
  | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' | '.' -> true
  | _ -> false

let is_lower_start s = s <> "" && (match s.[0] with 'a' .. 'z' | '_' -> true | _ -> false)

(* Words the Elm-like tokenizer or layout reader treats specially: using one
   as a name would change how the rendered text re-parses. *)
let reserved_human_name = function
  | "if" | "then" | "else" | "let" | "in" | "case" | "of" | "not" | "type" | "alias"
  | "module" | "import" | "export" | "exposing" | "capabilities" ->
      true
  | _ -> false

let plausible_ident n =
  n <> "" && is_ident_start n.[0] && String.for_all is_ident_char n

let check_name what n =
  if not (plausible_ident n) then
    unrenderable (what ^ " has no Protoss/H spelling: " ^ n);
  if reserved_human_name n then
    unrenderable (what ^ " collides with a Protoss/H keyword: " ^ n);
  n

(* A lowercase dotted name re-parses as field access, so it cannot appear as
   an expression-position name in Protoss/H. *)
let check_expr_name what n =
  let n = check_name what n in
  if is_lower_start n && String.contains n '.' then
    unrenderable (what ^ " would re-parse as field access: " ^ n);
  n

let check_binder what n =
  let n = check_expr_name what n in
  if String.contains n '.' then unrenderable (what ^ " contains a dot: " ^ n);
  n

let quote_import path =
  if String.exists (function '"' | '\\' | '\n' | '\r' -> true | _ -> false) path then
    unrenderable ("import path has no Protoss/H spelling: " ^ path);
  "\"" ^ path ^ "\""

(* --- types in type position (signatures, aliases, let annotations) --- *)

let rec type_is_atom = function
  | Ast.TUnit | Ast.TBool | Ast.TNat | Ast.TString | Ast.TNamed (_, []) -> true
  | Ast.TRecord _ -> true
  | _ -> false

and render_type t =
  match t with
  | Ast.TFun (a, b) ->
      let left = if type_is_atom a then render_type a else render_type_left a in
      left ^ " -> " ^ render_type b
  | _ -> render_type_app t

and render_type_left = function
  | Ast.TFun _ as t -> "(" ^ render_type t ^ ")"
  | t -> render_type_app t

and render_type_app t =
  match t with
  | Ast.TUnit -> "Unit"
  | Ast.TBool -> "Bool"
  | Ast.TNat -> "Nat"
  | Ast.TString -> "String"
  | Ast.TNamed (n, []) -> check_name "type name" n
  | Ast.TNamed (n, args) ->
      check_name "type name" n ^ " " ^ String.concat " " (List.map render_type_atom args)
  | Ast.TList t -> "List " ^ render_type_atom t
  | Ast.TView t -> "View " ^ render_type_atom t
  | Ast.TAttr t -> "Attr " ^ render_type_atom t
  | Ast.TStream t -> "Stream " ^ render_type_atom t
  | Ast.TAutomaton (s, o) -> "Automaton " ^ render_type_atom s ^ " " ^ render_type_atom o
  | Ast.TProcess (None, t) -> "Process " ^ render_type_atom t
  | Ast.TProcess (Some caps, t) ->
      "Process " ^ render_capability_args caps ^ " " ^ render_type_atom t
  | Ast.TCmd (None, t) -> "Cmd " ^ render_type_atom t
  | Ast.TCmd (Some caps, t) -> "Cmd " ^ render_capability_args caps ^ " " ^ render_type_atom t
  | Ast.TSecretRef (scope, t) ->
      "SecretRef " ^ check_name "secret scope" scope ^ " " ^ render_type_atom t
  | Ast.TRecord [] -> "{}"
  | Ast.TRecord fields ->
      "{ "
      ^ String.concat ", "
          (List.map
             (fun (n, t) -> check_name "record field" n ^ " : " ^ render_type t)
             fields)
      ^ " }"
  | Ast.TVariant cases ->
      if cases = [] then unrenderable "empty variant type";
      "Variant "
      ^ String.concat " "
          (List.map
             (fun (n, t) ->
               "(" ^ check_name "variant constructor" n ^ " " ^ render_type_atom t ^ ")")
             cases)
  | Ast.TFun _ -> "(" ^ render_type t ^ ")"
  | Ast.TVar _ -> unrenderable "type variable has no Protoss/H spelling"
  | Ast.TForall _ -> unrenderable "polymorphic type has no Protoss/H spelling"

and render_type_atom t =
  if type_is_atom t then render_type_app t else "(" ^ render_type t ^ ")"

and render_capability_args caps =
  List.iter (fun c -> ignore (check_name "capability" c)) caps;
  "(capabilities" ^ (match caps with [] -> "" | _ -> " " ^ String.concat " " caps) ^ ")"

(* --- types in expression position (variant/inst/Nil/Cons/coiter/...) ---
   These re-parse through the *expression* grammar, so braces and arrows are
   unavailable; only atoms and parenthesized juxtaposition survive. *)

let rec render_type_in_expr t =
  match t with
  | Ast.TUnit -> "Unit"
  | Ast.TBool -> "Bool"
  | Ast.TNat -> "Nat"
  | Ast.TString -> "String"
  | Ast.TNamed (n, []) -> check_expr_name "type name" n
  | Ast.TNamed (n, args) ->
      "(" ^ check_expr_name "type name" n ^ " "
      ^ String.concat " " (List.map render_type_in_expr args)
      ^ ")"
  | Ast.TList t -> "(List " ^ render_type_in_expr t ^ ")"
  | Ast.TView t -> "(View " ^ render_type_in_expr t ^ ")"
  | Ast.TAttr t -> "(Attr " ^ render_type_in_expr t ^ ")"
  | Ast.TStream t -> "(Stream " ^ render_type_in_expr t ^ ")"
  | Ast.TAutomaton (s, o) ->
      "(Automaton " ^ render_type_in_expr s ^ " " ^ render_type_in_expr o ^ ")"
  | Ast.TProcess (None, t) -> "(Process " ^ render_type_in_expr t ^ ")"
  | Ast.TProcess (Some caps, t) ->
      "(Process " ^ render_capability_args caps ^ " " ^ render_type_in_expr t ^ ")"
  | Ast.TCmd (None, t) -> "(Cmd " ^ render_type_in_expr t ^ ")"
  | Ast.TCmd (Some caps, t) ->
      "(Cmd " ^ render_capability_args caps ^ " " ^ render_type_in_expr t ^ ")"
  | Ast.TSecretRef (scope, t) ->
      "(SecretRef " ^ check_expr_name "secret scope" scope ^ " " ^ render_type_in_expr t ^ ")"
  | Ast.TRecord fields ->
      "(Record "
      ^ String.concat " "
          (List.map
             (fun (n, t) ->
               "(" ^ check_expr_name "record field" n ^ " " ^ render_type_in_expr t ^ ")")
             fields)
      ^ ")"
  | Ast.TVariant cases ->
      "(Variant "
      ^ String.concat " "
          (List.map
             (fun (n, t) ->
               "(" ^ check_expr_name "variant constructor" n ^ " " ^ render_type_in_expr t ^ ")")
             cases)
      ^ ")"
  | Ast.TFun _ -> unrenderable "function type in expression position has no Protoss/H spelling"
  | Ast.TVar _ -> unrenderable "type variable has no Protoss/H spelling"
  | Ast.TForall _ -> unrenderable "polymorphic type has no Protoss/H spelling"

(* --- expression sugar detection --- *)

(* Operator precedence levels mirror the Elm-like expression parser:
   1 = ||, 2 = &&, 3 = comparisons (non-associative), 4 = +, 5 = application,
   6 = atom. Lambdas and if/then/else are always parenthesized inline. *)

let infix_call = function
  | Ast.EApp (Ast.EApp (Ast.EName fn, a), b) -> Some (fn, a, b)
  | _ -> None

let comparison_op = function
  | "Nat.eqNat" -> Some "=="
  | "Nat.lt" -> Some "<"
  | "Nat.lte" -> Some "<="
  | "Nat.gt" -> Some ">"
  | "Nat.gte" -> Some ">="
  | _ -> None

(* a + b desugars to (foldNat a b (lambda acc (succ acc))); recognize that
   shape (alpha-insensitively: canonical hashing ignores the binder name). *)
let plus_operands = function
  | Ast.EFoldNat
      ( a,
        b,
        ( Ast.ELambdaInfer (x, Ast.EApp (Ast.EName "succ", Ast.EName y))
        | Ast.ELambda (x, Ast.TNat, Ast.EApp (Ast.EName "succ", Ast.EName y)) ) )
    when String.equal x y ->
      Some (a, b)
  | _ -> None

let list_literal_elements e =
  let rec loop acc = function
    | Ast.EConsInfer (head, tail) -> loop (head :: acc) tail
    | Ast.ENilInfer -> Some (List.rev acc)
    | _ -> None
  in
  match e with Ast.EConsInfer _ -> loop [] e | _ -> None

(* model.count style access: only lowercase dotless bases survive re-parsing
   as field access; everything else uses the (get e field) form. *)
let field_chain e =
  let rec loop fields = function
    | Ast.EField (base, field) when plausible_ident field && not (String.contains field '.')
      ->
        loop (field :: fields) base
    | Ast.EName base
      when is_lower_start base
           && plausible_ident base
           && (not (String.contains base '.'))
           && (not (reserved_human_name base))
           && fields <> [] ->
        Some (String.concat "." (base :: fields))
    | _ -> None
  in
  loop [] e

let app_spine e =
  let rec loop acc = function
    | Ast.EApp (f, x) -> loop (x :: acc) f
    | head -> (head, acc)
  in
  loop [] e

let if_branches = function
  | Ast.ECase (cond, [ Ast.BBool (true, a); Ast.BBool (false, b) ]) -> Some (cond, a, b)
  | _ -> None

(* Branch constructors named Nil/Cons would make the re-parsed match dispatch
   to the list form, changing the term. *)
let check_case_constructor con =
  let con = check_expr_name "case constructor" con in
  if String.equal con "Nil" || String.equal con "Cons" then
    unrenderable ("case constructor would re-parse as a list match: " ^ con);
  con

let rec lambda_params seen e =
  match e with
  | Ast.ELambdaInfer (x, body) | Ast.ELambda (x, _, body) ->
      if
        plausible_ident x
        && (not (String.contains x '.'))
        && (not (reserved_human_name x))
        && not (List.exists (String.equal x) seen)
      then
        let params, core = lambda_params (x :: seen) body in
        (x :: params, core)
      else ([], e)
  | _ -> ([], e)

(* --- inline expression rendering --- *)

let rec render_inline lvl e =
  let at natural text = if natural < lvl then "(" ^ text ^ ")" else text in
  match list_literal_elements e with
  | Some elems ->
      "[" ^ String.concat ", " (List.map (render_inline 1) elems) ^ "]"
  | None -> (
      match field_chain e with
      | Some path -> path
      | None -> (
          match if_branches e with
          | Some (cond, a, b) ->
              "(if " ^ render_inline 1 cond ^ " then " ^ render_inline 1 a ^ " else "
              ^ render_inline 1 b ^ ")"
          | None -> (
              match plus_operands e with
              | Some (a, b) -> at 4 (render_inline 4 a ^ " + " ^ render_inline 5 b)
              | None -> render_inline_core lvl at e)))

and render_inline_core lvl at e =
  match e with
  | Ast.EApp (Ast.EName "Bool.not", Ast.EApp (Ast.EApp (Ast.EName "Nat.eqNat", a), b)) ->
      at 3 (render_inline 4 a ^ " /= " ^ render_inline 4 b)
  | Ast.EApp (Ast.EName "Bool.not", x) -> at 5 ("not " ^ render_inline 6 x)
  | _ -> (
      match infix_call e with
      | Some ("Bool.or", a, b) -> at 1 (render_inline 1 a ^ " || " ^ render_inline 2 b)
      | Some ("Bool.and", a, b) -> at 2 (render_inline 2 a ^ " && " ^ render_inline 3 b)
      | Some (fn, a, b) when comparison_op fn <> None ->
          let op = Option.get (comparison_op fn) in
          at 3 (render_inline 4 a ^ " " ^ op ^ " " ^ render_inline 4 b)
      | _ -> render_inline_plain lvl at e)

and render_inline_plain _lvl at e =
  let arg = render_inline 6 in
  let app head args = at 5 (String.concat " " (head :: args)) in
  match e with
  | Ast.EUnit -> "unit"
  | Ast.EBool true -> "true"
  | Ast.EBool false -> "false"
  | Ast.ENat n -> string_of_int n
  | Ast.EString s -> Ast.quote s
  | Ast.EName n -> check_expr_name "name" n
  | Ast.ELambdaInfer _ | Ast.ELambda _ ->
      let params, core = lambda_params [] e in
      if params = [] then unrenderable "lambda binder has no Protoss/H spelling"
      else "(\\" ^ String.concat " " params ^ " -> " ^ render_inline 1 core ^ ")"
  | Ast.ELet _ | Ast.ELetAnnot _ ->
      unrenderable "let in inline position has no Protoss/H spelling"
  | Ast.ECase _ -> unrenderable "case in inline position has no Protoss/H spelling"
  | Ast.ECaseList _ -> unrenderable "caseList in inline position has no Protoss/H spelling"
  | Ast.ELetRecord (record, fields, body) ->
      (* Emitted as the record-match form: a bare (letRecord r (f) body)
         collapses under Elm's transparent parens when there is one field,
         while (match r ((record fields) body)) survives every arity. *)
      let binding (field, binder) =
        let field = check_expr_name "letRecord field" field in
        if String.equal field binder then field
        else "(" ^ field ^ " " ^ check_binder "letRecord binder" binder ^ ")"
      in
      app "match"
        [
          arg record;
          "((record " ^ String.concat " " (List.map binding fields) ^ ") " ^ arg body ^ ")";
        ]
  | Ast.ERecord [] -> "{}"
  | Ast.ERecord fields ->
      "{ "
      ^ String.concat ", "
          (List.map
             (fun (n, e) -> check_expr_name "record field" n ^ " = " ^ render_inline 1 e)
             fields)
      ^ " }"
  | Ast.ERecordUpdate (record, updates) ->
      "{ " ^ render_inline 1 record ^ " | "
      ^ String.concat ", "
          (List.map
             (fun (n, e) -> check_expr_name "record field" n ^ " = " ^ render_inline 1 e)
             updates)
      ^ " }"
  | Ast.EField (base, field) ->
      ignore (check_name "field" field);
      app "get" [ arg base; field ]
  | Ast.EVariant (ty, con, e) ->
      app "variant" [ render_type_in_expr ty; check_expr_name "constructor" con; arg e ]
  | Ast.EVariantInferred (con, e) ->
      app "variant" [ check_expr_name "constructor" con; arg e ]
  | Ast.EInst (n, args) ->
      app "inst" (check_expr_name "name" n :: List.map render_type_in_expr args)
  | Ast.EFoldNat (n, z, step) -> app "foldNat" [ arg n; arg z; arg step ]
  | Ast.EFoldList (xs, z, step) -> app "foldList" [ arg xs; arg z; arg step ]
  | Ast.EFoldVariant (target, result, scrut, branches) ->
      app "foldVariant"
        (render_type_in_expr target :: render_type_in_expr result :: arg scrut
        :: List.map render_branch_inline branches)
  | Ast.ERecur e -> app "recur" [ arg e ]
  | Ast.ENilInfer -> "Nil"
  | Ast.ENil ty -> app "Nil" [ render_type_in_expr ty ]
  | Ast.ECons (ty, head, tail) -> app "Cons" [ render_type_in_expr ty; arg head; arg tail ]
  | Ast.EConsInfer (head, tail) -> app "Cons" [ arg head; arg tail ]
  | Ast.ECoiter (state_ty, item_ty, seed, step) ->
      app "coiter"
        [ render_type_in_expr state_ty; render_type_in_expr item_ty; arg seed; arg step ]
  | Ast.EStreamHead s -> app "streamHead" [ arg s ]
  | Ast.EStreamTail s -> app "streamTail" [ arg s ]
  | Ast.EStreamTake (count, s) -> app "streamTake" [ arg count; arg s ]
  | Ast.EAutomaton (state_ty, output_ty, initial, transition) ->
      app "automaton"
        [
          render_type_in_expr state_ty;
          render_type_in_expr output_ty;
          arg initial;
          arg transition;
        ]
  | Ast.EAutomatonRun (count, a) -> app "automatonRun" [ arg count; arg a ]
  | Ast.EStrict e -> app "strict" [ arg e ]
  | Ast.EText e -> app "text" [ arg e ]
  | Ast.EImage (src, alt) -> app "image" [ arg src; arg alt ]
  | Ast.EButton (label, msg) -> app "button" [ arg label; arg msg ]
  | Ast.EInput (value, handler) -> app "input" [ arg value; arg handler ]
  | Ast.EColumn children -> app "column" [ arg children ]
  | Ast.ERow children -> app "row" [ arg children ]
  | Ast.EListView (items, render) -> app "list" [ arg items; arg render ]
  | Ast.EWhenView (cond, view) -> app "when" [ arg cond; arg view ]
  | Ast.ENode (tag, attrs, children) -> app "node" [ arg tag; arg attrs; arg children ]
  | Ast.EAttr (name, value) -> app "attr" [ arg name; arg value ]
  | Ast.EOn (event, msg) -> app "on" [ arg event; arg msg ]
  | Ast.EDone e -> app "done" [ arg e ]
  | Ast.ERequest (Ast.AskHuman prompt) -> app "Human.ask" [ Ast.quote prompt ]
  | Ast.ERequest (Ast.HttpGet url) -> app "Http.get" [ Ast.quote url ]
  | Ast.ERequest Ast.ReadClock ->
      unrenderable "Clock.read has no Protoss/H spelling (single-atom request form)"
  | Ast.ERequest (Ast.SaveLocal (key, value)) ->
      app "Local.save" [ Ast.quote key; Ast.quote value ]
  | Ast.ERequest (Ast.LoadLocal key) -> app "Local.load" [ Ast.quote key ]
  | Ast.ERequest (Ast.ServerRequest (route, payload)) ->
      app "Server.request" [ Ast.quote route; Ast.quote payload ]
  | Ast.ESendToBackend (_, payload) -> app "sendToBackend" [ arg payload ]
  | Ast.EBroadcast (_, payload) -> app "broadcast" [ arg payload ]
  | Ast.EBind (p, x, _, body) | Ast.EBindInfer (p, x, body) ->
      app "bind"
        [
          arg p;
          "(\\" ^ check_binder "bind binder" x ^ " -> " ^ render_inline 1 body ^ ")";
        ]
  | Ast.EApp _ ->
      let head, args = app_spine e in
      app (render_inline 6 head) (List.map arg args)

and render_branch_inline = function
  | Ast.BBool (true, e) -> "(true " ^ render_inline 6 e ^ ")"
  | Ast.BBool (false, e) -> "(false " ^ render_inline 6 e ^ ")"
  | Ast.BVariant (con, x, e) ->
      "(" ^ check_expr_name "constructor" con ^ " " ^ check_binder "branch binder" x ^ " "
      ^ render_inline 6 e ^ ")"
  | Ast.BVariantUnit (con, e) ->
      "(" ^ check_expr_name "constructor" con ^ " " ^ render_inline 6 e ^ ")"
  | Ast.BWildcard e -> "(_ " ^ render_inline 6 e ^ ")"

(* --- block (layout) rendering for tail positions --- *)

let indent_text n = String.make n ' '

let inline_fits text = String.length text <= 76 && not (String.contains text '\n')

(* A rendered scrutinee containing " of" would truncate the case header at the
   wrong place (the header is split on the first " of"). *)
let check_scrutinee text =
  let rec has_of i =
    match String.index_from_opt text i 'o' with
    | None -> false
    | Some j ->
        (j + 1 < String.length text
        && text.[j + 1] = 'f'
        && j > 0
        && text.[j - 1] = ' '
        && (j + 2 = String.length text || text.[j + 2] = ' '))
        || has_of (j + 1)
  in
  if has_of 0 then unrenderable "case scrutinee renders with a stray ' of'" else text

let rec render_block indent e =
  match e with
  | Ast.ELet _ | Ast.ELetAnnot _ -> render_let_block indent e
  | Ast.ECase (scrut, branches) -> (
      match if_branches e with
      | Some _ -> (
          match try_inline e with
          | Some text -> [ indent_text indent ^ strip_outer_parens text ]
          | None -> render_case_block indent scrut (`Branches branches))
      | None -> render_case_block indent scrut (`Branches branches))
  | Ast.ECaseList (xs, nil_body, head, tail, cons_body) ->
      render_case_block indent xs (`ListBranches (nil_body, head, tail, cons_body))
  | _ -> (
      match try_inline e with
      | Some text -> [ indent_text indent ^ strip_outer_parens text ]
      | None -> [ indent_text indent ^ render_inline 1 e ])

(* In tail position a lambda or if needs no surrounding parens; drop them for
   readability (the text between the parens is a complete expression). *)
and strip_outer_parens text =
  let len = String.length text in
  if len >= 2 && text.[0] = '(' && text.[len - 1] = ')' then
    let rec balanced depth i =
      if i = len - 1 then depth = 1
      else
        match text.[i] with
        | '(' -> balanced (depth + 1) (i + 1)
        | ')' -> depth > 1 && balanced (depth - 1) (i + 1)
        | '"' -> (
            match skip_string (i + 1) with
            | None -> false
            | Some j -> balanced depth j)
        | _ -> balanced depth (i + 1)
    and skip_string i =
      if i >= len then None
      else
        match text.[i] with
        | '"' -> Some (i + 1)
        | '\\' -> skip_string (i + 2)
        | _ -> skip_string (i + 1)
    in
    if balanced 1 1 then String.sub text 1 (len - 2) else text
  else text

and try_inline e = try Some (render_inline 1 e) with Unrenderable _ -> None

and render_case_block indent scrut branches =
  let scrut_text = check_scrutinee (render_inline 1 scrut) in
  let header = indent_text indent ^ "case " ^ scrut_text ^ " of" in
  let branch_lines =
    match branches with
    | `Branches branches ->
        List.concat_map
          (fun branch ->
            let pattern, body =
              match branch with
              | Ast.BBool (true, e) -> ("true", e)
              | Ast.BBool (false, e) -> ("false", e)
              | Ast.BWildcard e -> ("_", e)
              | Ast.BVariantUnit (con, e) -> (check_case_constructor con, e)
              | Ast.BVariant (con, x, e) ->
                  (check_case_constructor con ^ " " ^ check_binder "branch binder" x, e)
            in
            render_branch_lines (indent + 4) pattern body)
          branches
    | `ListBranches (nil_body, head, tail, cons_body) ->
        render_branch_lines (indent + 4) "Nil" nil_body
        @ render_branch_lines (indent + 4)
            ("Cons "
            ^ check_binder "list head binder" head
            ^ " "
            ^ check_binder "list tail binder" tail)
            cons_body
  in
  header :: branch_lines

and render_branch_lines indent pattern body =
  match try_inline body with
  | Some text when inline_fits text ->
      [ indent_text indent ^ pattern ^ " -> " ^ strip_outer_parens text ]
  | _ -> (indent_text indent ^ pattern ^ " ->") :: render_block (indent + 4) body

(* Group consecutive lets into one block while binding names stay distinct
   (the Elm-like let reader keys signatures by name, so duplicates must split
   into nested blocks). *)
and render_let_block indent e =
  let rec gather seen acc e =
    match e with
    | Ast.ELet (x, rhs, body) when not (List.exists (String.equal x) seen) ->
        gather (x :: seen) ((x, None, rhs) :: acc) body
    | Ast.ELetAnnot (x, ty, rhs, body) when not (List.exists (String.equal x) seen) ->
        gather (x :: seen) ((x, Some ty, rhs) :: acc) body
    | _ -> (List.rev acc, e)
  in
  let bindings, body = gather [] [] e in
  let binding_lines =
    List.concat_map
      (fun (x, ty, rhs) ->
        let x = check_binder "let binder" x in
        let signature =
          match ty with
          | None -> []
          | Some ty -> [ indent_text (indent + 4) ^ x ^ " : " ^ render_type ty ]
        in
        let value =
          match try_inline rhs with
          | Some text when inline_fits text ->
              [ indent_text (indent + 4) ^ x ^ " = " ^ strip_outer_parens text ]
          | _ -> (indent_text (indent + 4) ^ x ^ " =") :: render_block (indent + 8) rhs
        in
        signature @ value)
      bindings
  in
  (indent_text indent ^ "let")
  :: binding_lines
  @ (indent_text indent ^ "in") :: render_block (indent + 4) body

(* --- top-level declarations --- *)

let tuple_record_payload fields =
  List.length fields >= 2
  && List.for_all2
       (fun (name, _) index -> String.equal name ("_" ^ string_of_int (index + 1)))
       fields
       (List.init (List.length fields) Fun.id)

let render_variant_case (con, payload) =
  let con = check_name "variant constructor" con in
  match payload with
  | Ast.TUnit -> con
  | Ast.TRecord fields when tuple_record_payload fields ->
      con ^ " " ^ String.concat " " (List.map (fun (_, t) -> render_type_atom t) fields)
  | t -> con ^ " " ^ render_type_atom t

let render_type_alias (alias : Ast.type_alias) =
  let name = check_name "type name" alias.Ast.type_name in
  let params =
    List.map (fun p -> check_name "type parameter" p) alias.Ast.type_params
  in
  let header_params = match params with [] -> "" | _ -> " " ^ String.concat " " params in
  match alias.Ast.type_body with
  | Ast.TVariant cases when cases <> [] ->
      "type " ^ name ^ header_params ^ " = "
      ^ String.concat " | " (List.map render_variant_case cases)
  | body -> "type alias " ^ name ^ header_params ^ " = " ^ render_type body

let render_signature_type (d : Ast.def) =
  match (d.Ast.declared_capabilities, d.Ast.typ) with
  | Some declared, Ast.TProcess (Some caps, value_ty)
    when List.length declared = List.length caps && List.for_all2 String.equal declared caps
    ->
      (* Only an explicit matching scope survives: Process { ... } re-parses
         to TProcess Some, which hashes differently from a legacy
         scope-free Process type. *)
      List.iter (fun c -> ignore (check_name "capability" c)) declared;
      "Process { " ^ String.concat ", " declared ^ " } " ^ render_type value_ty
  | Some _, _ ->
      unrenderable
        ("defcap " ^ d.Ast.name
       ^ " has no Protoss/H spelling (capabilities are declared through a leading \
          Process { ... } signature)")
  | None, ty -> render_type ty

let render_def (d : Ast.def) =
  if d.Ast.type_params <> [] then
    unrenderable ("defpoly " ^ d.Ast.name ^ " has no Protoss/H spelling");
  let name = check_name "definition name" d.Ast.name in
  let signature = name ^ " : " ^ render_signature_type d in
  let params, core = lambda_params [] d.Ast.body in
  let lhs = String.concat " " (name :: params) in
  let body_lines =
    match try_inline core with
    | Some text when inline_fits text -> [ lhs ^ " = " ^ strip_outer_parens text ]
    | _ -> (lhs ^ " =") :: render_block 4 core
  in
  signature :: body_lines

let render_program (p : Ast.program) =
  let module_lines =
    match p.Ast.module_name with
    | None -> []
    | Some name -> [ "module " ^ check_name "module name" name ]
  in
  let import_lines = List.map (fun path -> "import " ^ quote_import path) p.Ast.imports in
  let export_lines =
    match p.Ast.exports with
    | None -> []
    | Some [] -> unrenderable "empty export list has no Protoss/H spelling"
    | Some names ->
        [ "export " ^ String.concat " " (List.map (check_name "export name") names) ]
  in
  let capability_lines =
    match p.Ast.capabilities with
    | [] -> []
    | caps -> [ "capabilities " ^ String.concat " " (List.map (check_name "capability") caps) ]
  in
  let alias_lines = List.map render_type_alias p.Ast.type_aliases in
  let def_blocks = List.map render_def p.Ast.defs in
  let header = module_lines @ import_lines @ export_lines @ capability_lines @ alias_lines in
  let blocks =
    (match header with [] -> [] | _ -> [ String.concat "\n" header ])
    @ List.map (String.concat "\n") def_blocks
  in
  match blocks with [] -> "" | _ -> String.concat "\n\n" blocks ^ "\n"

let human_grammar_text =
  {|protoss-human-grammar-v1

source ::= sexp_program | elm_program

sexp_program ::= declaration*
declaration ::= (module ModuleName)
              | (export Name*)
              | (import String)
              | (capabilities CapabilityName*)
              | (type Name type)
              | (type Name (TypeParam*) type)
              | (record Name record_type_params? field_type*)
              | (variant Name variant_type_params? case_type*)
              | (def Name type expr)
              | (defpoly Name (params TypeParam*) type expr)
              | (defcap Name (capabilities CapabilityName*) type expr)
              | (defpolycap Name (params TypeParam*) (capabilities CapabilityName*) type expr)
              | (defrec Name type recursion_spec recursion_body)
              | (defrecpoly Name (params TypeParam*) type recursion_spec recursion_body)

record_type_params ::= (params TypeParam*)
variant_type_params ::= (params TypeParam*)
field_type ::= (Name type)
case_type ::= (Name type)

type ::= Unit | Bool | Nat | String
       | (-> type type)
       | (Record field_type*)
       | (Variant case_type*)
       | (List type)
       | (View type)
       | (Attr type)
       | (Process type)
       | (Process (capabilities CapabilityName*) type)
       | (Cmd type)
       | (Cmd (capabilities CapabilityName*) type)
       | (SecretRef Scope type)
       | (forall Nat type)
       | (Name type*)

expr ::= Name | Nat | true | false | String | unit
       | (lambda (Name type) expr) | (lambda Name expr)
       | (let (Name expr) expr) | (let (Name type expr) expr)
       | (strict expr)
       | (record (Name expr)*)
       | (get expr Name)
       | (variant type Name expr) | (variant Name expr)
       | (case expr branch*)
       | (caseList expr (Nil expr) (Cons Name Name expr))
       | (foldNat expr expr expr)
       | (foldList expr expr expr)
       | (foldVariant type type expr branch*)
       | (recur expr)
       | (Nil type?) | (Cons type? expr expr)
       | (inst Name type*)
       | (done expr) | (bind expr expr) | request | (sendToBackend expr) | (broadcast expr)
       | (text expr) | (image expr expr) | (button expr expr)
       | (input expr) | (column expr) | (row expr) | (list expr expr)
       | (when expr expr) | (node expr expr expr) | (attr expr expr) | (on expr expr)
       | (expr expr+)

branch ::= (Name Name expr) | (Name expr) | (_ expr)
request ::= (AskHuman String)
          | (HttpGet String)
          | ReadClock
          | (SaveLocal String String)
          | (LoadLocal String)
          | (ServerRequest String String)

recursion_spec ::= (nat Name) | (list Name) | (variant Name)
recursion_body ::= (zero expr) (step Name expr)
                 | (nil expr) (cons Name Name expr)
                 | branch+

elm_program ::= elm_declaration*
elm_declaration ::= module_decl | import_decl | capabilities_decl
                  | type_alias_decl | union_decl | signature value_decl
module_decl ::= module ModuleName exposing (exposing_list)
import_decl ::= import String exposing (exposing_list)
capabilities_decl ::= capabilities CapabilityName*
signature ::= Name : elm_type
value_decl ::= Name pattern* = elm_expr
type_alias_decl ::= type alias Name type_params? = elm_type
union_decl ::= type Name type_params? = union_case+

elm_type ::= Unit | Bool | Nat | String | Name | Name elm_type+
           | elm_type -> elm_type
           | { field_type (, field_type)* }
           | [ elm_type ]
           | Process { CapabilityName* } elm_type
           | Cmd { CapabilityName* } elm_type

elm_expr ::= literal | Name | \Name+ -> elm_expr
           | if elm_expr then elm_expr else elm_expr
           | let elm_block(value_decl+) in elm_block(elm_expr)
           | case elm_expr of elm_block(elm_case+)
           | { field = elm_expr (, field = elm_expr)* }
           | { elm_expr | field = elm_expr (, field = elm_expr)* }
           | [ elm_expr (, elm_expr)* ]
           | elm_expr . Name
           | elm_expr |> elm_expr
           | elm_expr binary_op elm_expr
           | elm_expr elm_expr+

elm_case ::= pattern -> elm_expr
           | pattern -> elm_block(elm_expr)
elm_block(x) ::= NEWLINE INDENT x+ DEDENT
binary_op ::= + | == | /= | < | <= | > | >= | && | ||
exposing_list ::= .. | Name (, Name)*
Name ::= identifier
ModuleName ::= identifier(.identifier)*
CapabilityName ::= ModuleName.Name
TypeParam ::= identifier
|}
