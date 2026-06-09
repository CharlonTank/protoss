open Ast

exception Error of string

let fail msg = raise (Error msg)

let is_digit = function '0' .. '9' -> true | _ -> false

let has_line_col_prefix s =
  let len = String.length s in
  let rec digits i =
    if i < len && is_digit s.[i] then digits (i + 1) else i
  in
  let line_end = digits 0 in
  if line_end = 0 || line_end >= len || s.[line_end] <> ':' then false
  else
    let col_start = line_end + 1 in
    let col_end = digits col_start in
    col_end > col_start && col_end < len && s.[col_end] = ':'

let locate path msg =
  if has_line_col_prefix msg then path ^ ":" ^ msg else path ^ ":1:1: " ^ msg

let atom = function Sexp.Atom s -> s | x -> fail ("expected atom, got " ^ Sexp.to_string x)

let name = function
  | Sexp.Atom s when s <> "" -> s
  | x -> fail ("expected name, got " ^ Sexp.to_string x)

let int_atom s =
  try Some (int_of_string s) with Failure _ -> None

let ensure_unique what xs =
  let seen = Hashtbl.create 16 in
  List.iter
    (fun n ->
      if Hashtbl.mem seen n then fail ("duplicate " ^ what ^ ": " ^ n);
      Hashtbl.add seen n ())
    xs

let tuple_field_name index = "_" ^ string_of_int (index + 1)

let ensure_tuple_arity what xs =
  if List.length xs < 2 then fail (what ^ " requires at least two elements")

let tuple_fields xs =
  ensure_tuple_arity "tuple" xs;
  List.mapi (fun index value -> (tuple_field_name index, value)) xs

let rec sexp_atoms = function
  | Sexp.Atom s -> [ s ]
  | Sexp.Str _ -> []
  | Sexp.List xs -> List.concat_map sexp_atoms xs

let fresh_match_payload_name binders body =
  let taken =
    binders @ sexp_atoms body |> List.sort_uniq String.compare
  in
  let rec loop i =
    let candidate = "__match_payload" ^ string_of_int i in
    if List.exists (String.equal candidate) taken then loop (i + 1) else candidate
  in
  loop 0

let fresh_match_list_binders body =
  let taken = sexp_atoms body |> List.sort_uniq String.compare in
  let rec fresh prefix taken i =
    let candidate = prefix ^ string_of_int i in
    if List.exists (String.equal candidate) taken then fresh prefix taken (i + 1)
    else candidate
  in
  let head = fresh "__match_head" taken 0 in
  let tail = fresh "__match_tail" (head :: taken) 0 in
  (head, tail)

let case_list_syntax =
  "caseList syntax is (caseList xs (Nil nilExpr) (Cons head tail consExpr))"

let list_match_syntax =
  "list match syntax is (match xs (Nil nilExpr) (Cons head tail consExpr))"

let parse_list_case_branches context syntax branches =
  if branches = [] then fail (context ^ " requires at least one branch");
  let nil_branch = ref None in
  let cons_branch = ref None in
  let wildcard = ref None in
  List.iter
    (function
      | Sexp.List [ Sexp.Atom "Nil"; nil_body ] ->
          if Option.is_some !nil_branch then fail ("duplicate " ^ context ^ " Nil branch");
          nil_branch := Some nil_body
      | Sexp.List [ Sexp.Atom "Cons"; Sexp.Atom head; Sexp.Atom tail; cons_body ] ->
          if String.equal head tail then fail ("duplicate " ^ context ^ " Cons binder: " ^ head);
          if Option.is_some !cons_branch then fail ("duplicate " ^ context ^ " Cons branch");
          cons_branch := Some (head, tail, cons_body)
      | Sexp.List [ Sexp.Atom "_"; body ] ->
          if Option.is_some !wildcard then fail ("duplicate " ^ context ^ " wildcard branch");
          wildcard := Some body
      | _ -> fail syntax)
    branches;
  if Option.is_some !wildcard && Option.is_some !nil_branch && Option.is_some !cons_branch
  then fail (context ^ " wildcard branch is unreachable");
  let nil_body =
    match (!nil_branch, !wildcard) with
    | Some body, _ -> body
    | None, Some body -> body
    | None, None -> fail (context ^ " missing Nil branch")
  in
  let head, tail, cons_body =
    match (!cons_branch, !wildcard) with
    | Some branch, _ -> branch
    | None, Some body ->
        let head, tail = fresh_match_list_binders body in
        (head, tail, body)
    | None, None -> fail (context ^ " missing Cons branch")
  in
  (nil_body, head, tail, cons_body)

let rec parse_type = function
  | Sexp.Atom "Unit" -> TUnit
  | Sexp.Atom "Bool" -> TBool
  | Sexp.Atom "Nat" -> TNat
  | Sexp.Atom "String" -> TString
  | Sexp.List [ Sexp.Atom "List"; t ] -> TList (parse_type t)
  | Sexp.List [ Sexp.Atom "View"; t ] -> TView (parse_type t)
  | Sexp.List [ Sexp.Atom "Process"; t ] -> TProcess (parse_type t)
  | Sexp.List [ Sexp.Atom "TVar"; Sexp.Atom n ] -> TVar (int_of_string n)
  | Sexp.List [ Sexp.Atom "Forall"; Sexp.Atom n; t ] -> TForall (int_of_string n, parse_type t)
  | Sexp.List [ Sexp.Atom "->"; a; b ] -> TFun (parse_type a, parse_type b)
  | Sexp.List (Sexp.Atom "Tuple" :: elems) ->
      ensure_tuple_arity "Tuple type" elems;
      TRecord (tuple_fields (List.map parse_type elems))
  | Sexp.List (Sexp.Atom "Record" :: fields) ->
      let fields =
        List.map
          (function
            | Sexp.List [ Sexp.Atom n; t ] -> (n, parse_type t)
            | x -> fail ("invalid record field type: " ^ Sexp.to_string x))
          fields
      in
      ensure_unique "record field" (List.map fst fields);
      TRecord (sort_fields fields)
  | Sexp.List (Sexp.Atom "Variant" :: cases) ->
      let cases =
        List.map
          (function
            | Sexp.List [ Sexp.Atom n; t ] -> (n, parse_type t)
            | x -> fail ("invalid variant case type: " ^ Sexp.to_string x))
          cases
      in
      ensure_unique "variant constructor" (List.map fst cases);
      TVariant (sort_fields cases)
  | Sexp.List (Sexp.Atom n :: args) when n <> "" ->
      TNamed (n, List.map parse_type args)
  | Sexp.Atom s when s <> "" -> TNamed (s, [])
  | x -> fail ("invalid type: " ^ Sexp.to_string x)

let parse_binding = function
  | Sexp.List [ Sexp.Atom x; ty ] -> (x, parse_type ty)
  | x -> fail ("invalid binding: " ^ Sexp.to_string x)

let parse_lambda_binding = function
  | Sexp.List [ Sexp.Atom x; ty ] -> `Annotated (x, parse_type ty)
  | Sexp.List [ Sexp.Atom x ] | Sexp.Atom x -> `Inferred x
  | x -> fail ("invalid lambda binding: " ^ Sexp.to_string x)

let parse_record_destructure_bindings = function
  | Sexp.List bindings ->
      let fields =
        List.map
          (function
            | Sexp.Atom n -> (n, n)
            | Sexp.List [ Sexp.Atom field; Sexp.Atom binder ] -> (field, binder)
            | x -> fail ("invalid letRecord field binding: " ^ Sexp.to_string x))
          bindings
      in
      if fields = [] then fail "letRecord requires at least one field";
      ensure_unique "letRecord field" (List.map fst fields);
      ensure_unique "letRecord binder" (List.map snd fields);
      sort_fields fields
  | x -> fail ("letRecord fields must be a list: " ^ Sexp.to_string x)

let parse_named_type_params = function
  | Sexp.List (Sexp.Atom "params" :: params) :: rest -> (List.map atom params, rest)
  | rest -> ([], rest)

let parse_named_type_fields what fields =
  let fields =
    List.map
      (function
        | Sexp.List [ Sexp.Atom n; t ] -> (n, parse_type t)
        | x -> fail ("invalid " ^ what ^ " field type: " ^ Sexp.to_string x))
      fields
  in
  ensure_unique what (List.map fst fields);
  sort_fields fields

let parse_capability_clause = function
  | Sexp.List (Sexp.Atom "capabilities" :: caps) ->
      let caps = List.map atom caps in
      ensure_unique "definition capability" caps;
      List.sort String.compare caps
  | x -> fail ("invalid definition capabilities: " ^ Sexp.to_string x)

let rec parse_expr = function
  | Sexp.Atom "unit" -> EUnit
  | Sexp.Atom "true" -> EBool true
  | Sexp.Atom "false" -> EBool false
  | Sexp.Atom "Nil" -> ENilInfer
  | Sexp.Atom s -> (
      match int_atom s with Some n when n >= 0 -> ENat n | _ -> EName s)
  | Sexp.Str s -> EString s
  | Sexp.List [ Sexp.Atom "lambda"; binding; body ] ->
      (match parse_lambda_binding binding with
      | `Annotated (x, ty) -> ELambda (x, ty, parse_expr body)
      | `Inferred x -> ELambdaInfer (x, parse_expr body))
  | Sexp.List [ Sexp.Atom "let"; Sexp.List [ Sexp.Atom x; e ]; body ] ->
      ELet (x, parse_expr e, parse_expr body)
  | Sexp.List [ Sexp.Atom "let"; Sexp.List [ Sexp.Atom x; ty; e ]; body ] ->
      ELetAnnot (x, parse_type ty, parse_expr e, parse_expr body)
  | Sexp.List [ Sexp.Atom "letRecord"; record; fields; body ] ->
      ELetRecord (parse_expr record, parse_record_destructure_bindings fields, parse_expr body)
  | Sexp.List (Sexp.Atom "letRecord" :: _) ->
      fail "letRecord syntax is (letRecord recordExpr (field (source binder) ...) body)"
  | Sexp.List (Sexp.Atom "record" :: fields) ->
      let fields =
        List.map
          (function
            | Sexp.List [ Sexp.Atom n; e ] -> (n, parse_expr e)
            | x -> fail ("invalid record field: " ^ Sexp.to_string x))
          fields
      in
      ensure_unique "record field" (List.map fst fields);
      ERecord (sort_fields fields)
  | Sexp.List (Sexp.Atom "tuple" :: elems) ->
      ERecord (sort_fields (tuple_fields (List.map parse_expr elems)))
  | Sexp.List [ Sexp.Atom "get"; e; Sexp.Atom field ] -> EField (parse_expr e, field)
  | Sexp.List [ Sexp.Atom "variant"; ty; Sexp.Atom con; e ] ->
      EVariant (parse_type ty, con, parse_expr e)
  | Sexp.List [ Sexp.Atom "variant"; Sexp.Atom con; e ] ->
      EVariantInferred (con, parse_expr e)
  | Sexp.List (Sexp.Atom "inst" :: Sexp.Atom n :: args) ->
      EInst (n, List.map parse_type args)
  | Sexp.List (Sexp.Atom "case" :: scrut :: branches) ->
      ECase (parse_expr scrut, List.map parse_branch branches)
  | Sexp.List (Sexp.Atom "match" :: scrut :: branches) ->
      parse_match_expr scrut branches
  | Sexp.List [ Sexp.Atom "foldNat"; n; z; step ] ->
      EFoldNat (parse_expr n, parse_expr z, parse_expr step)
  | Sexp.List (Sexp.Atom "foldVariant" :: target :: result :: scrut :: branches) ->
      if branches = [] then fail "foldVariant requires at least one branch";
      EFoldVariant
        ( parse_type target,
          parse_type result,
          parse_expr scrut,
          List.map parse_branch branches )
  | Sexp.List [ Sexp.Atom "recur"; e ] -> ERecur (parse_expr e)
  | Sexp.List [ Sexp.Atom "Nil"; ty ] -> ENil (parse_type ty)
  | Sexp.List [ Sexp.Atom "Nil" ] -> ENilInfer
  | Sexp.List [ Sexp.Atom "Cons"; ty; head; tail ] ->
      ECons (parse_type ty, parse_expr head, parse_expr tail)
  | Sexp.List [ Sexp.Atom "Cons"; head; tail ] -> EConsInfer (parse_expr head, parse_expr tail)
  | Sexp.List [ Sexp.Atom "foldList"; xs; z; step ] ->
      EFoldList (parse_expr xs, parse_expr z, parse_expr step)
  | Sexp.List (Sexp.Atom "caseList" :: xs :: branches) ->
      let nil_body, head, tail, cons_body =
        parse_list_case_branches "caseList" case_list_syntax branches
      in
      ECaseList (parse_expr xs, parse_expr nil_body, head, tail, parse_expr cons_body)
  | Sexp.List (Sexp.Atom "caseList" :: _) ->
      fail case_list_syntax
  | Sexp.List [ Sexp.Atom "text"; e ] -> EText (parse_expr e)
  | Sexp.List [ Sexp.Atom "image"; src; alt ] ->
      EImage (parse_expr src, parse_expr alt)
  | Sexp.List [ Sexp.Atom "button"; label; msg ] ->
      EButton (parse_expr label, parse_expr msg)
  | Sexp.List [ Sexp.Atom "input"; value; handler ] ->
      EInput (parse_expr value, parse_expr handler)
  | Sexp.List [ Sexp.Atom "column"; children ] -> EColumn (parse_expr children)
  | Sexp.List [ Sexp.Atom "row"; children ] -> ERow (parse_expr children)
  | Sexp.List [ Sexp.Atom "list"; items; render ] ->
      EListView (parse_expr items, parse_expr render)
  | Sexp.List [ Sexp.Atom "when"; cond; view ] ->
      EWhenView (parse_expr cond, parse_expr view)
  | Sexp.List [ Sexp.Atom "done"; e ] -> EDone (parse_expr e)
  | Sexp.List [ Sexp.Atom "Human.ask"; Sexp.Str prompt ] ->
      ERequest (AskHuman prompt)
  | Sexp.List [ Sexp.Atom "Http.get"; Sexp.Str url ] -> ERequest (HttpGet url)
  | Sexp.List [ Sexp.Atom "Clock.read" ] -> ERequest ReadClock
  | Sexp.List [ Sexp.Atom "Local.save"; Sexp.Str key; Sexp.Str value ] ->
      ERequest (SaveLocal (key, value))
  | Sexp.List [ Sexp.Atom "Local.load"; Sexp.Str key ] -> ERequest (LoadLocal key)
  | Sexp.List [ Sexp.Atom "Server.request"; Sexp.Str route; Sexp.Str payload ] ->
      ERequest (ServerRequest (route, payload))
  | Sexp.List [ Sexp.Atom "bind"; p; Sexp.List [ Sexp.Atom "lambda"; binding; body ] ]
    ->
      (match parse_lambda_binding binding with
      | `Annotated (x, ty) -> EBind (parse_expr p, x, ty, parse_expr body)
      | `Inferred x -> EBindInfer (parse_expr p, x, parse_expr body))
  | Sexp.List [] -> fail "empty expression list"
  | Sexp.List (f :: args) ->
      List.fold_left (fun acc arg -> EApp (acc, parse_expr arg)) (parse_expr f) args

and parse_match_expr scrut branches =
  match parse_match_tuple scrut branches with
  | Some expr -> expr
  | None -> (
      match parse_match_record scrut branches with
      | Some expr -> expr
      | None -> (
          match parse_match_list scrut branches with
          | Some expr -> expr
          | None -> ECase (parse_expr scrut, List.map parse_branch branches)))

and parse_match_tuple scrut = function
  | [ Sexp.List [ Sexp.List (Sexp.Atom "tuple" :: binders); body ] ] ->
      ensure_tuple_arity "tuple match" binders;
      let binders = List.map name binders in
      ensure_unique "tuple match binder" binders;
      Some
        (ELetRecord
           ( parse_expr scrut,
             tuple_fields binders,
             parse_expr body ))
  | branches
    when List.exists
           (function
             | Sexp.List (Sexp.List (Sexp.Atom "tuple" :: _) :: _) -> true
             | _ -> false)
           branches ->
      fail "tuple match syntax is (match tupleExpr ((tuple a b ...) body))"
  | _ -> None

and parse_match_record scrut = function
  | [ Sexp.List [ Sexp.List (Sexp.Atom "record" :: fields); body ] ] ->
      Some
        (ELetRecord
           ( parse_expr scrut,
             parse_record_destructure_bindings (Sexp.List fields),
             parse_expr body ))
  | branches
    when List.exists
           (function
             | Sexp.List (Sexp.List (Sexp.Atom "record" :: _) :: _) -> true
             | _ -> false)
           branches ->
      fail "record match syntax is (match recordExpr ((record field (source binder) ...) body))"
  | _ -> None

and parse_match_list scrut branches =
  let is_list_branch = function
    | Sexp.List (Sexp.Atom "Nil" :: _) | Sexp.List (Sexp.Atom "Cons" :: _) -> true
    | _ -> false
  in
  if not (List.exists is_list_branch branches) then None
  else
    let nil_body, head, tail, cons_body =
      parse_list_case_branches "match" list_match_syntax branches
    in
    Some (ECaseList (parse_expr scrut, parse_expr nil_body, head, tail, parse_expr cons_body))

and parse_branch = function
  | Sexp.List [ Sexp.Atom "_"; e ] -> BWildcard (parse_expr e)
  | Sexp.List [ Sexp.Atom "true"; e ] -> BBool (true, parse_expr e)
  | Sexp.List [ Sexp.Atom "false"; e ] -> BBool (false, parse_expr e)
  | Sexp.List [ Sexp.Atom con; Sexp.List (Sexp.Atom "record" :: fields); body ] ->
      let fields = parse_record_destructure_bindings (Sexp.List fields) in
      let payload = fresh_match_payload_name (List.map snd fields) body in
      BVariant (con, payload, ELetRecord (EName payload, fields, parse_expr body))
  | Sexp.List [ Sexp.Atom con; Sexp.List (Sexp.Atom "tuple" :: binders); body ] ->
      ensure_tuple_arity "tuple match" binders;
      let binders = List.map name binders in
      ensure_unique "tuple match binder" binders;
      let payload = fresh_match_payload_name binders body in
      BVariant (con, payload, ELetRecord (EName payload, tuple_fields binders, parse_expr body))
  | Sexp.List [ Sexp.Atom con; Sexp.Atom x; e ] -> BVariant (con, x, parse_expr e)
  | Sexp.List [ Sexp.Atom con; e ] -> BVariantUnit (con, parse_expr e)
  | x -> fail ("invalid case branch: " ^ Sexp.to_string x)

let defrec_error keyword name =
  fail
    (keyword ^ " " ^ name
   ^ " must be structural Nat, List, or Variant recursion: \
      (defrec name (-> Nat R) (nat n) (zero z) (step acc body)) or \
      (defrec name (-> (List A) R) (list xs) (nil z) (cons x acc body)) or \
      (defrec name (-> VariantType R) (variant x) (Ctor payload body) ...)")

let parse_structural_defrec keyword name type_params typ clauses =
  match (typ, clauses) with
  | ( TFun (TNat, result_ty),
      [
        Sexp.List [ Sexp.Atom "nat"; Sexp.Atom param ];
        Sexp.List [ Sexp.Atom "zero"; zero ];
        Sexp.List [ Sexp.Atom "step"; Sexp.Atom acc; step ];
      ] ) ->
      {
        name;
        type_params;
        declared_capabilities = None;
        typ;
        body =
          ELambda
            ( param,
              TNat,
              EFoldNat (EName param, parse_expr zero, ELambda (acc, result_ty, parse_expr step)) );
      }
  | ( TFun (TList item_ty, result_ty),
      [
        Sexp.List [ Sexp.Atom "list"; Sexp.Atom param ];
        Sexp.List [ Sexp.Atom "nil"; nil ];
        Sexp.List [ Sexp.Atom "cons"; Sexp.Atom item; Sexp.Atom acc; step ];
      ] ) ->
      {
        name;
        type_params;
        declared_capabilities = None;
        typ;
        body =
          ELambda
            ( param,
              TList item_ty,
              EFoldList
                ( EName param,
                  parse_expr nil,
                  ELambda (item, item_ty, ELambda (acc, result_ty, parse_expr step)) ) );
      }
  | TFun (target_ty, result_ty), Sexp.List [ Sexp.Atom "variant"; Sexp.Atom param ] :: branches
    when branches <> [] ->
      {
        name;
        type_params;
        declared_capabilities = None;
        typ;
        body =
          ELambda
            ( param,
              target_ty,
              EFoldVariant (target_ty, result_ty, EName param, List.map parse_branch branches) );
      }
  | _ -> defrec_error keyword name

let has_dot s = String.contains s '.'

let qualify module_name name =
  match module_name with
  | Some m when not (has_dot name) -> m ^ "." ^ name
  | _ -> name

let assoc_opt name xs =
  List.find_opt (fun (n, _) -> String.equal n name) xs |> Option.map snd

let rec qualify_type local_types params = function
  | TUnit -> TUnit
  | TBool -> TBool
  | TNat -> TNat
  | TString -> TString
  | TFun (a, b) -> TFun (qualify_type local_types params a, qualify_type local_types params b)
  | TRecord fields ->
      TRecord (sort_fields (List.map (fun (n, t) -> (n, qualify_type local_types params t)) fields))
  | TVariant cases ->
      TVariant (sort_fields (List.map (fun (n, t) -> (n, qualify_type local_types params t)) cases))
  | TList t -> TList (qualify_type local_types params t)
  | TView t -> TView (qualify_type local_types params t)
  | TProcess t -> TProcess (qualify_type local_types params t)
  | TVar i -> TVar i
  | TForall (arity, body) -> TForall (arity, qualify_type local_types params body)
  | TNamed (n, args) ->
      let args = List.map (qualify_type local_types params) args in
      if List.exists (String.equal n) params then TNamed (n, args)
      else (
        match assoc_opt n local_types with
        | Some q -> TNamed (q, args)
        | None -> TNamed (n, args))

let rec qualify_expr local_defs local_types type_params bound = function
  | EUnit -> EUnit
  | EBool b -> EBool b
  | ENat n -> ENat n
  | EString s -> EString s
  | EName n ->
      if List.exists (String.equal n) bound then EName n
      else (
        match assoc_opt n local_defs with Some q -> EName q | None -> EName n)
  | ELambda (x, t, body) ->
      ELambda
        ( x,
          qualify_type local_types type_params t,
          qualify_expr local_defs local_types type_params (x :: bound) body )
  | ELambdaInfer (x, body) ->
      ELambdaInfer (x, qualify_expr local_defs local_types type_params (x :: bound) body)
  | EApp (f, x) ->
      EApp
        ( qualify_expr local_defs local_types type_params bound f,
          qualify_expr local_defs local_types type_params bound x )
  | ELet (x, e, body) ->
      ELet
        ( x,
          qualify_expr local_defs local_types type_params bound e,
          qualify_expr local_defs local_types type_params (x :: bound) body )
  | ELetAnnot (x, t, e, body) ->
      ELetAnnot
        ( x,
          qualify_type local_types type_params t,
          qualify_expr local_defs local_types type_params bound e,
          qualify_expr local_defs local_types type_params (x :: bound) body )
  | ELetRecord (record, fields, body) ->
      let binders = List.map snd fields in
      ELetRecord
        ( qualify_expr local_defs local_types type_params bound record,
          fields,
          qualify_expr local_defs local_types type_params (binders @ bound) body )
  | ERecord fields ->
      ERecord
        (sort_fields
           (List.map
              (fun (n, e) -> (n, qualify_expr local_defs local_types type_params bound e))
              fields))
  | EField (e, field) -> EField (qualify_expr local_defs local_types type_params bound e, field)
  | EVariant (t, con, e) ->
      EVariant
        ( qualify_type local_types type_params t,
          con,
          qualify_expr local_defs local_types type_params bound e )
  | EVariantInferred (con, e) ->
      EVariantInferred (con, qualify_expr local_defs local_types type_params bound e)
  | EInst (n, args) ->
      let n =
        if List.exists (String.equal n) bound then n
        else match assoc_opt n local_defs with Some q -> q | None -> n
      in
      EInst (n, List.map (qualify_type local_types type_params) args)
  | ECase (e, branches) ->
      ECase
        ( qualify_expr local_defs local_types type_params bound e,
          List.map (qualify_branch local_defs local_types type_params bound) branches )
  | EFoldNat (n, z, step) ->
      EFoldNat
        ( qualify_expr local_defs local_types type_params bound n,
          qualify_expr local_defs local_types type_params bound z,
          qualify_expr local_defs local_types type_params bound step )
  | EFoldVariant (target, result, scrut, branches) ->
      EFoldVariant
        ( qualify_type local_types type_params target,
          qualify_type local_types type_params result,
          qualify_expr local_defs local_types type_params bound scrut,
          List.map (qualify_branch local_defs local_types type_params bound) branches )
  | ERecur e -> ERecur (qualify_expr local_defs local_types type_params bound e)
  | ENil t -> ENil (qualify_type local_types type_params t)
  | ENilInfer -> ENilInfer
  | ECons (t, head, tail) ->
      ECons
        ( qualify_type local_types type_params t,
          qualify_expr local_defs local_types type_params bound head,
          qualify_expr local_defs local_types type_params bound tail )
  | EConsInfer (head, tail) ->
      EConsInfer
        ( qualify_expr local_defs local_types type_params bound head,
          qualify_expr local_defs local_types type_params bound tail )
  | EFoldList (xs, z, step) ->
      EFoldList
        ( qualify_expr local_defs local_types type_params bound xs,
          qualify_expr local_defs local_types type_params bound z,
          qualify_expr local_defs local_types type_params bound step )
  | ECaseList (xs, nil_body, head, tail, cons_body) ->
      ECaseList
        ( qualify_expr local_defs local_types type_params bound xs,
          qualify_expr local_defs local_types type_params bound nil_body,
          head,
          tail,
          qualify_expr local_defs local_types type_params (head :: tail :: bound) cons_body )
  | EText e -> EText (qualify_expr local_defs local_types type_params bound e)
  | EImage (src, alt) ->
      EImage
        ( qualify_expr local_defs local_types type_params bound src,
          qualify_expr local_defs local_types type_params bound alt )
  | EButton (label, msg) ->
      EButton
        ( qualify_expr local_defs local_types type_params bound label,
          qualify_expr local_defs local_types type_params bound msg )
  | EInput (value, handler) ->
      EInput
        ( qualify_expr local_defs local_types type_params bound value,
          qualify_expr local_defs local_types type_params bound handler )
  | EColumn children -> EColumn (qualify_expr local_defs local_types type_params bound children)
  | ERow children -> ERow (qualify_expr local_defs local_types type_params bound children)
  | EListView (items, render) ->
      EListView
        ( qualify_expr local_defs local_types type_params bound items,
          qualify_expr local_defs local_types type_params bound render )
  | EWhenView (cond, view) ->
      EWhenView
        ( qualify_expr local_defs local_types type_params bound cond,
          qualify_expr local_defs local_types type_params bound view )
  | EDone e -> EDone (qualify_expr local_defs local_types type_params bound e)
  | ERequest req -> ERequest req
  | EBind (p, x, t, body) ->
      EBind
        ( qualify_expr local_defs local_types type_params bound p,
          x,
          qualify_type local_types type_params t,
          qualify_expr local_defs local_types type_params (x :: bound) body )
  | EBindInfer (p, x, body) ->
      EBindInfer
        ( qualify_expr local_defs local_types type_params bound p,
          x,
          qualify_expr local_defs local_types type_params (x :: bound) body )

and qualify_branch local_defs local_types type_params bound = function
  | BBool (b, e) -> BBool (b, qualify_expr local_defs local_types type_params bound e)
  | BVariant (con, x, e) ->
      BVariant (con, x, qualify_expr local_defs local_types type_params (x :: bound) e)
  | BVariantUnit (con, e) ->
      BVariantUnit (con, qualify_expr local_defs local_types type_params bound e)
  | BWildcard e -> BWildcard (qualify_expr local_defs local_types type_params bound e)

let qualify_program module_name exports aliases defs =
  let local_defs = List.map (fun d -> (d.name, qualify module_name d.name)) defs in
  let local_types = List.map (fun a -> (a.type_name, qualify module_name a.type_name)) aliases in
  let aliases =
    List.map
      (fun a ->
        {
          type_name = qualify module_name a.type_name;
          type_params = a.type_params;
          type_body = qualify_type local_types a.type_params a.type_body;
        })
      aliases
  in
  let defs =
    List.map
      (fun d ->
        {
          name = qualify module_name d.name;
          type_params = d.type_params;
          declared_capabilities = d.declared_capabilities;
          typ = qualify_type local_types d.type_params d.typ;
          body = qualify_expr local_defs local_types d.type_params [] d.body;
        })
      defs
  in
  let local_symbols = List.map snd local_defs @ List.map snd local_types in
  let exports =
    Option.map
      (fun names ->
        let names = List.map (qualify module_name) names in
        ensure_unique "export" names;
        List.iter
          (fun name ->
            if not (List.exists (String.equal name) local_symbols) then
              fail ("exported symbol is not defined in module: " ^ name))
          names;
        names)
      exports
  in
  (aliases, defs, exports)

let parse_toplevel = function
  | Sexp.List [ Sexp.Atom "module"; Sexp.Atom name ] -> `Module name
  | Sexp.List (Sexp.Atom "export" :: names) -> `Export (List.map atom names)
  | Sexp.List [ Sexp.Atom "import"; Sexp.Str path ] -> `Import path
  | Sexp.List (Sexp.Atom "capabilities" :: caps) ->
      `Capabilities (List.map atom caps)
  | Sexp.List [ Sexp.Atom "type"; Sexp.Atom n; ty ]
  | Sexp.List [ Sexp.Atom "alias"; Sexp.Atom n; ty ] ->
      `TypeAlias { type_name = n; type_params = []; type_body = parse_type ty }
  | Sexp.List [ Sexp.Atom "type"; Sexp.Atom n; Sexp.List params; ty ]
  | Sexp.List [ Sexp.Atom "alias"; Sexp.Atom n; Sexp.List params; ty ] ->
      `TypeAlias { type_name = n; type_params = List.map atom params; type_body = parse_type ty }
  | Sexp.List (Sexp.Atom "record" :: Sexp.Atom n :: fields) ->
      let params, fields = parse_named_type_params fields in
      `TypeAlias
        { type_name = n; type_params = params; type_body = TRecord (parse_named_type_fields "record field" fields) }
  | Sexp.List (Sexp.Atom "variant" :: Sexp.Atom n :: cases) ->
      let params, cases = parse_named_type_params cases in
      `TypeAlias
        {
          type_name = n;
          type_params = params;
          type_body = TVariant (parse_named_type_fields "variant constructor" cases);
        }
  | Sexp.List [ Sexp.Atom "def"; Sexp.Atom n; ty; body ] ->
      `Def
        {
          name = n;
          type_params = [];
          declared_capabilities = None;
          typ = parse_type ty;
          body = parse_expr body;
        }
  | Sexp.List [ Sexp.Atom "defcap"; Sexp.Atom n; caps; ty; body ] ->
      `Def
        {
          name = n;
          type_params = [];
          declared_capabilities = Some (parse_capability_clause caps);
          typ = parse_type ty;
          body = parse_expr body;
        }
  | Sexp.List [ Sexp.Atom "defpoly"; Sexp.Atom n; Sexp.List (Sexp.Atom "params" :: params); ty; body ] ->
      let type_params = List.map atom params in
      ensure_unique "type parameter" type_params;
      if type_params = [] then fail ("defpoly requires at least one type parameter: " ^ n);
      `Def
        {
          name = n;
          type_params;
          declared_capabilities = None;
          typ = parse_type ty;
          body = parse_expr body;
        }
  | Sexp.List
      [
        Sexp.Atom "defpolycap";
        Sexp.Atom n;
        Sexp.List (Sexp.Atom "params" :: params);
        caps;
        ty;
        body;
      ] ->
      let type_params = List.map atom params in
      ensure_unique "type parameter" type_params;
      if type_params = [] then fail ("defpolycap requires at least one type parameter: " ^ n);
      `Def
        {
          name = n;
          type_params;
          declared_capabilities = Some (parse_capability_clause caps);
          typ = parse_type ty;
          body = parse_expr body;
        }
  | Sexp.List (Sexp.Atom "defrec" :: Sexp.Atom n :: ty :: clauses) ->
      `Def (parse_structural_defrec "defrec" n [] (parse_type ty) clauses)
  | Sexp.List (Sexp.Atom "defrec" :: _) -> fail "invalid defrec form"
  | Sexp.List
      (Sexp.Atom "defrecpoly" :: Sexp.Atom n :: Sexp.List (Sexp.Atom "params" :: params)
       :: ty :: clauses) ->
      let type_params = List.map atom params in
      ensure_unique "type parameter" type_params;
      if type_params = [] then fail ("defrecpoly requires at least one type parameter: " ^ n);
      `Def (parse_structural_defrec "defrecpoly" n type_params (parse_type ty) clauses)
  | Sexp.List (Sexp.Atom "defrecpoly" :: _) -> fail "invalid defrecpoly form"
  | x -> fail ("invalid top-level form: " ^ Sexp.to_string x)

let parse_sexp_string input =
  let forms =
    try Sexp.parse input with Sexp.Error msg -> fail msg
  in
  let module_name = ref None and exports = ref None in
  let imports = ref [] and caps = ref [] and aliases = ref [] and defs = ref [] in
  List.iter
    (fun form ->
      match parse_toplevel form with
      | `Module name ->
          if !module_name <> None then fail "duplicate module declaration";
          module_name := Some name
      | `Export names ->
          if !exports <> None then fail "duplicate export declaration";
          exports := Some names
      | `Import path -> imports := path :: !imports
      | `Capabilities xs -> caps := xs @ !caps
      | `TypeAlias a -> aliases := a :: !aliases
      | `Def d -> defs := d :: !defs)
    forms;
  ensure_unique "type alias" (List.map (fun a -> a.type_name) !aliases);
  ensure_unique "definition" (List.map (fun d -> d.name) !defs);
  let type_aliases, defs, exports =
    qualify_program !module_name !exports (List.rev !aliases) (List.rev !defs)
  in
  {
    imports = List.rev !imports;
    capabilities = List.sort_uniq String.compare !caps;
    module_name = !module_name;
    exports;
    type_aliases;
    defs;
  }

let parse_string input =
  if Elm_syntax.looks_like input then
    let converted =
      try Elm_syntax.to_sexp_source input with Elm_syntax.Error msg -> fail msg
    in
    parse_sexp_string converted
  else parse_sexp_string input

let parse_file path =
  let ic = open_in path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () ->
      let len = in_channel_length ic in
      let input = really_input_string ic len in
      try parse_string input with Error msg -> fail (locate path msg))
