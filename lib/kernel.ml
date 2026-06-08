open Ast

exception Error = Kernel_error.Error

let fail = Kernel_error.fail

let hash_string s = "p1:" ^ Hashcons.digest s

let builtin_types =
  [
    ("succ", TFun (TNat, TNat));
    ("prim.Nat.eq", TFun (TNat, TFun (TNat, TBool)));
    ("prim.String.concat", TFun (TString, TFun (TString, TString)));
    ("prim.String.eq", TFun (TString, TFun (TString, TBool)));
  ]

let builtin_names = List.map fst builtin_types

let is_builtin n = List.exists (String.equal n) builtin_names

let assoc_opt k xs = List.find_opt (fun (k', _) -> String.equal k k') xs |> Option.map snd

let require cond msg = if not cond then fail msg

let option_or_fail msg = function Some x -> x | None -> fail msg

let rec type_to_canonical = function
  | TUnit -> "Unit"
  | TBool -> "Bool"
  | TNat -> "Nat"
  | TString -> "String"
  | TFun (a, b) -> "(Fun " ^ type_to_canonical a ^ " " ^ type_to_canonical b ^ ")"
  | TRecord fields ->
      "(Record "
      ^ String.concat " "
          (List.map
             (fun (n, t) -> "(" ^ n ^ " " ^ type_to_canonical t ^ ")")
             (sort_fields fields))
      ^ ")"
  | TVariant cases ->
      "(Variant "
      ^ String.concat " "
          (List.map
             (fun (n, t) -> "(" ^ n ^ " " ^ type_to_canonical t ^ ")")
             (sort_fields cases))
      ^ ")"
  | TList t -> "(List " ^ type_to_canonical t ^ ")"
  | TView t -> "(View " ^ type_to_canonical t ^ ")"
  | TProcess t -> "(Process " ^ type_to_canonical t ^ ")"
  | TVar i -> "(TVar " ^ string_of_int i ^ ")"
  | TForall (arity, body) ->
      "(Forall " ^ string_of_int arity ^ " " ^ type_to_canonical body ^ ")"
  | TNamed (n, _) -> fail ("unresolved type alias in canonical type: " ^ n)

let req_capability = function
  | AskHuman _ -> "Human.ask"
  | HttpGet _ -> "Http.get"
  | ReadClock -> "Clock.read"
  | SaveLocal _ -> "Local.storage"
  | LoadLocal _ -> "Local.storage"
  | ServerRequest _ -> "Server.request"

let req_result_type = function
  | AskHuman _ | HttpGet _ | ReadClock | LoadLocal _ | ServerRequest _ -> TString
  | SaveLocal _ -> TUnit

type capability_request_signature = {
  request_tag : string;
  request_payload_type : typ;
  response_type : typ;
}

type capability_descriptor = {
  capability_name : string;
  request_signatures : capability_request_signature list;
}

let capability_catalog =
  [
    {
      capability_name = "Clock.read";
      request_signatures =
        [ { request_tag = "ReadClock"; request_payload_type = TUnit; response_type = TString } ];
    };
    {
      capability_name = "Http.get";
      request_signatures =
        [
          {
            request_tag = "HttpGet";
            request_payload_type = TRecord [ ("url", TString) ];
            response_type = TString;
          };
        ];
    };
    {
      capability_name = "Human.ask";
      request_signatures =
        [
          {
            request_tag = "AskHuman";
            request_payload_type = TRecord [ ("prompt", TString) ];
            response_type = TString;
          };
        ];
    };
    {
      capability_name = "Local.storage";
      request_signatures =
        [
          {
            request_tag = "LoadLocal";
            request_payload_type = TRecord [ ("key", TString) ];
            response_type = TString;
          };
          {
            request_tag = "SaveLocal";
            request_payload_type = TRecord [ ("key", TString); ("value", TString) ];
            response_type = TUnit;
          };
        ];
    };
    {
      capability_name = "Server.request";
      request_signatures =
        [
          {
            request_tag = "ServerRequest";
            request_payload_type = TRecord [ ("payload", TString); ("route", TString) ];
            response_type = TString;
          };
        ];
    };
  ]

let capability_descriptor name =
  List.find_opt (fun d -> String.equal d.capability_name name) capability_catalog

let known_capabilities () =
  capability_catalog |> List.map (fun d -> d.capability_name) |> List.sort String.compare

let validate_capabilities caps =
  let unknown =
    caps
    |> List.filter (fun cap -> capability_descriptor cap = None)
    |> List.sort_uniq String.compare
  in
  match unknown with
  | [] -> ()
  | [ cap ] ->
      fail
        ("unknown capability: " ^ cap ^ ". Known capabilities: "
        ^ String.concat ", " (known_capabilities ()))
  | caps ->
      fail
        ("unknown capabilities: " ^ String.concat ", " caps ^ ". Known capabilities: "
        ^ String.concat ", " (known_capabilities ()))

let req_to_canonical = function
  | AskHuman prompt -> "(AskHuman " ^ Ast.quote prompt ^ ")"
  | HttpGet url -> "(HttpGet " ^ Ast.quote url ^ ")"
  | ReadClock -> "ReadClock"
  | SaveLocal (key, value) -> "(SaveLocal " ^ Ast.quote key ^ " " ^ Ast.quote value ^ ")"
  | LoadLocal key -> "(LoadLocal " ^ Ast.quote key ^ ")"
  | ServerRequest (route, payload) ->
      "(ServerRequest " ^ Ast.quote route ^ " " ^ Ast.quote payload ^ ")"

let builtin_type_names =
  [ "Unit"; "Bool"; "Nat"; "String"; "List"; "View"; "Process"; "Record"; "Variant"; "->"; "Fun" ]

let check_duplicate_names defs =
  let seen = Hashtbl.create 32 in
  List.iter
    (fun (d : def) ->
      if is_builtin d.name then fail ("definition shadows builtin: " ^ d.name);
      if Hashtbl.mem seen d.name then fail ("duplicate definition: " ^ d.name);
      let params_seen = Hashtbl.create 8 in
      List.iter
        (fun param ->
          if List.exists (String.equal param) builtin_type_names then
            fail ("type parameter shadows builtin type: " ^ param);
          if Hashtbl.mem params_seen param then
            fail ("duplicate type parameter " ^ param ^ " in definition " ^ d.name);
          Hashtbl.add params_seen param ())
        d.type_params;
      Hashtbl.add seen d.name ())
    defs

let check_duplicate_type_aliases aliases =
  let seen = Hashtbl.create 32 in
  List.iter
    (fun a ->
      if List.exists (String.equal a.type_name) builtin_type_names then
        fail ("type alias shadows builtin type: " ^ a.type_name);
      let params_seen = Hashtbl.create 8 in
      List.iter
        (fun param ->
          if List.exists (String.equal param) builtin_type_names then
            fail ("type parameter shadows builtin type: " ^ param);
          if Hashtbl.mem params_seen param then
            fail ("duplicate type parameter " ^ param ^ " in alias " ^ a.type_name);
          Hashtbl.add params_seen param ())
        a.type_params;
      if Hashtbl.mem seen a.type_name then fail ("duplicate type alias: " ^ a.type_name);
      Hashtbl.add seen a.type_name ())
    aliases

let alias_by_name aliases name =
  List.find_opt (fun a -> String.equal a.type_name name) aliases

let bind_type_params alias args =
  let expected = List.length alias.type_params and actual = List.length args in
  if expected <> actual then
    fail
      ("type alias " ^ alias.type_name ^ " expects " ^ string_of_int expected
     ^ " argument(s), got " ^ string_of_int actual);
  List.combine alias.type_params args

let rec expand_type aliases vars stack = function
  | TUnit -> TUnit
  | TBool -> TBool
  | TNat -> TNat
  | TString -> TString
  | TFun (a, b) -> TFun (expand_type aliases vars stack a, expand_type aliases vars stack b)
  | TRecord fields ->
      TRecord (sort_fields (List.map (fun (n, t) -> (n, expand_type aliases vars stack t)) fields))
  | TVariant cases ->
      TVariant (sort_fields (List.map (fun (n, t) -> (n, expand_type aliases vars stack t)) cases))
  | TList t -> TList (expand_type aliases vars stack t)
  | TView t -> TView (expand_type aliases vars stack t)
  | TProcess t -> TProcess (expand_type aliases vars stack t)
  | TVar i -> TVar i
  | TForall (arity, body) -> TForall (arity, expand_type aliases vars stack body)
  | TNamed (n, args) -> (
      let args = List.map (expand_type aliases vars stack) args in
      match assoc_opt n vars with
      | Some t ->
          if args <> [] then fail ("type parameter is not a type constructor: " ^ n);
          t
      | None ->
      if List.exists (String.equal n) stack then
        fail ("cyclic type alias: " ^ String.concat " -> " (List.rev (n :: stack)));
      match alias_by_name aliases n with
      | Some alias ->
          let vars = bind_type_params alias args in
          expand_type aliases vars (n :: stack) alias.type_body
      | None ->
          if List.exists (String.equal n) builtin_type_names then
            fail ("invalid builtin type application: " ^ Ast.string_of_typ (TNamed (n, args)))
          else fail ("unknown type alias: " ^ n))

let type_var_env params =
  List.mapi (fun i param -> (param, TVar i)) params

let rec expand_expr_types aliases vars = function
  | EUnit -> EUnit
  | EBool b -> EBool b
  | ENat n -> ENat n
  | EString s -> EString s
  | EName n -> EName n
  | ELambda (x, t, body) ->
      ELambda (x, expand_type aliases vars [] t, expand_expr_types aliases vars body)
  | EApp (f, x) -> EApp (expand_expr_types aliases vars f, expand_expr_types aliases vars x)
  | ELet (x, e, body) ->
      ELet (x, expand_expr_types aliases vars e, expand_expr_types aliases vars body)
  | ERecord fields ->
      ERecord
        (sort_fields (List.map (fun (n, e) -> (n, expand_expr_types aliases vars e)) fields))
  | EField (e, field) -> EField (expand_expr_types aliases vars e, field)
  | EVariant (t, con, e) ->
      EVariant (expand_type aliases vars [] t, con, expand_expr_types aliases vars e)
  | EVariantInferred (con, e) -> EVariantInferred (con, expand_expr_types aliases vars e)
  | EInst (name, args) -> EInst (name, List.map (expand_type aliases vars []) args)
  | ECase (e, branches) ->
      ECase (expand_expr_types aliases vars e, List.map (expand_branch_types aliases vars) branches)
  | EFoldNat (n, z, step) ->
      EFoldNat
        ( expand_expr_types aliases vars n,
          expand_expr_types aliases vars z,
          expand_expr_types aliases vars step )
  | ENil t -> ENil (expand_type aliases vars [] t)
  | ECons (t, head, tail) ->
      ECons
        ( expand_type aliases vars [] t,
          expand_expr_types aliases vars head,
          expand_expr_types aliases vars tail )
  | EFoldList (xs, z, step) ->
      EFoldList
        ( expand_expr_types aliases vars xs,
          expand_expr_types aliases vars z,
          expand_expr_types aliases vars step )
  | EText e -> EText (expand_expr_types aliases vars e)
  | EImage (src, alt) ->
      EImage (expand_expr_types aliases vars src, expand_expr_types aliases vars alt)
  | EButton (label, msg) ->
      EButton (expand_expr_types aliases vars label, expand_expr_types aliases vars msg)
  | EInput (value, handler) ->
      EInput (expand_expr_types aliases vars value, expand_expr_types aliases vars handler)
  | EColumn children -> EColumn (expand_expr_types aliases vars children)
  | ERow children -> ERow (expand_expr_types aliases vars children)
  | EListView (items, render) ->
      EListView (expand_expr_types aliases vars items, expand_expr_types aliases vars render)
  | EWhenView (cond, view) ->
      EWhenView (expand_expr_types aliases vars cond, expand_expr_types aliases vars view)
  | EDone e -> EDone (expand_expr_types aliases vars e)
  | ERequest req -> ERequest req
  | EBind (p, x, t, body) ->
      EBind
        ( expand_expr_types aliases vars p,
          x,
          expand_type aliases vars [] t,
          expand_expr_types aliases vars body )

and expand_branch_types aliases vars = function
  | BBool (b, e) -> BBool (b, expand_expr_types aliases vars e)
  | BVariant (con, x, e) -> BVariant (con, x, expand_expr_types aliases vars e)

let resolve_program_types program =
  check_duplicate_type_aliases program.type_aliases;
  let aliases = program.type_aliases in
  let expanded_aliases =
    List.map
      (fun a ->
        let vars = List.map (fun param -> (param, TNamed (param, []))) a.type_params in
        { a with type_body = expand_type aliases vars [ a.type_name ] a.type_body })
      program.type_aliases
  in
  let defs =
    List.map
      (fun (d : def) ->
        let vars = type_var_env d.type_params in
        let typ = expand_type aliases vars [] d.typ in
        {
          d with
          typ =
            (match d.type_params with
            | [] -> typ
            | params -> TForall (List.length params, typ));
          body = expand_expr_types aliases vars d.body;
        })
      program.defs
  in
  { program with type_aliases = expanded_aliases; defs }

let collect_deps defs =
  let def_names = List.map (fun d -> d.name) defs in
  let is_global n = List.exists (String.equal n) def_names in
  let rec expr bound acc = function
    | EUnit | EBool _ | ENat _ | EString _ | ERequest _ | ENil _ -> acc
    | EName n ->
        if List.exists (String.equal n) bound || is_builtin n then acc
        else if is_global n && not (List.exists (String.equal n) acc) then n :: acc
        else acc
    | ELambda (x, _, body) -> expr (x :: bound) acc body
    | EApp (f, x) -> expr bound (expr bound acc f) x
    | ELet (x, e, body) -> expr (x :: bound) (expr bound acc e) body
    | ERecord fields -> List.fold_left (fun a (_, e) -> expr bound a e) acc fields
    | EField (e, _) -> expr bound acc e
    | EVariant (_, _, e) | EVariantInferred (_, e) -> expr bound acc e
    | EInst (n, _) ->
        if is_global n && not (List.exists (String.equal n) acc) then n :: acc else acc
    | ECase (e, branches) ->
        List.fold_left
          (fun a -> function
            | BBool (_, b) -> expr bound a b
            | BVariant (_, x, b) -> expr (x :: bound) a b)
          (expr bound acc e) branches
    | EFoldNat (n, z, step) -> expr bound (expr bound (expr bound acc n) z) step
    | ECons (_, head, tail) -> expr bound (expr bound acc head) tail
    | EFoldList (xs, z, step) -> expr bound (expr bound (expr bound acc xs) z) step
    | EText e | EColumn e | ERow e -> expr bound acc e
    | EButton (label, msg) | EInput (label, msg) | EImage (label, msg)
    | EListView (label, msg) | EWhenView (label, msg) ->
        expr bound (expr bound acc label) msg
    | EDone e -> expr bound acc e
    | EBind (p, x, _, body) -> expr (x :: bound) (expr bound acc p) body
  in
  List.map (fun d -> (d.name, expr [] [] d.body)) defs

let dependencies_of_defs defs name =
  Option.value (assoc_opt name (collect_deps defs)) ~default:[]

let reject_cycles defs =
  let deps = collect_deps defs in
  let state = Hashtbl.create 32 in
  let rec visit path n =
    match Hashtbl.find_opt state n with
    | Some `Done -> ()
    | Some `Visiting ->
        fail ("general recursion rejected: " ^ String.concat " -> " (List.rev (n :: path)))
    | None ->
        Hashtbl.add state n `Visiting;
        let ds = Option.value (assoc_opt n deps) ~default:[] in
        List.iter (visit (n :: path)) ds;
        Hashtbl.replace state n `Done
  in
  List.iter (fun d -> visit [] d.name) defs

type global_type = { global_type_params : string list; global_typ : typ }

type type_ctx = {
  globals : (string * global_type) list;
  capabilities : string list;
  locals : (string * typ) list;
}

let rec subst_type args = function
  | TUnit -> TUnit
  | TBool -> TBool
  | TNat -> TNat
  | TString -> TString
  | TFun (a, b) -> TFun (subst_type args a, subst_type args b)
  | TRecord fields -> TRecord (sort_fields (List.map (fun (n, t) -> (n, subst_type args t)) fields))
  | TVariant cases -> TVariant (sort_fields (List.map (fun (n, t) -> (n, subst_type args t)) cases))
  | TList t -> TList (subst_type args t)
  | TView t -> TView (subst_type args t)
  | TProcess t -> TProcess (subst_type args t)
  | TVar i -> option_or_fail ("type argument missing for TVar " ^ string_of_int i) (List.nth_opt args i)
  | TForall (arity, body) -> TForall (arity, subst_type args body)
  | TNamed (n, ts) -> TNamed (n, List.map (subst_type args) ts)

let instantiate_type name params typ args =
  let arity = List.length params and actual = List.length args in
  if arity <> actual then
    fail
      ("polymorphic definition " ^ name ^ " expects " ^ string_of_int arity
     ^ " type argument(s), got " ^ string_of_int actual);
  let body = match typ with TForall (_, body) -> body | t -> t in
  subst_type args body

let lookup_global ctx n =
  assoc_opt n ctx.globals

let lookup_type ctx n =
  match assoc_opt n ctx.locals with
  | Some t -> Some t
  | None -> (
      match lookup_global ctx n with
      | Some g when g.global_type_params = [] -> Some g.global_typ
      | Some _ -> fail ("polymorphic definition requires inst: " ^ n)
      | None -> assoc_opt n builtin_types)

let edit_distance a b =
  let la = String.length a and lb = String.length b in
  let prev = Array.init (lb + 1) (fun j -> j) in
  let curr = Array.make (lb + 1) 0 in
  for i = 1 to la do
    curr.(0) <- i;
    for j = 1 to lb do
      let cost = if Char.equal a.[i - 1] b.[j - 1] then 0 else 1 in
      curr.(j) <- min (min (prev.(j) + 1) (curr.(j - 1) + 1)) (prev.(j - 1) + cost)
    done;
    Array.blit curr 0 prev 0 (lb + 1)
  done;
  prev.(lb)

let suggestion ctx n =
  let names =
    List.map fst ctx.locals @ List.map fst ctx.globals @ builtin_names
    |> List.sort_uniq String.compare
  in
  names
  |> List.map (fun candidate -> (edit_distance n candidate, candidate))
  |> List.sort (fun (a, _) (b, _) -> Int.compare a b)
  |> function
  | (distance, candidate) :: _ when distance <= 4 -> " Did you mean " ^ candidate ^ "?"
  | _ -> ""

let require_type expected actual where =
  if not (equal_typ expected actual) then
    fail
      (where ^ ": expected " ^ string_of_typ expected ^ ", got " ^ string_of_typ actual)

let has_capability ctx cap = List.exists (String.equal cap) ctx.capabilities

let rec contains_process_type = function
  | TProcess _ -> true
  | TFun (a, b) -> contains_process_type a || contains_process_type b
  | TRecord fields | TVariant fields -> List.exists (fun (_, t) -> contains_process_type t) fields
  | TList t | TView t -> contains_process_type t
  | TForall (_, t) -> contains_process_type t
  | TNamed _ -> false
  | TVar _ | TUnit | TBool | TNat | TString -> false

let is_process_type = function TProcess _ -> true | _ -> false

let rec infer ctx = function
  | EUnit -> TUnit
  | EBool _ -> TBool
  | ENat _ -> TNat
  | EString _ -> TString
  | EName n -> (
      match lookup_type ctx n with
      | Some t -> t
      | None -> fail ("unknown name: " ^ n ^ "." ^ suggestion ctx n))
  | ELambda (x, t, body) ->
      TFun (t, infer { ctx with locals = (x, t) :: ctx.locals } body)
  | EApp (f, arg) -> (
      match infer ctx f with
      | TFun (a, b) ->
          let at = infer ctx arg in
          require_type a at "application";
          b
      | t -> fail ("application of non-function: " ^ string_of_typ t))
  | ELet (x, e, body) ->
      let t = infer ctx e in
      let body_ty = infer { ctx with locals = (x, t) :: ctx.locals } body in
      if is_process_type t && not (is_process_type body_ty) then
        fail
          ("Process used as pure value in let " ^ x ^ ": expected Process flow, got "
         ^ string_of_typ body_ty ^ ", expression " ^ string_of_expr body);
      body_ty
  | ERecord fields ->
      let fields = List.map (fun (n, e) -> (n, infer ctx e, e)) fields in
      List.iter
        (fun (n, t, e) ->
          if contains_process_type t then
            fail
              ("Process used as pure record field " ^ n ^ ": got " ^ string_of_typ t
             ^ ", expression " ^ string_of_expr e))
        fields;
      TRecord (sort_fields (List.map (fun (n, t, _) -> (n, t)) fields))
  | EField (e, field) -> (
      match infer ctx e with
      | TRecord fields -> (
          match assoc_opt field fields with
          | Some t -> t
          | None -> fail ("unknown record field: " ^ field))
      | t -> fail ("field access on non-record: " ^ string_of_typ t))
  | EVariant (ty, con, e) -> (
      match ty with
      | TVariant cases -> (
          match assoc_opt con cases with
          | Some payload_ty ->
              let actual = infer ctx e in
              require_type payload_ty actual ("variant " ^ con);
              if contains_process_type actual then
                fail
                  ("Process used as pure variant payload " ^ con ^ ": got "
                 ^ string_of_typ actual ^ ", expression " ^ string_of_expr e);
              ty
          | None -> fail ("unknown variant constructor: " ^ con))
      | _ -> fail "variant expression must carry a Variant type")
  | EVariantInferred (con, _) ->
      fail ("variant constructor " ^ con ^ " requires an expected Variant type")
  | EInst (name, args) -> (
      match lookup_global ctx name with
      | Some g when g.global_type_params <> [] ->
          instantiate_type name g.global_type_params g.global_typ args
      | Some _ -> fail ("definition is not polymorphic: " ^ name)
      | None -> fail ("unknown polymorphic definition: " ^ name ^ "." ^ suggestion ctx name))
  | ECase (scrut, branches) -> (
      match infer ctx scrut with
      | TBool -> infer_bool_case ctx branches
      | TVariant cases -> infer_variant_case ctx cases branches
      | t -> fail ("case on unsupported type: " ^ string_of_typ t))
  | EFoldNat (n, zero, step) ->
      require_type TNat (infer ctx n) "foldNat index";
      let result_ty = infer ctx zero in
      require_type (TFun (result_ty, result_ty)) (infer ctx step) "foldNat step";
      result_ty
  | ENil t -> TList t
  | ECons (t, head, tail) ->
      require_type t (infer ctx head) "Cons head";
      require_type (TList t) (infer ctx tail) "Cons tail";
      TList t
  | EFoldList (xs, zero, step) -> (
      match infer ctx xs with
      | TList item_ty ->
          let result_ty = infer ctx zero in
          require_type (TFun (item_ty, TFun (result_ty, result_ty))) (infer ctx step)
            "foldList step";
          result_ty
      | t -> fail ("foldList target must be List, got " ^ string_of_typ t))
  | EText e ->
      require_type TString (infer ctx e) "text";
      TView TUnit
  | EImage (src, alt) ->
      require_type TString (infer ctx src) "image src";
      require_type TString (infer ctx alt) "image alt";
      TView TUnit
  | EButton (label, msg) ->
      require_type TString (infer ctx label) "button label";
      TView (infer ctx msg)
  | EInput (value, handler) -> (
      require_type TString (infer ctx value) "input value";
      match infer ctx handler with
      | TFun (TString, msg_ty) -> TView msg_ty
      | t -> fail ("input handler must be String -> msg, got " ^ string_of_typ t))
  | EColumn children -> (
      match infer ctx children with
      | TList (TView msg_ty) -> TView msg_ty
      | t -> fail ("column expects List (View msg), got " ^ string_of_typ t))
  | ERow children -> (
      match infer ctx children with
      | TList (TView msg_ty) -> TView msg_ty
      | t -> fail ("row expects List (View msg), got " ^ string_of_typ t))
  | EListView (items, render) -> (
      match infer ctx items with
      | TList item_ty -> (
          match infer ctx render with
          | TFun (arg_ty, TView msg_ty) ->
              require_type item_ty arg_ty "list renderer";
              TView msg_ty
          | t -> fail ("list renderer must return View msg, got " ^ string_of_typ t))
      | t -> fail ("list expects List input, got " ^ string_of_typ t))
  | EWhenView (cond, view) ->
      require_type TBool (infer ctx cond) "when condition";
      (match infer ctx view with
      | TView _ as t -> t
      | t -> fail ("when expects View msg, got " ^ string_of_typ t))
  | EDone e -> TProcess (infer ctx e)
  | ERequest req ->
      let cap = req_capability req in
      if not (has_capability ctx cap) then fail ("missing capability: " ^ cap);
      TProcess (req_result_type req)
  | EBind (p, x, annotation, body) -> (
      match infer ctx p with
      | TProcess a ->
          require_type a annotation "bind annotation";
          let body_ty = infer { ctx with locals = (x, a) :: ctx.locals } body in
          (match body_ty with
          | TProcess _ -> body_ty
          | t -> fail ("bind body must return Process, got " ^ string_of_typ t))
      | t -> fail ("bind on non-process: " ^ string_of_typ t))

and infer_bool_case ctx branches =
  let true_branch = ref None and false_branch = ref None in
  List.iter
    (function
      | BBool (true, e) -> true_branch := Some e
      | BBool (false, e) -> false_branch := Some e
      | BVariant _ -> fail "variant branch in Bool case")
    branches;
  let t_expr = option_or_fail "Bool case missing true branch" !true_branch in
  let f_expr = option_or_fail "Bool case missing false branch" !false_branch in
  let ty = infer ctx t_expr in
  require_type ty (infer ctx f_expr) "Bool case branches";
  ty

and infer_variant_case ctx cases branches =
  let branch_names =
    List.map
      (function
        | BVariant (con, _, _) -> con
        | BBool _ -> fail "Bool branch in Variant case")
      branches
  in
  let case_names = List.map fst cases in
  List.iter
    (fun con ->
      if not (List.exists (String.equal con) branch_names) then
        fail ("Variant case missing branch: " ^ con))
    case_names;
  List.iter
    (fun con ->
      if not (List.exists (String.equal con) case_names) then
        fail ("unknown Variant branch: " ^ con))
    branch_names;
  let result = ref None in
  List.iter
    (function
      | BBool _ -> assert false
      | BVariant (con, x, body) ->
          let payload_ty = Option.get (assoc_opt con cases) in
          let ty = infer { ctx with locals = (x, payload_ty) :: ctx.locals } body in
          (match !result with
          | None -> result := Some ty
          | Some expected -> require_type expected ty "Variant case branches"))
    branches;
  option_or_fail "empty Variant case" !result

let rec infer_elab ctx expr =
  match expr with
  | EUnit | EBool _ | ENat _ | EString _ | EName _ | ERequest _ | ENil _ ->
      (infer ctx expr, expr)
  | ELambda (x, t, body) ->
      let body_ty, body = infer_elab { ctx with locals = (x, t) :: ctx.locals } body in
      (TFun (t, body_ty), ELambda (x, t, body))
  | EApp (f, arg) -> (
      let fn_ty, f = infer_elab ctx f in
      match fn_ty with
      | TFun (a, b) ->
          let _, arg = check_elab ctx a arg in
          (b, EApp (f, arg))
      | t -> fail ("application of non-function: " ^ string_of_typ t))
  | ELet (x, e, body) ->
      let t, e = infer_elab ctx e in
      let body_ty, body = infer_elab { ctx with locals = (x, t) :: ctx.locals } body in
      if is_process_type t && not (is_process_type body_ty) then
        fail
          ("Process used as pure value in let " ^ x ^ ": expected Process flow, got "
         ^ string_of_typ body_ty ^ ", expression " ^ string_of_expr body);
      (body_ty, ELet (x, e, body))
  | ERecord fields ->
      let fields =
        List.map
          (fun (n, e) ->
            let t, e = infer_elab ctx e in
            if contains_process_type t then
              fail
                ("Process used as pure record field " ^ n ^ ": got " ^ string_of_typ t
               ^ ", expression " ^ string_of_expr e);
            (n, t, e))
          fields
      in
      (TRecord (sort_fields (List.map (fun (n, t, _) -> (n, t)) fields)),
       ERecord (sort_fields (List.map (fun (n, _, e) -> (n, e)) fields)))
  | EField (e, field) -> (
      let t, e = infer_elab ctx e in
      match t with
      | TRecord fields -> (
          match assoc_opt field fields with
          | Some field_ty -> (field_ty, EField (e, field))
          | None -> fail ("unknown record field: " ^ field))
      | t -> fail ("field access on non-record: " ^ string_of_typ t))
  | EVariant (ty, con, e) ->
      let _, e = elaborate_variant_payload ctx ty con e in
      (ty, EVariant (ty, con, e))
  | EVariantInferred (con, _) ->
      fail ("variant constructor " ^ con ^ " requires an expected Variant type")
  | EInst (name, args) -> (
      match lookup_global ctx name with
      | Some g when g.global_type_params <> [] ->
          (instantiate_type name g.global_type_params g.global_typ args, EInst (name, args))
      | Some _ -> fail ("definition is not polymorphic: " ^ name)
      | None -> fail ("unknown polymorphic definition: " ^ name ^ "." ^ suggestion ctx name))
  | ECase (scrut, branches) -> (
      let scrut_ty, scrut = infer_elab ctx scrut in
      match scrut_ty with
      | TBool ->
          let ty, branches = infer_bool_case_elab ctx branches in
          (ty, ECase (scrut, branches))
      | TVariant cases ->
          let ty, branches = infer_variant_case_elab ctx cases branches in
          (ty, ECase (scrut, branches))
      | t -> fail ("case on unsupported type: " ^ string_of_typ t))
  | EFoldNat (n, zero, step) ->
      let _, n = check_elab ctx TNat n in
      let result_ty, zero = infer_elab ctx zero in
      let _, step = check_elab ctx (TFun (result_ty, result_ty)) step in
      (result_ty, EFoldNat (n, zero, step))
  | ECons (t, head, tail) ->
      let _, head = check_elab ctx t head in
      let _, tail = check_elab ctx (TList t) tail in
      (TList t, ECons (t, head, tail))
  | EFoldList (xs, zero, step) -> (
      let xs_ty, xs = infer_elab ctx xs in
      match xs_ty with
      | TList item_ty ->
          let result_ty, zero = infer_elab ctx zero in
          let _, step = check_elab ctx (TFun (item_ty, TFun (result_ty, result_ty))) step in
          (result_ty, EFoldList (xs, zero, step))
      | t -> fail ("foldList target must be List, got " ^ string_of_typ t))
  | EText e ->
      let _, e = check_elab ctx TString e in
      (TView TUnit, EText e)
  | EImage (src, alt) ->
      let _, src = check_elab ctx TString src in
      let _, alt = check_elab ctx TString alt in
      (TView TUnit, EImage (src, alt))
  | EButton (label, msg) ->
      let _, label = check_elab ctx TString label in
      let msg_ty, msg = infer_elab ctx msg in
      (TView msg_ty, EButton (label, msg))
  | EInput (value, handler) -> (
      let _, value = check_elab ctx TString value in
      let handler_ty, handler = infer_elab ctx handler in
      match handler_ty with
      | TFun (TString, msg_ty) -> (TView msg_ty, EInput (value, handler))
      | t -> fail ("input handler must be String -> msg, got " ^ string_of_typ t))
  | EColumn children ->
      let children_ty, children = infer_elab ctx children in
      (match children_ty with
      | TList (TView msg_ty) -> (TView msg_ty, EColumn children)
      | t -> fail ("column expects List (View msg), got " ^ string_of_typ t))
  | ERow children ->
      let children_ty, children = infer_elab ctx children in
      (match children_ty with
      | TList (TView msg_ty) -> (TView msg_ty, ERow children)
      | t -> fail ("row expects List (View msg), got " ^ string_of_typ t))
  | EListView (items, render) -> (
      let items_ty, items = infer_elab ctx items in
      match items_ty with
      | TList item_ty -> (
          let render_ty, render = infer_elab ctx render in
          match render_ty with
          | TFun (arg_ty, TView msg_ty) ->
              require_type item_ty arg_ty "list renderer";
              (TView msg_ty, EListView (items, render))
          | t -> fail ("list renderer must return View msg, got " ^ string_of_typ t))
      | t -> fail ("list expects List input, got " ^ string_of_typ t))
  | EWhenView (cond, view) ->
      let _, cond = check_elab ctx TBool cond in
      let view_ty, view = infer_elab ctx view in
      (match view_ty with
      | TView _ as t -> (t, EWhenView (cond, view))
      | t -> fail ("when expects View msg, got " ^ string_of_typ t))
  | EDone e ->
      let t, e = infer_elab ctx e in
      (TProcess t, EDone e)
  | EBind (p, x, annotation, body) ->
      let p_ty, p = infer_elab ctx p in
      (match p_ty with
      | TProcess a ->
          require_type a annotation "bind annotation";
          let body_ty, body = infer_elab { ctx with locals = (x, a) :: ctx.locals } body in
          (match body_ty with
          | TProcess _ -> (body_ty, EBind (p, x, annotation, body))
          | t -> fail ("bind body must return Process, got " ^ string_of_typ t))
      | t -> fail ("bind on non-process: " ^ string_of_typ t))

and check_elab ctx expected expr =
  match (expected, expr) with
  | TVariant _, EVariantInferred (con, e) ->
      let _, e = elaborate_variant_payload ctx expected con e in
      (expected, EVariant (expected, con, e))
  | TVariant _, EVariant (explicit_ty, con, e) ->
      require_type expected explicit_ty "variant expected type";
      let _, e = elaborate_variant_payload ctx expected con e in
      (expected, EVariant (expected, con, e))
  | TFun (expected_arg, expected_body), ELambda (x, actual_arg, body) ->
      require_type expected_arg actual_arg "lambda parameter";
      let _, body = check_elab { ctx with locals = (x, actual_arg) :: ctx.locals } expected_body body in
      (expected, ELambda (x, actual_arg, body))
  | TRecord expected_fields, ERecord fields ->
      let field_names = List.map fst fields in
      List.iter
        (fun (name, _) ->
          if not (List.exists (String.equal name) field_names) then
            fail ("record missing field: " ^ name))
        expected_fields;
      List.iter
        (fun name ->
          if assoc_opt name expected_fields = None then fail ("unknown record field: " ^ name))
        field_names;
      let fields =
        List.map
          (fun (name, expected_ty) ->
            let expr = option_or_fail ("record missing field: " ^ name) (assoc_opt name fields) in
            let _, expr = check_elab ctx expected_ty expr in
            (name, expr))
          (sort_fields expected_fields)
      in
      (expected, ERecord fields)
  | TProcess expected_value, EDone e ->
      let _, e = check_elab ctx expected_value e in
      (expected, EDone e)
  | TList item_ty, ECons (actual_item_ty, head, tail) ->
      require_type item_ty actual_item_ty "Cons type";
      let _, head = check_elab ctx item_ty head in
      let _, tail = check_elab ctx expected tail in
      (expected, ECons (actual_item_ty, head, tail))
  | _, EFoldNat (n, zero, step) ->
      let _, n = check_elab ctx TNat n in
      let _, zero = check_elab ctx expected zero in
      let _, step = check_elab ctx (TFun (expected, expected)) step in
      (expected, EFoldNat (n, zero, step))
  | _, EFoldList (xs, zero, step) -> (
      let xs_ty, xs = infer_elab ctx xs in
      match xs_ty with
      | TList item_ty ->
          let _, zero = check_elab ctx expected zero in
          let _, step = check_elab ctx (TFun (item_ty, TFun (expected, expected))) step in
          (expected, EFoldList (xs, zero, step))
      | t -> fail ("foldList target must be List, got " ^ string_of_typ t))
  | _, ELet (x, e, body) ->
      let t, e = infer_elab ctx e in
      let _, body = check_elab { ctx with locals = (x, t) :: ctx.locals } expected body in
      if is_process_type t && not (is_process_type expected) then
        fail
          ("Process used as pure value in let " ^ x ^ ": expected Process flow, got "
         ^ string_of_typ expected ^ ", expression " ^ string_of_expr body);
      (expected, ELet (x, e, body))
  | _, ECase (scrut, branches) ->
      let scrut_ty, scrut = infer_elab ctx scrut in
      let branches =
        match scrut_ty with
        | TBool -> check_bool_case_elab ctx expected branches
        | TVariant cases -> check_variant_case_elab ctx cases expected branches
        | t -> fail ("case on unsupported type: " ^ string_of_typ t)
      in
      (expected, ECase (scrut, branches))
  | _ ->
      let actual, expr = infer_elab ctx expr in
      require_type expected actual "expected context";
      (expected, expr)

and elaborate_variant_payload ctx ty con e =
  match ty with
  | TVariant cases -> (
      match assoc_opt con cases with
      | Some payload_ty ->
          let _, e = check_elab ctx payload_ty e in
          if contains_process_type payload_ty then
            fail
              ("Process used as pure variant payload " ^ con ^ ": got "
             ^ string_of_typ payload_ty ^ ", expression " ^ string_of_expr e);
          (ty, e)
      | None -> fail ("unknown variant constructor: " ^ con))
  | _ -> fail "variant expression must carry a Variant type"

and infer_bool_case_elab ctx branches =
  let true_branch = ref None and false_branch = ref None in
  List.iter
    (function
      | BBool (true, e) -> true_branch := Some e
      | BBool (false, e) -> false_branch := Some e
      | BVariant _ -> fail "variant branch in Bool case")
    branches;
  let t_expr = option_or_fail "Bool case missing true branch" !true_branch in
  let f_expr = option_or_fail "Bool case missing false branch" !false_branch in
  let ty, t_expr = infer_elab ctx t_expr in
  let _, f_expr = check_elab ctx ty f_expr in
  (ty, [ BBool (true, t_expr); BBool (false, f_expr) ])

and check_bool_case_elab ctx expected branches =
  let true_branch = ref None and false_branch = ref None in
  List.iter
    (function
      | BBool (true, e) -> true_branch := Some e
      | BBool (false, e) -> false_branch := Some e
      | BVariant _ -> fail "variant branch in Bool case")
    branches;
  let t_expr = option_or_fail "Bool case missing true branch" !true_branch in
  let f_expr = option_or_fail "Bool case missing false branch" !false_branch in
  let _, t_expr = check_elab ctx expected t_expr in
  let _, f_expr = check_elab ctx expected f_expr in
  [ BBool (true, t_expr); BBool (false, f_expr) ]

and infer_variant_case_elab ctx cases branches =
  let ty = infer_variant_case ctx cases branches in
  let branches = check_variant_case_elab ctx cases ty branches in
  (ty, branches)

and check_variant_case_elab ctx cases expected branches =
  let branch_names =
    List.map
      (function
        | BVariant (con, _, _) -> con
        | BBool _ -> fail "Bool branch in Variant case")
      branches
  in
  let case_names = List.map fst cases in
  List.iter
    (fun con ->
      if not (List.exists (String.equal con) branch_names) then
        fail ("Variant case missing branch: " ^ con))
    case_names;
  List.iter
    (fun con ->
      if not (List.exists (String.equal con) case_names) then
        fail ("unknown Variant branch: " ^ con))
    branch_names;
  List.map
    (function
      | BBool _ -> assert false
      | BVariant (con, x, body) ->
          let payload_ty = Option.get (assoc_opt con cases) in
          let _, body = check_elab { ctx with locals = (x, payload_ty) :: ctx.locals } expected body in
          BVariant (con, x, body))
    branches

type cterm =
  | CUnit
  | CBool of bool
  | CNat of int
  | CString of string
  | CVar of int
  | CGlobal of string
  | CLambda of typ * cterm
  | CApp of cterm * cterm
  | CLet of cterm * cterm
  | CRecord of (string * cterm) list
  | CField of cterm * string
  | CVariant of typ * string * cterm
  | CInst of string * typ list
  | CCase of cterm * cbranch list
  | CFoldNat of cterm * cterm * cterm
  | CNil of typ
  | CCons of typ * cterm * cterm
  | CFoldList of cterm * cterm * cterm
  | CText of cterm
  | CImage of cterm * cterm
  | CButton of cterm * cterm
  | CInput of cterm * cterm
  | CColumn of cterm
  | CRow of cterm
  | CListView of cterm * cterm
  | CWhenView of cterm * cterm
  | CDone of cterm
  | CRequest of req
  | CBind of cterm * typ * cterm

and cbranch =
  | CBBool of bool * cterm
  | CBVariant of string * cterm

let rec index_of x env i =
  match env with
  | [] -> None
  | y :: ys -> if String.equal x y then Some i else index_of x ys (i + 1)

let rec canonical_expr env = function
  | EUnit -> CUnit
  | EBool b -> CBool b
  | ENat n -> CNat n
  | EString s -> CString s
  | EName n -> (
      match index_of n env 0 with Some i -> CVar i | None -> CGlobal n)
  | ELambda (x, t, body) -> CLambda (t, canonical_expr (x :: env) body)
  | EApp (f, x) -> CApp (canonical_expr env f, canonical_expr env x)
  | ELet (x, e, body) -> CLet (canonical_expr env e, canonical_expr (x :: env) body)
  | ERecord fields ->
      CRecord (sort_fields (List.map (fun (n, e) -> (n, canonical_expr env e)) fields))
  | EField (e, field) -> CField (canonical_expr env e, field)
  | EVariant (t, con, e) -> CVariant (t, con, canonical_expr env e)
  | EVariantInferred (con, _) -> fail ("unelaborated variant constructor in canonicalization: " ^ con)
  | EInst (name, args) ->
      if index_of name env 0 <> None then fail ("cannot instantiate local binding: " ^ name);
      CInst (name, args)
  | ECase (e, branches) -> CCase (canonical_expr env e, canonical_branches env branches)
  | EFoldNat (n, z, step) ->
      CFoldNat (canonical_expr env n, canonical_expr env z, canonical_expr env step)
  | ENil t -> CNil t
  | ECons (t, head, tail) -> CCons (t, canonical_expr env head, canonical_expr env tail)
  | EFoldList (xs, z, step) ->
      CFoldList (canonical_expr env xs, canonical_expr env z, canonical_expr env step)
  | EText e -> CText (canonical_expr env e)
  | EImage (src, alt) -> CImage (canonical_expr env src, canonical_expr env alt)
  | EButton (label, msg) -> CButton (canonical_expr env label, canonical_expr env msg)
  | EInput (value, handler) -> CInput (canonical_expr env value, canonical_expr env handler)
  | EColumn children -> CColumn (canonical_expr env children)
  | ERow children -> CRow (canonical_expr env children)
  | EListView (items, render) -> CListView (canonical_expr env items, canonical_expr env render)
  | EWhenView (cond, view) -> CWhenView (canonical_expr env cond, canonical_expr env view)
  | EDone e -> CDone (canonical_expr env e)
  | ERequest req -> CRequest req
  | EBind (p, x, t, body) -> CBind (canonical_expr env p, t, canonical_expr (x :: env) body)

and canonical_branches env branches =
  let cbs =
    List.map
      (function
        | BBool (b, e) -> CBBool (b, canonical_expr env e)
        | BVariant (con, x, e) -> CBVariant (con, canonical_expr (x :: env) e))
      branches
  in
  List.sort
    (fun a b ->
      let ka = match a with CBBool (v, _) -> if v then "1" else "0" | CBVariant (c, _) -> c in
      let kb = match b with CBBool (v, _) -> if v then "1" else "0" | CBVariant (c, _) -> c in
      String.compare ka kb)
    cbs

let rec cterm_to_string = function
  | CUnit -> "unit"
  | CBool true -> "true"
  | CBool false -> "false"
  | CNat n -> string_of_int n
  | CString s -> Ast.quote s
  | CVar i -> "#" ^ string_of_int i
  | CGlobal n -> "@" ^ n
  | CLambda (t, body) -> "(lam " ^ type_to_canonical t ^ " " ^ cterm_to_string body ^ ")"
  | CApp (f, x) -> "(app " ^ cterm_to_string f ^ " " ^ cterm_to_string x ^ ")"
  | CLet (e, body) -> "(let " ^ cterm_to_string e ^ " " ^ cterm_to_string body ^ ")"
  | CRecord fields ->
      "(record "
      ^ String.concat " "
          (List.map (fun (n, e) -> "(" ^ n ^ " " ^ cterm_to_string e ^ ")") fields)
      ^ ")"
  | CField (e, field) -> "(field " ^ cterm_to_string e ^ " " ^ field ^ ")"
  | CVariant (t, con, e) ->
      "(variant " ^ type_to_canonical t ^ " " ^ con ^ " " ^ cterm_to_string e ^ ")"
  | CInst (name, args) ->
      "(inst @" ^ name ^ " "
      ^ String.concat " " (List.map type_to_canonical args)
      ^ ")"
  | CCase (e, branches) ->
      "(case " ^ cterm_to_string e ^ " "
      ^ String.concat " " (List.map cbranch_to_string branches)
      ^ ")"
  | CFoldNat (n, z, step) ->
      "(foldNat " ^ cterm_to_string n ^ " " ^ cterm_to_string z ^ " "
      ^ cterm_to_string step ^ ")"
  | CNil t -> "(Nil " ^ type_to_canonical t ^ ")"
  | CCons (t, head, tail) ->
      "(Cons " ^ type_to_canonical t ^ " " ^ cterm_to_string head ^ " " ^ cterm_to_string tail
      ^ ")"
  | CFoldList (xs, z, step) ->
      "(foldList " ^ cterm_to_string xs ^ " " ^ cterm_to_string z ^ " "
      ^ cterm_to_string step ^ ")"
  | CText e -> "(text " ^ cterm_to_string e ^ ")"
  | CImage (src, alt) -> "(image " ^ cterm_to_string src ^ " " ^ cterm_to_string alt ^ ")"
  | CButton (label, msg) -> "(button " ^ cterm_to_string label ^ " " ^ cterm_to_string msg ^ ")"
  | CInput (value, handler) ->
      "(input " ^ cterm_to_string value ^ " " ^ cterm_to_string handler ^ ")"
  | CColumn children -> "(column " ^ cterm_to_string children ^ ")"
  | CRow children -> "(row " ^ cterm_to_string children ^ ")"
  | CListView (items, render) ->
      "(list " ^ cterm_to_string items ^ " " ^ cterm_to_string render ^ ")"
  | CWhenView (cond, view) ->
      "(when " ^ cterm_to_string cond ^ " " ^ cterm_to_string view ^ ")"
  | CDone e -> "(done " ^ cterm_to_string e ^ ")"
  | CRequest req -> req_to_canonical req
  | CBind (p, t, body) ->
      "(bind " ^ cterm_to_string p ^ " " ^ type_to_canonical t ^ " " ^ cterm_to_string body
      ^ ")"

and cbranch_to_string = function
  | CBBool (true, e) -> "(true " ^ cterm_to_string e ^ ")"
  | CBBool (false, e) -> "(false " ^ cterm_to_string e ^ ")"
  | CBVariant (con, e) -> "(" ^ con ^ " " ^ cterm_to_string e ^ ")"

let canonical_version = "protoss-canon-v2"

type canonical_def = {
  cname : string;
  cdef_id : string;
  ctyp : typ;
  cbody : cterm;
}

let rec cterm_to_canonical_v2 def_id_of = function
  | CUnit -> "unit"
  | CBool true -> "true"
  | CBool false -> "false"
  | CNat n -> string_of_int n
  | CString s -> Ast.quote s
  | CVar i -> "#" ^ string_of_int i
  | CGlobal n when is_builtin n -> "(builtin " ^ n ^ ")"
  | CGlobal n -> "(ref " ^ def_id_of n ^ ")"
  | CLambda (t, body) -> "(lam " ^ type_to_canonical t ^ " " ^ cterm_to_canonical_v2 def_id_of body ^ ")"
  | CApp (f, x) ->
      "(app " ^ cterm_to_canonical_v2 def_id_of f ^ " " ^ cterm_to_canonical_v2 def_id_of x ^ ")"
  | CLet (e, body) ->
      "(let " ^ cterm_to_canonical_v2 def_id_of e ^ " "
      ^ cterm_to_canonical_v2 def_id_of body ^ ")"
  | CRecord fields ->
      "(record "
      ^ String.concat " "
          (List.map
             (fun (n, e) -> "(" ^ n ^ " " ^ cterm_to_canonical_v2 def_id_of e ^ ")")
             fields)
      ^ ")"
  | CField (e, field) -> "(field " ^ cterm_to_canonical_v2 def_id_of e ^ " " ^ field ^ ")"
  | CVariant (t, con, e) ->
      "(variant " ^ type_to_canonical t ^ " " ^ con ^ " "
      ^ cterm_to_canonical_v2 def_id_of e ^ ")"
  | CInst (name, args) ->
      "(inst " ^ def_id_of name ^ " "
      ^ String.concat " " (List.map type_to_canonical args)
      ^ ")"
  | CCase (e, branches) ->
      "(case " ^ cterm_to_canonical_v2 def_id_of e ^ " "
      ^ String.concat " " (List.map (cbranch_to_canonical_v2 def_id_of) branches)
      ^ ")"
  | CFoldNat (n, z, step) ->
      "(foldNat " ^ cterm_to_canonical_v2 def_id_of n ^ " " ^ cterm_to_canonical_v2 def_id_of z
      ^ " " ^ cterm_to_canonical_v2 def_id_of step ^ ")"
  | CNil t -> "(Nil " ^ type_to_canonical t ^ ")"
  | CCons (t, head, tail) ->
      "(Cons " ^ type_to_canonical t ^ " " ^ cterm_to_canonical_v2 def_id_of head ^ " "
      ^ cterm_to_canonical_v2 def_id_of tail ^ ")"
  | CFoldList (xs, z, step) ->
      "(foldList " ^ cterm_to_canonical_v2 def_id_of xs ^ " "
      ^ cterm_to_canonical_v2 def_id_of z ^ " " ^ cterm_to_canonical_v2 def_id_of step ^ ")"
  | CText e -> "(text " ^ cterm_to_canonical_v2 def_id_of e ^ ")"
  | CImage (src, alt) ->
      "(image " ^ cterm_to_canonical_v2 def_id_of src ^ " "
      ^ cterm_to_canonical_v2 def_id_of alt ^ ")"
  | CButton (label, msg) ->
      "(button " ^ cterm_to_canonical_v2 def_id_of label ^ " "
      ^ cterm_to_canonical_v2 def_id_of msg ^ ")"
  | CInput (value, handler) ->
      "(input " ^ cterm_to_canonical_v2 def_id_of value ^ " "
      ^ cterm_to_canonical_v2 def_id_of handler ^ ")"
  | CColumn children -> "(column " ^ cterm_to_canonical_v2 def_id_of children ^ ")"
  | CRow children -> "(row " ^ cterm_to_canonical_v2 def_id_of children ^ ")"
  | CListView (items, render) ->
      "(list " ^ cterm_to_canonical_v2 def_id_of items ^ " "
      ^ cterm_to_canonical_v2 def_id_of render ^ ")"
  | CWhenView (cond, view) ->
      "(when " ^ cterm_to_canonical_v2 def_id_of cond ^ " "
      ^ cterm_to_canonical_v2 def_id_of view ^ ")"
  | CDone e -> "(done " ^ cterm_to_canonical_v2 def_id_of e ^ ")"
  | CRequest req -> req_to_canonical req
  | CBind (p, t, body) ->
      "(bind " ^ cterm_to_canonical_v2 def_id_of p ^ " " ^ type_to_canonical t ^ " "
      ^ cterm_to_canonical_v2 def_id_of body ^ ")"

and cbranch_to_canonical_v2 def_id_of = function
  | CBBool (true, e) -> "(true " ^ cterm_to_canonical_v2 def_id_of e ^ ")"
  | CBBool (false, e) -> "(false " ^ cterm_to_canonical_v2 def_id_of e ^ ")"
  | CBVariant (con, e) -> "(" ^ con ^ " " ^ cterm_to_canonical_v2 def_id_of e ^ ")"

let serialize_def_payload name def_id typ body def_id_of =
  "(def " ^ name ^ " " ^ def_id ^ " " ^ type_to_canonical typ ^ " "
  ^ cterm_to_canonical_v2 def_id_of body ^ ")"

let serialize_def name def_id typ body def_id_of =
  "(" ^ canonical_version ^ " " ^ serialize_def_payload name def_id typ body def_id_of ^ ")"

let serialize_program_payload caps defs =
  let caps = List.sort_uniq String.compare caps in
  let defs =
    List.sort (fun a b -> String.compare a.cname b.cname) defs
    |> List.map (fun d ->
           serialize_def_payload d.cname d.cdef_id d.ctyp d.cbody (fun name ->
               let dep =
                 List.find_opt (fun d -> String.equal d.cname name || String.equal d.cdef_id name) defs
               in
               match dep with Some d -> d.cdef_id | None -> name))
  in
  "(program (caps " ^ String.concat " " caps ^ ") (defs "
  ^ String.concat " " defs ^ "))"

let serialize_program caps defs =
  "(" ^ canonical_version ^ " " ^ serialize_program_payload caps defs ^ ")"

let canonical_graph_version = "protoss-canon-graph-v1"

let json_string = Ast.quote

let json_field name value = json_string name ^ ": " ^ value

let json_obj fields = "{ " ^ String.concat ", " fields ^ " }"

let json_array f xs = "[" ^ String.concat ", " (List.map f xs) ^ "]"

let json_bool b = if b then "true" else "false"

let rec type_to_graph_json = function
  | TUnit -> json_obj [ json_field "tag" (json_string "Unit") ]
  | TBool -> json_obj [ json_field "tag" (json_string "Bool") ]
  | TNat -> json_obj [ json_field "tag" (json_string "Nat") ]
  | TString -> json_obj [ json_field "tag" (json_string "String") ]
  | TFun (a, b) ->
      json_obj
        [
          json_field "tag" (json_string "Fun");
          json_field "from" (type_to_graph_json a);
          json_field "to" (type_to_graph_json b);
        ]
  | TRecord fields ->
      json_obj
        [
          json_field "tag" (json_string "Record");
          json_field "fields"
            (json_array
               (fun (name, typ) ->
                 json_obj
                   [ json_field "name" (json_string name); json_field "type" (type_to_graph_json typ) ])
               (sort_fields fields));
        ]
  | TVariant cases ->
      json_obj
        [
          json_field "tag" (json_string "Variant");
          json_field "cases"
            (json_array
               (fun (name, typ) ->
                 json_obj
                   [ json_field "name" (json_string name); json_field "type" (type_to_graph_json typ) ])
               (sort_fields cases));
        ]
  | TList t ->
      json_obj [ json_field "tag" (json_string "List"); json_field "item" (type_to_graph_json t) ]
  | TView t ->
      json_obj [ json_field "tag" (json_string "View"); json_field "message" (type_to_graph_json t) ]
  | TProcess t ->
      json_obj [ json_field "tag" (json_string "Process"); json_field "result" (type_to_graph_json t) ]
  | TVar i ->
      json_obj [ json_field "tag" (json_string "TypeVar"); json_field "index" (string_of_int i) ]
  | TForall (arity, body) ->
      json_obj
        [
          json_field "tag" (json_string "Forall");
          json_field "arity" (string_of_int arity);
          json_field "body" (type_to_graph_json body);
        ]
  | TNamed (n, _) -> fail ("unresolved type alias in graph type: " ^ n)

let req_to_graph_json req =
  let tag, fields =
    match req with
    | AskHuman prompt -> ("AskHuman", [ json_field "prompt" (json_string prompt) ])
    | HttpGet url -> ("HttpGet", [ json_field "url" (json_string url) ])
    | ReadClock -> ("ReadClock", [])
    | SaveLocal (key, value) ->
        ("SaveLocal", [ json_field "key" (json_string key); json_field "value" (json_string value) ])
    | LoadLocal key -> ("LoadLocal", [ json_field "key" (json_string key) ])
    | ServerRequest (route, payload) ->
        ( "ServerRequest",
          [ json_field "route" (json_string route); json_field "payload" (json_string payload) ] )
  in
  json_obj
    (json_field "tag" (json_string tag) :: json_field "capability" (json_string (req_capability req))
    :: fields)

let capability_request_to_graph_json req =
  json_obj
    [
      json_field "tag" (json_string req.request_tag);
      json_field "payloadType" (type_to_graph_json req.request_payload_type);
      json_field "responseType" (type_to_graph_json req.response_type);
    ]

let capability_descriptor_to_graph_json desc =
  json_obj
    [
      json_field "name" (json_string desc.capability_name);
      json_field "requests" (json_array capability_request_to_graph_json desc.request_signatures);
    ]

let declared_capability_descriptors caps =
  caps
  |> List.sort_uniq String.compare
  |> List.filter_map capability_descriptor

let capabilities_to_graph_json caps =
  json_array capability_descriptor_to_graph_json (declared_capability_descriptors caps)

let rec cterm_to_graph_json def_id_of = function
  | CUnit -> json_obj [ json_field "tag" (json_string "Unit") ]
  | CBool b -> json_obj [ json_field "tag" (json_string "Bool"); json_field "value" (json_bool b) ]
  | CNat n ->
      json_obj [ json_field "tag" (json_string "Nat"); json_field "value" (string_of_int n) ]
  | CString s ->
      json_obj [ json_field "tag" (json_string "String"); json_field "value" (json_string s) ]
  | CVar i ->
      json_obj [ json_field "tag" (json_string "Var"); json_field "index" (string_of_int i) ]
  | CGlobal n when is_builtin n ->
      json_obj [ json_field "tag" (json_string "Builtin"); json_field "name" (json_string n) ]
  | CGlobal n ->
      json_obj [ json_field "tag" (json_string "Ref"); json_field "defId" (json_string (def_id_of n)) ]
  | CLambda (typ, body) ->
      json_obj
        [
          json_field "tag" (json_string "Lambda");
          json_field "paramType" (type_to_graph_json typ);
          json_field "body" (cterm_to_graph_json def_id_of body);
        ]
  | CApp (f, arg) ->
      json_obj
        [
          json_field "tag" (json_string "App");
          json_field "fn" (cterm_to_graph_json def_id_of f);
          json_field "arg" (cterm_to_graph_json def_id_of arg);
        ]
  | CLet (e, body) ->
      json_obj
        [
          json_field "tag" (json_string "Let");
          json_field "value" (cterm_to_graph_json def_id_of e);
          json_field "body" (cterm_to_graph_json def_id_of body);
        ]
  | CRecord fields ->
      json_obj
        [
          json_field "tag" (json_string "Record");
          json_field "fields"
            (json_array
               (fun (name, value) ->
                 json_obj
                   [
                     json_field "name" (json_string name);
                     json_field "value" (cterm_to_graph_json def_id_of value);
                   ])
               fields);
        ]
  | CField (e, field) ->
      json_obj
        [
          json_field "tag" (json_string "Field");
          json_field "record" (cterm_to_graph_json def_id_of e);
          json_field "field" (json_string field);
        ]
  | CVariant (typ, con, payload) ->
      json_obj
        [
          json_field "tag" (json_string "Variant");
          json_field "type" (type_to_graph_json typ);
          json_field "constructor" (json_string con);
          json_field "payload" (cterm_to_graph_json def_id_of payload);
        ]
  | CInst (name, args) ->
      json_obj
        [
          json_field "tag" (json_string "Inst");
          json_field "defId" (json_string (def_id_of name));
          json_field "typeArgs" (json_array type_to_graph_json args);
        ]
  | CCase (scrutinee, branches) ->
      json_obj
        [
          json_field "tag" (json_string "Case");
          json_field "scrutinee" (cterm_to_graph_json def_id_of scrutinee);
          json_field "branches" (json_array (cbranch_to_graph_json def_id_of) branches);
        ]
  | CFoldNat (n, zero, step) ->
      json_obj
        [
          json_field "tag" (json_string "FoldNat");
          json_field "index" (cterm_to_graph_json def_id_of n);
          json_field "zero" (cterm_to_graph_json def_id_of zero);
          json_field "step" (cterm_to_graph_json def_id_of step);
        ]
  | CNil typ ->
      json_obj [ json_field "tag" (json_string "Nil"); json_field "type" (type_to_graph_json typ) ]
  | CCons (typ, head, tail) ->
      json_obj
        [
          json_field "tag" (json_string "Cons");
          json_field "type" (type_to_graph_json typ);
          json_field "head" (cterm_to_graph_json def_id_of head);
          json_field "tail" (cterm_to_graph_json def_id_of tail);
        ]
  | CFoldList (xs, zero, step) ->
      json_obj
        [
          json_field "tag" (json_string "FoldList");
          json_field "list" (cterm_to_graph_json def_id_of xs);
          json_field "zero" (cterm_to_graph_json def_id_of zero);
          json_field "step" (cterm_to_graph_json def_id_of step);
        ]
  | CText e ->
      json_obj [ json_field "tag" (json_string "Text"); json_field "value" (cterm_to_graph_json def_id_of e) ]
  | CImage (src, alt) ->
      json_obj
        [
          json_field "tag" (json_string "Image");
          json_field "src" (cterm_to_graph_json def_id_of src);
          json_field "alt" (cterm_to_graph_json def_id_of alt);
        ]
  | CButton (label, msg) ->
      json_obj
        [
          json_field "tag" (json_string "Button");
          json_field "label" (cterm_to_graph_json def_id_of label);
          json_field "message" (cterm_to_graph_json def_id_of msg);
        ]
  | CInput (value, handler) ->
      json_obj
        [
          json_field "tag" (json_string "Input");
          json_field "value" (cterm_to_graph_json def_id_of value);
          json_field "handler" (cterm_to_graph_json def_id_of handler);
        ]
  | CColumn children ->
      json_obj
        [ json_field "tag" (json_string "Column"); json_field "children" (cterm_to_graph_json def_id_of children) ]
  | CRow children ->
      json_obj
        [ json_field "tag" (json_string "Row"); json_field "children" (cterm_to_graph_json def_id_of children) ]
  | CListView (items, render) ->
      json_obj
        [
          json_field "tag" (json_string "ListView");
          json_field "items" (cterm_to_graph_json def_id_of items);
          json_field "render" (cterm_to_graph_json def_id_of render);
        ]
  | CWhenView (cond, view) ->
      json_obj
        [
          json_field "tag" (json_string "WhenView");
          json_field "condition" (cterm_to_graph_json def_id_of cond);
          json_field "view" (cterm_to_graph_json def_id_of view);
        ]
  | CDone e ->
      json_obj [ json_field "tag" (json_string "Done"); json_field "value" (cterm_to_graph_json def_id_of e) ]
  | CRequest req ->
      json_obj [ json_field "tag" (json_string "Request"); json_field "request" (req_to_graph_json req) ]
  | CBind (p, typ, body) ->
      json_obj
        [
          json_field "tag" (json_string "Bind");
          json_field "process" (cterm_to_graph_json def_id_of p);
          json_field "valueType" (type_to_graph_json typ);
          json_field "body" (cterm_to_graph_json def_id_of body);
        ]

and cbranch_to_graph_json def_id_of = function
  | CBBool (b, body) ->
      json_obj
        [
          json_field "tag" (json_string "BoolBranch");
          json_field "value" (json_bool b);
          json_field "body" (cterm_to_graph_json def_id_of body);
        ]
  | CBVariant (con, body) ->
      json_obj
        [
          json_field "tag" (json_string "VariantBranch");
          json_field "constructor" (json_string con);
          json_field "body" (cterm_to_graph_json def_id_of body);
        ]

let single_sexp input =
  match Sexp.parse input with
  | [ x ] -> x
  | [] -> fail "empty canonical serialization"
  | _ -> fail "canonical serialization must contain one form"

let atom = function Sexp.Atom s -> s | x -> fail ("expected canonical atom, got " ^ Sexp.to_string x)

let strip_prefix prefix s =
  let plen = String.length prefix in
  if String.length s >= plen && String.sub s 0 plen = prefix then
    Some (String.sub s plen (String.length s - plen))
  else None

let parse_nat_atom s =
  try
    let n = int_of_string s in
    if n >= 0 then Some n else None
  with Failure _ -> None

let rec type_of_canonical_sexp = function
  | Sexp.Atom "Unit" -> TUnit
  | Sexp.Atom "Bool" -> TBool
  | Sexp.Atom "Nat" -> TNat
  | Sexp.Atom "String" -> TString
  | Sexp.List [ Sexp.Atom "Fun"; a; b ] ->
      TFun (type_of_canonical_sexp a, type_of_canonical_sexp b)
  | Sexp.List [ Sexp.Atom "Process"; t ] -> TProcess (type_of_canonical_sexp t)
  | Sexp.List [ Sexp.Atom "List"; t ] -> TList (type_of_canonical_sexp t)
  | Sexp.List [ Sexp.Atom "View"; t ] -> TView (type_of_canonical_sexp t)
  | Sexp.List [ Sexp.Atom "TVar"; Sexp.Atom i ] -> TVar (int_of_string i)
  | Sexp.List [ Sexp.Atom "Forall"; Sexp.Atom arity; body ] ->
      TForall (int_of_string arity, type_of_canonical_sexp body)
  | Sexp.List (Sexp.Atom "Record" :: fields) ->
      TRecord
        (sort_fields
           (List.map
              (function
                | Sexp.List [ Sexp.Atom n; t ] -> (n, type_of_canonical_sexp t)
                | x -> fail ("invalid canonical record field: " ^ Sexp.to_string x))
              fields))
  | Sexp.List (Sexp.Atom "Variant" :: cases) ->
      TVariant
        (sort_fields
           (List.map
              (function
                | Sexp.List [ Sexp.Atom n; t ] -> (n, type_of_canonical_sexp t)
                | x -> fail ("invalid canonical variant case: " ^ Sexp.to_string x))
              cases))
  | x -> fail ("invalid canonical type: " ^ Sexp.to_string x)

let req_of_canonical_sexp = function
  | Sexp.List [ Sexp.Atom "AskHuman"; Sexp.Str prompt ] -> AskHuman prompt
  | Sexp.List [ Sexp.Atom "HttpGet"; Sexp.Str url ] -> HttpGet url
  | Sexp.Atom "ReadClock" -> ReadClock
  | Sexp.List [ Sexp.Atom "SaveLocal"; Sexp.Str key; Sexp.Str value ] ->
      SaveLocal (key, value)
  | Sexp.List [ Sexp.Atom "LoadLocal"; Sexp.Str key ] -> LoadLocal key
  | Sexp.List [ Sexp.Atom "ServerRequest"; Sexp.Str route; Sexp.Str payload ] ->
      ServerRequest (route, payload)
  | x -> fail ("invalid canonical request: " ^ Sexp.to_string x)

let rec cterm_of_canonical_sexp = function
  | Sexp.Atom "unit" -> CUnit
  | Sexp.Atom "true" -> CBool true
  | Sexp.Atom "false" -> CBool false
  | Sexp.Atom "ReadClock" -> CRequest ReadClock
  | Sexp.List [ Sexp.Atom "builtin"; Sexp.Atom name ] when is_builtin name -> CGlobal name
  | Sexp.List [ Sexp.Atom "ref"; Sexp.Atom def_id ] -> CGlobal def_id
  | Sexp.Atom s -> (
      match (strip_prefix "#" s, strip_prefix "@" s, parse_nat_atom s) with
      | Some i, _, _ -> CVar (int_of_string i)
      | _, Some name, _ -> CGlobal name
      | _, _, Some n -> CNat n
      | _ -> fail ("invalid canonical atom: " ^ s))
  | Sexp.Str s -> CString s
  | Sexp.List [ Sexp.Atom "lam"; typ; body ] ->
      CLambda (type_of_canonical_sexp typ, cterm_of_canonical_sexp body)
  | Sexp.List [ Sexp.Atom "app"; f; arg ] ->
      CApp (cterm_of_canonical_sexp f, cterm_of_canonical_sexp arg)
  | Sexp.List [ Sexp.Atom "let"; e; body ] ->
      CLet (cterm_of_canonical_sexp e, cterm_of_canonical_sexp body)
  | Sexp.List (Sexp.Atom "record" :: fields) ->
      CRecord
        (sort_fields
           (List.map
              (function
                | Sexp.List [ Sexp.Atom n; e ] -> (n, cterm_of_canonical_sexp e)
                | x -> fail ("invalid canonical record value: " ^ Sexp.to_string x))
              fields))
  | Sexp.List [ Sexp.Atom "field"; e; Sexp.Atom field ] ->
      CField (cterm_of_canonical_sexp e, field)
  | Sexp.List [ Sexp.Atom "variant"; typ; Sexp.Atom con; e ] ->
      CVariant (type_of_canonical_sexp typ, con, cterm_of_canonical_sexp e)
  | Sexp.List (Sexp.Atom "inst" :: Sexp.Atom def_id :: args) ->
      CInst (def_id, List.map type_of_canonical_sexp args)
  | Sexp.List (Sexp.Atom "case" :: e :: branches) ->
      CCase (cterm_of_canonical_sexp e, List.map cbranch_of_canonical_sexp branches)
  | Sexp.List [ Sexp.Atom "foldNat"; n; zero; step ] ->
      CFoldNat
        (cterm_of_canonical_sexp n, cterm_of_canonical_sexp zero, cterm_of_canonical_sexp step)
  | Sexp.List [ Sexp.Atom "Nil"; typ ] -> CNil (type_of_canonical_sexp typ)
  | Sexp.List [ Sexp.Atom "Cons"; typ; head; tail ] ->
      CCons (type_of_canonical_sexp typ, cterm_of_canonical_sexp head, cterm_of_canonical_sexp tail)
  | Sexp.List [ Sexp.Atom "foldList"; xs; zero; step ] ->
      CFoldList
        (cterm_of_canonical_sexp xs, cterm_of_canonical_sexp zero, cterm_of_canonical_sexp step)
  | Sexp.List [ Sexp.Atom "text"; e ] -> CText (cterm_of_canonical_sexp e)
  | Sexp.List [ Sexp.Atom "image"; src; alt ] ->
      CImage (cterm_of_canonical_sexp src, cterm_of_canonical_sexp alt)
  | Sexp.List [ Sexp.Atom "button"; label; msg ] ->
      CButton (cterm_of_canonical_sexp label, cterm_of_canonical_sexp msg)
  | Sexp.List [ Sexp.Atom "input"; value; handler ] ->
      CInput (cterm_of_canonical_sexp value, cterm_of_canonical_sexp handler)
  | Sexp.List [ Sexp.Atom "column"; children ] -> CColumn (cterm_of_canonical_sexp children)
  | Sexp.List [ Sexp.Atom "row"; children ] -> CRow (cterm_of_canonical_sexp children)
  | Sexp.List [ Sexp.Atom "list"; items; render ] ->
      CListView (cterm_of_canonical_sexp items, cterm_of_canonical_sexp render)
  | Sexp.List [ Sexp.Atom "when"; cond; view ] ->
      CWhenView (cterm_of_canonical_sexp cond, cterm_of_canonical_sexp view)
  | Sexp.List [ Sexp.Atom "done"; e ] -> CDone (cterm_of_canonical_sexp e)
  | Sexp.List [ Sexp.Atom "AskHuman"; Sexp.Str _ ] as req -> CRequest (req_of_canonical_sexp req)
  | Sexp.List [ Sexp.Atom "HttpGet"; Sexp.Str _ ] as req -> CRequest (req_of_canonical_sexp req)
  | Sexp.List [ Sexp.Atom "SaveLocal"; Sexp.Str _; Sexp.Str _ ] as req ->
      CRequest (req_of_canonical_sexp req)
  | Sexp.List [ Sexp.Atom "LoadLocal"; Sexp.Str _ ] as req -> CRequest (req_of_canonical_sexp req)
  | Sexp.List [ Sexp.Atom "ServerRequest"; Sexp.Str _; Sexp.Str _ ] as req ->
      CRequest (req_of_canonical_sexp req)
  | Sexp.List [ Sexp.Atom "bind"; p; typ; body ] ->
      CBind (cterm_of_canonical_sexp p, type_of_canonical_sexp typ, cterm_of_canonical_sexp body)
  | x -> fail ("invalid canonical term: " ^ Sexp.to_string x)

and cbranch_of_canonical_sexp = function
  | Sexp.List [ Sexp.Atom "true"; e ] -> CBBool (true, cterm_of_canonical_sexp e)
  | Sexp.List [ Sexp.Atom "false"; e ] -> CBBool (false, cterm_of_canonical_sexp e)
  | Sexp.List [ Sexp.Atom con; e ] -> CBVariant (con, cterm_of_canonical_sexp e)
  | x -> fail ("invalid canonical branch: " ^ Sexp.to_string x)

let def_of_payload = function
  | Sexp.List [ Sexp.Atom "def"; Sexp.Atom name; Sexp.Atom def_id; typ; body ] ->
      {
        cname = name;
        cdef_id = def_id;
        ctyp = type_of_canonical_sexp typ;
        cbody = cterm_of_canonical_sexp body;
      }
  | Sexp.List [ Sexp.Atom "def"; Sexp.Atom name; typ; body ] ->
      {
        cname = name;
        cdef_id = hash_string ("legacy-def:" ^ name ^ ":" ^ Sexp.to_string typ ^ ":" ^ Sexp.to_string body);
        ctyp = type_of_canonical_sexp typ;
        cbody = cterm_of_canonical_sexp body;
      }
  | x -> fail ("invalid canonical def payload: " ^ Sexp.to_string x)

let parse_serialized_def input =
  match single_sexp input with
  | Sexp.List [ Sexp.Atom version; payload ] when String.equal version canonical_version ->
      def_of_payload payload
  | x -> fail ("invalid canonical def serialization: " ^ Sexp.to_string x)

let parse_serialized_program input =
  match single_sexp input with
  | Sexp.List
      [
        Sexp.Atom version;
        Sexp.List [ Sexp.Atom "program"; Sexp.List (Sexp.Atom "caps" :: caps); Sexp.List (Sexp.Atom "defs" :: defs) ];
      ]
    when String.equal version canonical_version ->
      (List.map atom caps, List.map def_of_payload defs)
  | x -> fail ("invalid canonical program serialization: " ^ Sexp.to_string x)

type checked_def = {
  def : def;
  def_id : string;
  cterm : cterm;
  canonical : string;
  hash : string;
}

type checked = {
  program : program;
  defs : checked_def list;
}

let check_program (program : program) =
  let program = resolve_program_types program in
  validate_capabilities program.capabilities;
  check_duplicate_names program.defs;
  reject_cycles program.defs;
  let globals =
    List.map (fun d -> (d.name, { global_type_params = d.type_params; global_typ = d.typ })) program.defs
  in
  let ctx = { globals; capabilities = program.capabilities; locals = [] } in
  let defs =
    List.map
      (fun d ->
        try
          let expected = match d.typ with TForall (_, body) -> body | t -> t in
          let actual, body = check_elab ctx expected d.body in
          if not (equal_typ expected actual) then
            fail
              ("definition " ^ d.name ^ ": expected " ^ string_of_typ expected ^ ", got "
             ^ string_of_typ actual ^ ", expression " ^ string_of_expr d.body);
          { d with body }
        with Error msg ->
          fail ("definition " ^ d.name ^ ": " ^ msg ^ ", expression " ^ string_of_expr d.body))
      program.defs
  in
  let program = { program with defs } in
  let cterms = Hashtbl.create 32 in
  List.iter (fun d -> Hashtbl.add cterms d.name (canonical_expr [] d.body)) program.defs;
  let defs_by_name = Hashtbl.create 32 in
  List.iter (fun d -> Hashtbl.add defs_by_name d.name d) program.defs;
  let def_ids = Hashtbl.create 32 in
  let rec def_id_of name =
    if is_builtin name then "builtin:" ^ name
    else
      match Hashtbl.find_opt def_ids name with
      | Some id -> id
      | None -> (
          match (Hashtbl.find_opt defs_by_name name, Hashtbl.find_opt cterms name) with
          | Some d, Some cterm ->
              let body = cterm_to_canonical_v2 def_id_of cterm in
              let id = hash_string ("defid-v2:" ^ type_to_canonical d.typ ^ ":" ^ body) in
              Hashtbl.add def_ids name id;
              id
          | _ -> fail ("unknown global definition in canonicalization: " ^ name))
  in
  let defs =
    List.map
      (fun d ->
        let cterm = Hashtbl.find cterms d.name in
        let def_id = def_id_of d.name in
        let c = serialize_def d.name def_id d.typ cterm def_id_of in
        let _ = Hashcons.intern c in
        { def = d; def_id; cterm; canonical = c; hash = hash_string c })
      program.defs
  in
  { program; defs }

let canonical_defs_of_checked checked =
  let defs =
    checked.defs
    |> List.map (fun d -> { cname = d.def.name; cdef_id = d.def_id; ctyp = d.def.typ; cbody = d.cterm })
  in
  defs

let serialize_checked_program checked =
  serialize_program checked.program.capabilities (canonical_defs_of_checked checked)

let hash_program checked =
  hash_string (serialize_checked_program checked)

let checked_to_graph_json checked =
  let defs = checked.defs |> List.sort (fun a b -> String.compare a.def.name b.def.name) in
  let def_id_of name =
    if is_builtin name then "builtin:" ^ name
    else
      match List.find_opt (fun d -> String.equal d.def.name name || String.equal d.def_id name) defs with
      | Some d -> d.def_id
      | None -> name
  in
  let def_json d =
    let canonical_payload = cterm_to_canonical_v2 def_id_of d.cterm in
    json_obj
      [
        json_field "name" (json_string d.def.name);
        json_field "defId" (json_string d.def_id);
        json_field "hash" (json_string d.hash);
        json_field "type" (type_to_graph_json d.def.typ);
        json_field "typeCanonical" (json_string (type_to_canonical d.def.typ));
        json_field "deps"
          (json_array json_string
             (dependencies_of_defs checked.program.defs d.def.name |> List.sort_uniq String.compare));
        json_field "term" (cterm_to_graph_json def_id_of d.cterm);
        json_field "termCanonical" (json_string canonical_payload);
      ]
  in
  json_obj
    [
      json_field "version" (json_string canonical_graph_version);
      json_field "canonicalVersion" (json_string canonical_version);
      json_field "programHash" (json_string (hash_program checked));
      json_field "capabilities"
        (json_array json_string (List.sort_uniq String.compare checked.program.capabilities));
      json_field "capabilityDescriptors" (capabilities_to_graph_json checked.program.capabilities);
      json_field "defs" (json_array def_json defs);
    ]
  ^ "\n"

let checked_def_by_name checked name =
  checked.defs |> List.find_opt (fun d -> String.equal d.def.name name || String.equal d.def_id name)

let rec shift amount cutoff = function
  | CUnit -> CUnit
  | CBool b -> CBool b
  | CNat n -> CNat n
  | CString s -> CString s
  | CVar i -> if i >= cutoff then CVar (i + amount) else CVar i
  | CGlobal n -> CGlobal n
  | CLambda (t, body) -> CLambda (t, shift amount (cutoff + 1) body)
  | CApp (f, x) -> CApp (shift amount cutoff f, shift amount cutoff x)
  | CLet (e, body) -> CLet (shift amount cutoff e, shift amount (cutoff + 1) body)
  | CRecord fields -> CRecord (List.map (fun (n, e) -> (n, shift amount cutoff e)) fields)
  | CField (e, field) -> CField (shift amount cutoff e, field)
  | CVariant (t, con, e) -> CVariant (t, con, shift amount cutoff e)
  | CInst (name, args) -> CInst (name, args)
  | CCase (e, branches) -> CCase (shift amount cutoff e, List.map (shift_branch amount cutoff) branches)
  | CFoldNat (n, zero, step) ->
      CFoldNat (shift amount cutoff n, shift amount cutoff zero, shift amount cutoff step)
  | CNil t -> CNil t
  | CCons (t, head, tail) -> CCons (t, shift amount cutoff head, shift amount cutoff tail)
  | CFoldList (xs, zero, step) ->
      CFoldList (shift amount cutoff xs, shift amount cutoff zero, shift amount cutoff step)
  | CText e -> CText (shift amount cutoff e)
  | CImage (src, alt) -> CImage (shift amount cutoff src, shift amount cutoff alt)
  | CButton (label, msg) -> CButton (shift amount cutoff label, shift amount cutoff msg)
  | CInput (value, handler) -> CInput (shift amount cutoff value, shift amount cutoff handler)
  | CColumn children -> CColumn (shift amount cutoff children)
  | CRow children -> CRow (shift amount cutoff children)
  | CListView (items, render) -> CListView (shift amount cutoff items, shift amount cutoff render)
  | CWhenView (cond, view) -> CWhenView (shift amount cutoff cond, shift amount cutoff view)
  | CDone e -> CDone (shift amount cutoff e)
  | CRequest req -> CRequest req
  | CBind (p, t, body) -> CBind (shift amount cutoff p, t, shift amount (cutoff + 1) body)

and shift_branch amount cutoff = function
  | CBBool (b, e) -> CBBool (b, shift amount cutoff e)
  | CBVariant (con, e) -> CBVariant (con, shift amount (cutoff + 1) e)

let rec subst index replacement = function
  | CUnit -> CUnit
  | CBool b -> CBool b
  | CNat n -> CNat n
  | CString s -> CString s
  | CVar i -> if i = index then replacement else CVar i
  | CGlobal n -> CGlobal n
  | CLambda (t, body) -> CLambda (t, subst (index + 1) (shift 1 0 replacement) body)
  | CApp (f, x) -> CApp (subst index replacement f, subst index replacement x)
  | CLet (e, body) -> CLet (subst index replacement e, subst (index + 1) (shift 1 0 replacement) body)
  | CRecord fields -> CRecord (List.map (fun (n, e) -> (n, subst index replacement e)) fields)
  | CField (e, field) -> CField (subst index replacement e, field)
  | CVariant (t, con, e) -> CVariant (t, con, subst index replacement e)
  | CInst (name, args) -> CInst (name, args)
  | CCase (e, branches) -> CCase (subst index replacement e, List.map (subst_branch index replacement) branches)
  | CFoldNat (n, zero, step) ->
      CFoldNat
        (subst index replacement n, subst index replacement zero, subst index replacement step)
  | CNil t -> CNil t
  | CCons (t, head, tail) -> CCons (t, subst index replacement head, subst index replacement tail)
  | CFoldList (xs, zero, step) ->
      CFoldList
        (subst index replacement xs, subst index replacement zero, subst index replacement step)
  | CText e -> CText (subst index replacement e)
  | CImage (src, alt) -> CImage (subst index replacement src, subst index replacement alt)
  | CButton (label, msg) -> CButton (subst index replacement label, subst index replacement msg)
  | CInput (value, handler) -> CInput (subst index replacement value, subst index replacement handler)
  | CColumn children -> CColumn (subst index replacement children)
  | CRow children -> CRow (subst index replacement children)
  | CListView (items, render) ->
      CListView (subst index replacement items, subst index replacement render)
  | CWhenView (cond, view) ->
      CWhenView (subst index replacement cond, subst index replacement view)
  | CDone e -> CDone (subst index replacement e)
  | CRequest req -> CRequest req
  | CBind (p, t, body) -> CBind (subst index replacement p, t, subst (index + 1) (shift 1 0 replacement) body)

and subst_branch index replacement = function
  | CBBool (b, e) -> CBBool (b, subst index replacement e)
  | CBVariant (con, e) -> CBVariant (con, subst (index + 1) (shift 1 0 replacement) e)

let subst_top replacement body = shift (-1) 0 (subst 0 (shift 1 0 replacement) body)

let rec subst_type_in_cterm args = function
  | CUnit -> CUnit
  | CBool b -> CBool b
  | CNat n -> CNat n
  | CString s -> CString s
  | CVar i -> CVar i
  | CGlobal n -> CGlobal n
  | CLambda (t, body) -> CLambda (subst_type args t, subst_type_in_cterm args body)
  | CApp (f, x) -> CApp (subst_type_in_cterm args f, subst_type_in_cterm args x)
  | CLet (e, body) -> CLet (subst_type_in_cterm args e, subst_type_in_cterm args body)
  | CRecord fields -> CRecord (List.map (fun (n, e) -> (n, subst_type_in_cterm args e)) fields)
  | CField (e, field) -> CField (subst_type_in_cterm args e, field)
  | CVariant (t, con, e) -> CVariant (subst_type args t, con, subst_type_in_cterm args e)
  | CInst (name, nested_args) -> CInst (name, List.map (subst_type args) nested_args)
  | CCase (e, branches) -> CCase (subst_type_in_cterm args e, List.map (subst_type_in_branch args) branches)
  | CFoldNat (n, zero, step) ->
      CFoldNat
        (subst_type_in_cterm args n, subst_type_in_cterm args zero, subst_type_in_cterm args step)
  | CNil t -> CNil (subst_type args t)
  | CCons (t, head, tail) ->
      CCons (subst_type args t, subst_type_in_cterm args head, subst_type_in_cterm args tail)
  | CFoldList (xs, zero, step) ->
      CFoldList
        (subst_type_in_cterm args xs, subst_type_in_cterm args zero, subst_type_in_cterm args step)
  | CText e -> CText (subst_type_in_cterm args e)
  | CImage (src, alt) -> CImage (subst_type_in_cterm args src, subst_type_in_cterm args alt)
  | CButton (label, msg) -> CButton (subst_type_in_cterm args label, subst_type_in_cterm args msg)
  | CInput (value, handler) -> CInput (subst_type_in_cterm args value, subst_type_in_cterm args handler)
  | CColumn children -> CColumn (subst_type_in_cterm args children)
  | CRow children -> CRow (subst_type_in_cterm args children)
  | CListView (items, render) -> CListView (subst_type_in_cterm args items, subst_type_in_cterm args render)
  | CWhenView (cond, view) -> CWhenView (subst_type_in_cterm args cond, subst_type_in_cterm args view)
  | CDone e -> CDone (subst_type_in_cterm args e)
  | CRequest req -> CRequest req
  | CBind (p, t, body) ->
      CBind (subst_type_in_cterm args p, subst_type args t, subst_type_in_cterm args body)

and subst_type_in_branch args = function
  | CBBool (b, e) -> CBBool (b, subst_type_in_cterm args e)
  | CBVariant (con, e) -> CBVariant (con, subst_type_in_cterm args e)

let rec normalize_cterm checked = function
  | CUnit | CBool _ | CNat _ | CString _ | CVar _ | CRequest _ | CNil _ as t -> t
  | CGlobal "succ" -> CGlobal "succ"
  | CGlobal name -> (
      match checked_def_by_name checked name with
      | None -> CGlobal name
      | Some d ->
          let canonical = parse_serialized_def d.canonical in
          normalize_cterm checked canonical.cbody)
  | CInst (name, args) -> (
      match checked_def_by_name checked name with
      | None -> CInst (name, args)
      | Some d ->
          let canonical = parse_serialized_def d.canonical in
          normalize_cterm checked (subst_type_in_cterm args canonical.cbody))
  | CLambda (t, body) -> CLambda (t, normalize_cterm checked body)
  | CApp (f, x) -> (
      let nf = normalize_cterm checked f in
      let nx = normalize_cterm checked x in
      match (nf, nx) with
      | CLambda (_, body), arg -> normalize_cterm checked (subst_top arg body)
      | CGlobal "succ", CNat n -> CNat (n + 1)
      | _ -> CApp (nf, nx))
  | CLet (e, body) -> normalize_cterm checked (subst_top (normalize_cterm checked e) body)
  | CRecord fields ->
      CRecord (sort_fields (List.map (fun (n, e) -> (n, normalize_cterm checked e)) fields))
  | CField (e, field) -> (
      match normalize_cterm checked e with
      | CRecord fields -> (
          match assoc_opt field fields with
          | Some v -> normalize_cterm checked v
          | None -> CField (CRecord fields, field))
      | e -> CField (e, field))
  | CVariant (t, con, e) -> CVariant (t, con, normalize_cterm checked e)
  | CCase (e, branches) -> (
      match normalize_cterm checked e with
      | CBool b -> (
          match
            List.find_map
              (function CBBool (b', body) when b = b' -> Some body | _ -> None)
              branches
          with
          | Some body -> normalize_cterm checked body
          | None -> CCase (CBool b, branches))
      | CVariant (_, con, payload) -> (
          match
            List.find_map
              (function CBVariant (con', body) when String.equal con con' -> Some body | _ -> None)
              branches
          with
          | Some body -> normalize_cterm checked (subst_top payload body)
          | None -> CCase (CVariant (TUnit, con, payload), branches))
      | e -> CCase (e, List.map (normalize_branch checked) branches))
  | CFoldNat (n, zero, step) -> (
      match normalize_cterm checked n with
      | CNat count ->
          let step = normalize_cterm checked step in
          let rec loop i acc =
            if i <= 0 then acc else loop (i - 1) (normalize_cterm checked (CApp (step, acc)))
          in
          loop count (normalize_cterm checked zero)
      | n -> CFoldNat (n, normalize_cterm checked zero, normalize_cterm checked step))
  | CCons (t, head, tail) ->
      CCons (t, normalize_cterm checked head, normalize_cterm checked tail)
  | CFoldList (xs, zero, step) -> (
      let step = normalize_cterm checked step in
      let acc = normalize_cterm checked zero in
      let rec loop = function
        | CNil _ -> acc
        | CCons (_, head, tail) ->
            let folded_tail = loop (normalize_cterm checked tail) in
            normalize_cterm checked (CApp (CApp (step, normalize_cterm checked head), folded_tail))
        | other -> CFoldList (other, acc, step)
      in
      loop (normalize_cterm checked xs))
  | CText e -> CText (normalize_cterm checked e)
  | CImage (src, alt) ->
      CImage (normalize_cterm checked src, normalize_cterm checked alt)
  | CButton (label, msg) -> CButton (normalize_cterm checked label, normalize_cterm checked msg)
  | CInput (value, handler) -> CInput (normalize_cterm checked value, normalize_cterm checked handler)
  | CColumn children -> CColumn (normalize_cterm checked children)
  | CRow children -> CRow (normalize_cterm checked children)
  | CListView (items, render) -> CListView (normalize_cterm checked items, normalize_cterm checked render)
  | CWhenView (cond, view) -> (
      match normalize_cterm checked cond with
      | CBool true -> normalize_cterm checked view
      | CBool false -> CColumn (CNil (TView TUnit))
      | cond -> CWhenView (cond, normalize_cterm checked view))
  | CDone e -> CDone (normalize_cterm checked e)
  | CBind (p, t, body) -> (
      match normalize_cterm checked p with
      | CDone v -> normalize_cterm checked (subst_top v body)
      | p -> CBind (p, t, normalize_cterm checked body))

and normalize_branch checked = function
  | CBBool (b, e) -> CBBool (b, normalize_cterm checked e)
  | CBVariant (con, e) -> CBVariant (con, normalize_cterm checked e)

let normalize_checked_def checked name =
  match checked_def_by_name checked name with
  | None -> fail ("unknown definition for normalization: " ^ name)
  | Some d ->
      let canonical = parse_serialized_def d.canonical in
      normalize_cterm checked canonical.cbody
