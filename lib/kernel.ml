open Ast

exception Error = Kernel_error.Error

let fail = Kernel_error.fail

let hash_algorithm = Hashcons.hash_algorithm

let hash_prefix = Hashcons.hash_prefix

let hash_string = Hashcons.hash

let builtin_types =
  [
    ("succ", TFun (TNat, TNat));
    ("prim.Nat.add", TFun (TNat, TFun (TNat, TNat)));
    ("prim.Nat.mul", TFun (TNat, TFun (TNat, TNat)));
    ("prim.Nat.pred", TFun (TNat, TNat));
    ("prim.Nat.sub", TFun (TNat, TFun (TNat, TNat)));
    ("prim.Nat.eq", TFun (TNat, TFun (TNat, TBool)));
    ("prim.Nat.lte", TFun (TNat, TFun (TNat, TBool)));
    ("prim.Nat.lt", TFun (TNat, TFun (TNat, TBool)));
    ("prim.Nat.gte", TFun (TNat, TFun (TNat, TBool)));
    ("prim.Nat.gt", TFun (TNat, TFun (TNat, TBool)));
    ("prim.Nat.toString", TFun (TNat, TString));
    ("prim.String.concat", TFun (TString, TFun (TString, TString)));
    ("prim.String.eq", TFun (TString, TFun (TString, TBool)));
    ("prim.String.length", TFun (TString, TNat));
    ("prim.String.slice", TFun (TString, TFun (TNat, TFun (TNat, TString))));
    ("prim.String.charAt", TFun (TString, TFun (TNat, TVariant [ ("None", TUnit); ("Some", TString) ])));
  ]

let builtin_names = List.map fst builtin_types

let is_builtin n = List.exists (String.equal n) builtin_names

let assoc_opt k xs = List.find_opt (fun (k', _) -> String.equal k k') xs |> Option.map snd

let require cond msg = if not cond then fail msg

let option_or_fail msg = function Some x -> x | None -> fail msg

let normalize_process_capabilities = function
  | None -> None
  | Some caps -> Some (List.sort_uniq String.compare caps)

let merge_process_capabilities a b =
  match (normalize_process_capabilities a, normalize_process_capabilities b) with
  | Some a, Some b -> Some (List.sort_uniq String.compare (a @ b))
  | _ -> None

let process_capabilities_to_canonical = function
  | None -> ""
  | Some caps ->
      " (Capabilities"
      ^ (match List.sort_uniq String.compare caps with
        | [] -> ""
        | caps -> " " ^ String.concat " " (List.map Ast.quote caps))
      ^ ")"

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
  | TAttr t -> "(Attr " ^ type_to_canonical t ^ ")"
  | TStream t -> "(Stream " ^ type_to_canonical t ^ ")"
  | TAutomaton (state, output) ->
      "(Automaton " ^ type_to_canonical state ^ " " ^ type_to_canonical output ^ ")"
  | TProcess (caps, t) ->
      "(Process" ^ process_capabilities_to_canonical caps ^ " " ^ type_to_canonical t ^ ")"
  | TCmd (caps, t) ->
      "(Cmd" ^ process_capabilities_to_canonical caps ^ " " ^ type_to_canonical t ^ ")"
  | TSecretRef (scope, t) -> "(SecretRef " ^ scope ^ " " ^ type_to_canonical t ^ ")"
  | TVar i -> "(TVar " ^ string_of_int i ^ ")"
  | TForall (arity, body) ->
      "(Forall " ^ string_of_int arity ^ " " ^ type_to_canonical body ^ ")"
  | TNamed (n, args) ->
      "(Named " ^ n
      ^ (match args with [] -> "" | _ -> " " ^ String.concat " " (List.map type_to_canonical args))
      ^ ")"

let req_capability = function
  | AskHuman _ -> "Human.ask"
  | HttpGet _ -> "Http.get"
  | ReadClock -> "Clock.read"
  | SaveLocal _ -> "Local.storage"
  | LoadLocal _ -> "Local.storage"
  | ServerRequest _ -> "Server.request"

let req_tag = function
  | AskHuman _ -> "AskHuman"
  | HttpGet _ -> "HttpGet"
  | ReadClock -> "ReadClock"
  | SaveLocal _ -> "SaveLocal"
  | LoadLocal _ -> "LoadLocal"
  | ServerRequest _ -> "ServerRequest"

let req_payload_type = function
  | ReadClock -> TUnit
  | AskHuman _ -> TRecord [ ("prompt", TString) ]
  | HttpGet _ -> TRecord [ ("url", TString) ]
  | LoadLocal _ -> TRecord [ ("key", TString) ]
  | SaveLocal _ -> TRecord [ ("key", TString); ("value", TString) ]
  | ServerRequest _ -> TRecord [ ("payload", TString); ("route", TString) ]

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

let capability_request_signature_canonical capability signature =
  "(capability-request-signature-v1 (capability " ^ Ast.quote capability ^ ") (tag "
  ^ Ast.quote signature.request_tag ^ ") (payload-type "
  ^ type_to_canonical signature.request_payload_type ^ ") (response-type "
  ^ type_to_canonical signature.response_type ^ "))"

let capability_request_signature_ref capability signature =
  hash_string (capability_request_signature_canonical capability signature)

let capability_descriptor_canonical descriptor =
  let signatures =
    descriptor.request_signatures
    |> List.map (capability_request_signature_canonical descriptor.capability_name)
    |> List.sort String.compare
  in
  "(capability-descriptor-v1 (name " ^ Ast.quote descriptor.capability_name
  ^ ") (requests " ^ String.concat " " signatures ^ "))"

let capability_descriptor_ref descriptor =
  hash_string (capability_descriptor_canonical descriptor)

let capability_ref name =
  capability_descriptor name |> Option.map capability_descriptor_ref

let capability_scope_canonical caps =
  let caps = List.sort_uniq String.compare caps in
  let capability_item cap =
    match capability_ref cap with
    | Some ref -> "(capability " ^ Ast.quote cap ^ " " ^ ref ^ ")"
    | None -> fail ("unknown capability in capability scope: " ^ cap)
  in
  "(capability-scope-v1 " ^ String.concat " " (List.map capability_item caps) ^ ")"

let capability_scope_ref caps = hash_string (capability_scope_canonical caps)

let req_signature_ref req =
  capability_request_signature_ref (req_capability req)
    {
      request_tag = req_tag req;
      request_payload_type = req_payload_type req;
      response_type = req_result_type req;
    }

let req_capability_ref req = capability_ref (req_capability req)

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
  [
    "Unit";
    "Bool";
    "Nat";
    "String";
    "List";
    "View";
    "Stream";
    "Automaton";
    "Process";
    "Cmd";
    "Record";
    "Variant";
    "Tuple";
    "->";
    "Fun";
  ]

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

let rec direct_self_refs alias guarded = function
  | TUnit | TBool | TNat | TString | TVar _ -> []
  | TFun (a, b) -> direct_self_refs alias false a @ direct_self_refs alias false b
  | TRecord fields -> List.concat_map (fun (_, t) -> direct_self_refs alias guarded t) fields
  | TVariant cases -> List.concat_map (fun (_, t) -> direct_self_refs alias true t) cases
  | TList t | TStream t -> direct_self_refs alias guarded t
  | TAutomaton (state, output) ->
      direct_self_refs alias false state @ direct_self_refs alias false output
  | TView t | TAttr t | TProcess (_, t) | TCmd (_, t) | TSecretRef (_, t) ->
      direct_self_refs alias false t
  | TForall (_, body) -> direct_self_refs alias guarded body
  | TNamed (n, args) ->
      let arg_refs = List.concat_map (direct_self_refs alias guarded) args in
      if String.equal n alias.type_name
         && not (List.exists (String.equal n) alias.type_params)
      then guarded :: arg_refs
      else arg_refs

let validate_type_alias_recursion aliases =
  List.iter
    (fun alias ->
      match direct_self_refs alias false alias.type_body with
      | [] -> ()
      | refs when List.for_all Fun.id refs -> ()
      | _ ->
          fail
            ("recursive type alias must be guarded by a Variant constructor: "
            ^ alias.type_name))
    aliases

let recursive_alias_names aliases =
  aliases
  |> List.filter (fun alias -> direct_self_refs alias false alias.type_body <> [])
  |> List.map (fun alias -> alias.type_name)

let rec expand_type recursive_names aliases vars stack = function
  | TUnit -> TUnit
  | TBool -> TBool
  | TNat -> TNat
  | TString -> TString
  | TFun (a, b) -> TFun (expand_type recursive_names aliases vars stack a, expand_type recursive_names aliases vars stack b)
  | TRecord fields ->
      TRecord
        (sort_fields
           (List.map (fun (n, t) -> (n, expand_type recursive_names aliases vars stack t)) fields))
  | TVariant cases ->
      TVariant
        (sort_fields
           (List.map (fun (n, t) -> (n, expand_type recursive_names aliases vars stack t)) cases))
  | TList t -> TList (expand_type recursive_names aliases vars stack t)
  | TView t -> TView (expand_type recursive_names aliases vars stack t)
  | TAttr t -> TAttr (expand_type recursive_names aliases vars stack t)
  | TStream t -> TStream (expand_type recursive_names aliases vars stack t)
  | TAutomaton (state, output) ->
      TAutomaton
        ( expand_type recursive_names aliases vars stack state,
          expand_type recursive_names aliases vars stack output )
  | TProcess (caps, t) -> TProcess (normalize_process_capabilities caps, expand_type recursive_names aliases vars stack t)
  | TCmd (caps, t) -> TCmd (normalize_process_capabilities caps, expand_type recursive_names aliases vars stack t)
  | TSecretRef (scope, t) -> TSecretRef (scope, expand_type recursive_names aliases vars stack t)
  | TVar i -> TVar i
  | TForall (arity, body) -> TForall (arity, expand_type recursive_names aliases vars stack body)
  | TNamed (n, args) -> (
      let args = List.map (expand_type recursive_names aliases vars stack) args in
      match assoc_opt n vars with
      | Some t ->
          if args <> [] then fail ("type parameter is not a type constructor: " ^ n);
          t
      | None ->
      if List.exists (String.equal n) recursive_names then (
        match alias_by_name aliases n with
        | Some alias ->
            ignore (bind_type_params alias args);
            TNamed (n, args)
        | None -> fail ("unknown recursive type alias: " ^ n))
      else if List.exists (String.equal n) stack then
        fail ("cyclic type alias: " ^ String.concat " -> " (List.rev (n :: stack)))
      else
        match alias_by_name aliases n with
        | Some alias ->
            let vars = bind_type_params alias args in
            expand_type recursive_names aliases vars (n :: stack) alias.type_body
        | None ->
            if List.exists (String.equal n) builtin_type_names then
              fail ("invalid builtin type application: " ^ Ast.string_of_typ (TNamed (n, args)))
            else fail ("unknown type alias: " ^ n))

let type_var_env params =
  List.mapi (fun i param -> (param, TVar i)) params

let rec expand_expr_types recursive_names aliases vars = function
  | EUnit -> EUnit
  | EBool b -> EBool b
  | ENat n -> ENat n
  | EString s -> EString s
  | EName n -> EName n
  | ELambda (x, t, body) ->
      ELambda (x, expand_type recursive_names aliases vars [] t, expand_expr_types recursive_names aliases vars body)
  | ELambdaInfer (x, body) -> ELambdaInfer (x, expand_expr_types recursive_names aliases vars body)
  | EApp (f, x) -> EApp (expand_expr_types recursive_names aliases vars f, expand_expr_types recursive_names aliases vars x)
  | EStrict e -> EStrict (expand_expr_types recursive_names aliases vars e)
  | ELet (x, e, body) ->
      ELet (x, expand_expr_types recursive_names aliases vars e, expand_expr_types recursive_names aliases vars body)
  | ELetAnnot (x, t, e, body) ->
      ELetAnnot
        ( x,
          expand_type recursive_names aliases vars [] t,
          expand_expr_types recursive_names aliases vars e,
          expand_expr_types recursive_names aliases vars body )
  | ELetRecord (record, fields, body) ->
      ELetRecord
        ( expand_expr_types recursive_names aliases vars record,
          fields,
          expand_expr_types recursive_names aliases vars body )
  | ERecord fields ->
      ERecord
        (sort_fields (List.map (fun (n, e) -> (n, expand_expr_types recursive_names aliases vars e)) fields))
  | ERecordUpdate (record, updates) ->
      ERecordUpdate
        ( expand_expr_types recursive_names aliases vars record,
          sort_fields
            (List.map
               (fun (n, e) -> (n, expand_expr_types recursive_names aliases vars e))
               updates) )
  | EField (e, field) -> EField (expand_expr_types recursive_names aliases vars e, field)
  | EVariant (t, con, e) ->
      EVariant (expand_type recursive_names aliases vars [] t, con, expand_expr_types recursive_names aliases vars e)
  | EVariantInferred (con, e) -> EVariantInferred (con, expand_expr_types recursive_names aliases vars e)
  | EInst (name, args) -> EInst (name, List.map (expand_type recursive_names aliases vars []) args)
  | ECase (e, branches) ->
      ECase (expand_expr_types recursive_names aliases vars e, List.map (expand_branch_types recursive_names aliases vars) branches)
  | EFoldNat (n, z, step) ->
      EFoldNat
        ( expand_expr_types recursive_names aliases vars n,
          expand_expr_types recursive_names aliases vars z,
          expand_expr_types recursive_names aliases vars step )
  | EFoldVariant (target, result, scrut, branches) ->
      EFoldVariant
        ( expand_type recursive_names aliases vars [] target,
          expand_type recursive_names aliases vars [] result,
          expand_expr_types recursive_names aliases vars scrut,
          List.map (expand_branch_types recursive_names aliases vars) branches )
  | ERecur e -> ERecur (expand_expr_types recursive_names aliases vars e)
  | ENil t -> ENil (expand_type recursive_names aliases vars [] t)
  | ENilInfer -> ENilInfer
  | ECons (t, head, tail) ->
      ECons
        ( expand_type recursive_names aliases vars [] t,
          expand_expr_types recursive_names aliases vars head,
          expand_expr_types recursive_names aliases vars tail )
  | EConsInfer (head, tail) ->
      EConsInfer
        ( expand_expr_types recursive_names aliases vars head,
          expand_expr_types recursive_names aliases vars tail )
  | EFoldList (xs, z, step) ->
      EFoldList
        ( expand_expr_types recursive_names aliases vars xs,
          expand_expr_types recursive_names aliases vars z,
          expand_expr_types recursive_names aliases vars step )
  | ECaseList (xs, nil_body, head, tail, cons_body) ->
      ECaseList
        ( expand_expr_types recursive_names aliases vars xs,
          expand_expr_types recursive_names aliases vars nil_body,
          head,
          tail,
          expand_expr_types recursive_names aliases vars cons_body )
  | ECoiter (state_ty, item_ty, seed, step) ->
      ECoiter
        ( expand_type recursive_names aliases vars [] state_ty,
          expand_type recursive_names aliases vars [] item_ty,
          expand_expr_types recursive_names aliases vars seed,
          expand_expr_types recursive_names aliases vars step )
  | EStreamHead stream ->
      EStreamHead (expand_expr_types recursive_names aliases vars stream)
  | EStreamTail stream ->
      EStreamTail (expand_expr_types recursive_names aliases vars stream)
  | EStreamTake (count, stream) ->
      EStreamTake
        ( expand_expr_types recursive_names aliases vars count,
          expand_expr_types recursive_names aliases vars stream )
  | EAutomaton (state_ty, output_ty, initial, transition) ->
      EAutomaton
        ( expand_type recursive_names aliases vars [] state_ty,
          expand_type recursive_names aliases vars [] output_ty,
          expand_expr_types recursive_names aliases vars initial,
          expand_expr_types recursive_names aliases vars transition )
  | EAutomatonRun (count, automaton) ->
      EAutomatonRun
        ( expand_expr_types recursive_names aliases vars count,
          expand_expr_types recursive_names aliases vars automaton )
  | EText e -> EText (expand_expr_types recursive_names aliases vars e)
  | EImage (src, alt) ->
      EImage (expand_expr_types recursive_names aliases vars src, expand_expr_types recursive_names aliases vars alt)
  | EButton (label, msg) ->
      EButton (expand_expr_types recursive_names aliases vars label, expand_expr_types recursive_names aliases vars msg)
  | EInput (value, handler) ->
      EInput (expand_expr_types recursive_names aliases vars value, expand_expr_types recursive_names aliases vars handler)
  | EColumn children -> EColumn (expand_expr_types recursive_names aliases vars children)
  | ERow children -> ERow (expand_expr_types recursive_names aliases vars children)
  | EListView (items, render) ->
      EListView (expand_expr_types recursive_names aliases vars items, expand_expr_types recursive_names aliases vars render)
  | EWhenView (cond, view) ->
      EWhenView (expand_expr_types recursive_names aliases vars cond, expand_expr_types recursive_names aliases vars view)
  | ENode (tag, attrs, children) ->
      ENode
        ( expand_expr_types recursive_names aliases vars tag,
          expand_expr_types recursive_names aliases vars attrs,
          expand_expr_types recursive_names aliases vars children )
  | EAttr (name, value) ->
      EAttr (expand_expr_types recursive_names aliases vars name, expand_expr_types recursive_names aliases vars value)
  | EOn (event, msg) ->
      EOn (expand_expr_types recursive_names aliases vars event, expand_expr_types recursive_names aliases vars msg)
  | EDone e -> EDone (expand_expr_types recursive_names aliases vars e)
  | ERequest req -> ERequest req
  | EBind (p, x, t, body) ->
      EBind
        ( expand_expr_types recursive_names aliases vars p,
          x,
          expand_type recursive_names aliases vars [] t,
          expand_expr_types recursive_names aliases vars body )
  | EBindInfer (p, x, body) ->
      EBindInfer
        ( expand_expr_types recursive_names aliases vars p,
          x,
          expand_expr_types recursive_names aliases vars body )

and expand_branch_types recursive_names aliases vars = function
  | BBool (b, e) -> BBool (b, expand_expr_types recursive_names aliases vars e)
  | BVariant (con, x, e) -> BVariant (con, x, expand_expr_types recursive_names aliases vars e)
  | BVariantUnit (con, e) -> BVariantUnit (con, expand_expr_types recursive_names aliases vars e)
  | BWildcard e -> BWildcard (expand_expr_types recursive_names aliases vars e)

let resolve_program_types program =
  check_duplicate_type_aliases program.type_aliases;
  validate_type_alias_recursion program.type_aliases;
  let aliases = program.type_aliases in
  let recursive_names = recursive_alias_names aliases in
  let expanded_aliases =
    List.map
      (fun a ->
        let vars = List.map (fun param -> (param, TNamed (param, []))) a.type_params in
        { a with type_body = expand_type recursive_names aliases vars [ a.type_name ] a.type_body })
      program.type_aliases
  in
  let defs =
    List.map
      (fun (d : def) ->
        let vars = type_var_env d.type_params in
        let typ = expand_type recursive_names aliases vars [] d.typ in
        {
          d with
          typ =
            (match d.type_params with
            | [] -> typ
            | params -> TForall (List.length params, typ));
          body = expand_expr_types recursive_names aliases vars d.body;
        })
      program.defs
  in
  { program with type_aliases = expanded_aliases; defs }

let collect_deps defs =
  let global_names = Hashtbl.create 1024 in
  List.iter (fun d -> Hashtbl.replace global_names d.name ()) defs;
  let is_global n = Hashtbl.mem global_names n in
  (* Per-definition membership set mirroring [acc], so dedup is O(1) instead of
     a linear scan of the accumulated dependencies. *)
  let seen = Hashtbl.create 64 in
  let rec expr bound acc = function
    | EUnit | EBool _ | ENat _ | EString _ | ERequest _ | ENil _ | ENilInfer -> acc
    | EName n ->
        if List.exists (String.equal n) bound || is_builtin n then acc
        else if is_global n && not (Hashtbl.mem seen n) then (
          Hashtbl.add seen n ();
          n :: acc)
        else acc
    | ELambda (x, _, body) | ELambdaInfer (x, body) -> expr (x :: bound) acc body
    | EApp (f, x) -> expr bound (expr bound acc f) x
    | EStrict e -> expr bound acc e
    | ELet (x, e, body) | ELetAnnot (x, _, e, body) ->
        expr (x :: bound) (expr bound acc e) body
    | ELetRecord (record, fields, body) ->
        let binders = List.map snd fields in
        expr (binders @ bound) (expr bound acc record) body
    | ERecord fields -> List.fold_left (fun a (_, e) -> expr bound a e) acc fields
    | ERecordUpdate (record, updates) ->
        List.fold_left (fun a (_, e) -> expr bound a e) (expr bound acc record) updates
    | EField (e, _) -> expr bound acc e
    | EVariant (_, _, e) | EVariantInferred (_, e) -> expr bound acc e
    | EInst (n, _) ->
        if is_global n && not (Hashtbl.mem seen n) then (
          Hashtbl.add seen n ();
          n :: acc)
        else acc
    | ECase (e, branches) ->
        List.fold_left
          (fun a -> function
            | BBool (_, b) -> expr bound a b
            | BVariant (_, x, b) -> expr (x :: bound) a b
            | BVariantUnit (_, b) -> expr bound a b
            | BWildcard b -> expr bound a b)
          (expr bound acc e) branches
    | EFoldNat (n, z, step) -> expr bound (expr bound (expr bound acc n) z) step
    | EFoldVariant (_, _, scrut, branches) ->
        List.fold_left
          (fun a -> function
            | BBool (_, b) -> expr bound a b
            | BVariant (_, x, b) -> expr (x :: bound) a b
            | BVariantUnit (_, b) -> expr bound a b
            | BWildcard b -> expr bound a b)
          (expr bound acc scrut) branches
    | ERecur e -> expr bound acc e
    | ECons (_, head, tail) | EConsInfer (head, tail) ->
        expr bound (expr bound acc head) tail
    | EFoldList (xs, z, step) -> expr bound (expr bound (expr bound acc xs) z) step
    | ECaseList (xs, nil_body, head, tail, cons_body) ->
        expr (head :: tail :: bound) (expr bound (expr bound acc xs) nil_body) cons_body
    | ECoiter (_, _, seed, step) -> expr bound (expr bound acc seed) step
    | EStreamHead stream | EStreamTail stream -> expr bound acc stream
    | EStreamTake (count, stream) -> expr bound (expr bound acc count) stream
    | EAutomaton (_, _, initial, transition) ->
        expr bound (expr bound acc initial) transition
    | EAutomatonRun (count, automaton) -> expr bound (expr bound acc count) automaton
    | EText e | EColumn e | ERow e -> expr bound acc e
    | EButton (label, msg) | EInput (label, msg) | EImage (label, msg)
    | EListView (label, msg) | EWhenView (label, msg) | EAttr (label, msg) | EOn (label, msg) ->
        expr bound (expr bound acc label) msg
    | ENode (tag, attrs, children) ->
        expr bound (expr bound (expr bound acc tag) attrs) children
    | EDone e -> expr bound acc e
    | EBind (p, x, _, body) | EBindInfer (p, x, body) ->
        expr (x :: bound) (expr bound acc p) body
  in
  List.map
    (fun d ->
      Hashtbl.reset seen;
      (d.name, expr [] [] d.body))
    defs

(* [dependencies_of_defs] is called once per definition by loaders, builds,
   diffs and audits; recomputing the whole dependency table on each call is
   quadratic in program size. Tables are memoized by physical identity of the
   definition list, keeping a few recent programs (diff compares two). *)
let deps_table_memo : (def list * (string, string list) Hashtbl.t) list ref = ref []

let deps_table defs =
  match List.find_opt (fun (d, _) -> d == defs) !deps_table_memo with
  | Some (_, table) -> table
  | None ->
      let table = Hashtbl.create 256 in
      List.iter
        (fun (name, deps) -> if not (Hashtbl.mem table name) then Hashtbl.add table name deps)
        (collect_deps defs);
      deps_table_memo := (defs, table) :: List.filteri (fun i _ -> i < 3) !deps_table_memo;
      table

let dependencies_of_defs defs name =
  Option.value (Hashtbl.find_opt (deps_table defs) name) ~default:[]

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

type fold_scope = {
  fold_target : typ;
  fold_result : typ;
  fold_allowed : expr list;
  fold_list_allowed : expr list;
}

type type_ctx = {
  type_aliases : type_alias list;
  globals : (string * global_type) list;
  capabilities : string list;
  locals : (string * typ) list;
  fold_scope : fold_scope option;
}

let rec recur_root_name = function
  | EName n -> Some n
  | EField (e, _) -> recur_root_name e
  | _ -> None

let shadow_fold_scope scope name =
  match scope with
  | None -> None
  | Some s ->
      Some
        {
          s with
          fold_allowed =
            List.filter
              (fun e ->
                match recur_root_name e with
                | Some root -> not (String.equal root name)
                | None -> true)
              s.fold_allowed;
          fold_list_allowed =
            List.filter
              (fun e ->
                match recur_root_name e with
                | Some root -> not (String.equal root name)
                | None -> true)
              s.fold_list_allowed;
        }

let bind_local ctx name typ =
  { ctx with locals = (name, typ) :: ctx.locals; fold_scope = shadow_fold_scope ctx.fold_scope name }

let bind_lambda ctx name typ = { ctx with locals = (name, typ) :: ctx.locals; fold_scope = None }

let rec expr_names = function
  | EUnit | EBool _ | ENat _ | EString _ | ERequest _ | ENilInfer -> []
  | EName n -> [ n ]
  | ELambda (x, _, body) | ELambdaInfer (x, body) -> x :: expr_names body
  | EApp (f, x) -> expr_names f @ expr_names x
  | EStrict e -> expr_names e
  | ELet (x, e, body) -> x :: expr_names e @ expr_names body
  | ELetAnnot (x, _, e, body) -> x :: expr_names e @ expr_names body
  | ELetRecord (record, fields, body) ->
      expr_names record @ List.map snd fields @ expr_names body
  | ERecord fields -> List.concat_map (fun (_, e) -> expr_names e) fields
  | ERecordUpdate (record, updates) ->
      expr_names record @ List.concat_map (fun (_, e) -> expr_names e) updates
  | EField (e, _) -> expr_names e
  | EVariant (_, _, e) | EVariantInferred (_, e) -> expr_names e
  | EInst (name, _) -> [ name ]
  | ECase (e, branches) -> expr_names e @ List.concat_map branch_names_in_expr branches
  | EFoldNat (n, z, step) -> expr_names n @ expr_names z @ expr_names step
  | EFoldVariant (_, _, scrut, branches) ->
      expr_names scrut @ List.concat_map branch_names_in_expr branches
  | ERecur e -> expr_names e
  | ENil _ -> []
  | ECons (_, head, tail) | EConsInfer (head, tail) -> expr_names head @ expr_names tail
  | EFoldList (xs, z, step) -> expr_names xs @ expr_names z @ expr_names step
  | ECaseList (xs, nil_body, head, tail, cons_body) ->
      expr_names xs @ expr_names nil_body @ [ head; tail ] @ expr_names cons_body
  | ECoiter (_, _, seed, step) -> expr_names seed @ expr_names step
  | EStreamHead stream | EStreamTail stream -> expr_names stream
  | EStreamTake (count, stream) -> expr_names count @ expr_names stream
  | EAutomaton (_, _, initial, transition) ->
      expr_names initial @ expr_names transition
  | EAutomatonRun (count, automaton) -> expr_names count @ expr_names automaton
  | EText e | EColumn e | ERow e | EDone e -> expr_names e
  | EImage (a, b) | EButton (a, b) | EInput (a, b) | EListView (a, b)
  | EWhenView (a, b) | EAttr (a, b) | EOn (a, b) ->
      expr_names a @ expr_names b
  | ENode (tag, attrs, children) -> expr_names tag @ expr_names attrs @ expr_names children
  | EBind (p, x, _, body) | EBindInfer (p, x, body) -> expr_names p @ (x :: expr_names body)

and branch_names_in_expr = function
  | BBool (_, e) -> expr_names e
  | BVariant (_, x, e) -> x :: expr_names e
  | BVariantUnit (_, e) -> expr_names e
  | BWildcard e -> expr_names e

let fresh_record_name ctx record fields body =
  let taken =
    List.map fst ctx.locals @ List.map fst ctx.globals @ expr_names record
    @ List.map snd fields @ expr_names body
    |> List.sort_uniq String.compare
  in
  let rec loop i =
    let candidate = "__record" ^ string_of_int i in
    if List.exists (String.equal candidate) taken then loop (i + 1) else candidate
  in
  loop 0

let desugar_let_record ctx record fields body =
  let record_name = fresh_record_name ctx record fields body in
  let body =
    List.fold_right
      (fun (field, binder) body -> ELet (binder, EField (EName record_name, field), body))
      fields body
  in
  ELet (record_name, record, body)

let fresh_record_update_name ctx record updates =
  let taken =
    List.map fst ctx.locals @ List.map fst ctx.globals @ expr_names record
    @ List.concat_map (fun (_, e) -> expr_names e) updates
    |> List.sort_uniq String.compare
  in
  let rec loop i =
    let candidate = "__record" ^ string_of_int i in
    if List.exists (String.equal candidate) taken then loop (i + 1) else candidate
  in
  loop 0

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
  | TAttr t -> TAttr (subst_type args t)
  | TStream t -> TStream (subst_type args t)
  | TAutomaton (state, output) -> TAutomaton (subst_type args state, subst_type args output)
  | TProcess (caps, t) -> TProcess (normalize_process_capabilities caps, subst_type args t)
  | TCmd (caps, t) -> TCmd (normalize_process_capabilities caps, subst_type args t)
  | TSecretRef (scope, t) -> TSecretRef (scope, subst_type args t)
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

let type_body = function TForall (_, body) -> body | t -> t

(* When set, every global type lookup made by the elaborator is recorded
   (name -> looked-up binding, including absent ones). The per-definition
   check cache uses the recorded trace as the dynamic part of its key: a
   cached elaboration is reusable under a new program iff every recorded
   lookup yields the same binding there. Lookups whose result is unused are
   recorded too - a sound over-approximation. *)
let global_lookup_trace : (string, global_type option) Hashtbl.t option ref = ref None

let lookup_global ctx n =
  let r = assoc_opt n ctx.globals in
  (match !global_lookup_trace with
  | Some tbl -> if not (Hashtbl.mem tbl n) then Hashtbl.add tbl n r
  | None -> ());
  r

let rec subst_named_type_params vars = function
  | TUnit -> TUnit
  | TBool -> TBool
  | TNat -> TNat
  | TString -> TString
  | TFun (a, b) -> TFun (subst_named_type_params vars a, subst_named_type_params vars b)
  | TRecord fields ->
      TRecord (sort_fields (List.map (fun (n, t) -> (n, subst_named_type_params vars t)) fields))
  | TVariant cases ->
      TVariant (sort_fields (List.map (fun (n, t) -> (n, subst_named_type_params vars t)) cases))
  | TList t -> TList (subst_named_type_params vars t)
  | TView t -> TView (subst_named_type_params vars t)
  | TAttr t -> TAttr (subst_named_type_params vars t)
  | TStream t -> TStream (subst_named_type_params vars t)
  | TAutomaton (state, output) ->
      TAutomaton (subst_named_type_params vars state, subst_named_type_params vars output)
  | TProcess (caps, t) -> TProcess (normalize_process_capabilities caps, subst_named_type_params vars t)
  | TCmd (caps, t) -> TCmd (normalize_process_capabilities caps, subst_named_type_params vars t)
  | TSecretRef (scope, t) -> TSecretRef (scope, subst_named_type_params vars t)
  | TVar i -> TVar i
  | TForall (arity, body) -> TForall (arity, subst_named_type_params vars body)
  | TNamed (n, []) -> Option.value (assoc_opt n vars) ~default:(TNamed (n, []))
  | TNamed (n, args) -> TNamed (n, List.map (subst_named_type_params vars) args)

let unfold_type ctx = function
  | TNamed (n, args) -> (
      match alias_by_name ctx.type_aliases n with
      | None -> TNamed (n, args)
      | Some alias ->
          let vars = bind_type_params alias args in
          subst_named_type_params vars alias.type_body)
  | t -> t

let rec expr_equal a b =
  match (a, b) with
  | EName a, EName b -> String.equal a b
  | EField (a, fa), EField (b, fb) -> String.equal fa fb && expr_equal a b
  | _ -> false

let rec direct_recur_terms_for_value ctx target value value_ty =
  let base = if equal_typ target value_ty then [ value ] else [] in
  match unfold_type ctx value_ty with
  | TRecord fields ->
      base
      @ (fields
        |> List.concat_map (fun (field, field_ty) ->
               direct_recur_terms_for_value ctx target (EField (value, field)) field_ty))
  | _ -> base

let direct_recur_terms ctx target payload_name payload_ty =
  direct_recur_terms_for_value ctx target (EName payload_name) payload_ty

let has_direct_recur_terms ctx target item_ty =
  direct_recur_terms_for_value ctx target (EName "__item") item_ty <> []

let direct_recur_list_terms ctx target payload_name payload_ty =
  let base =
    match unfold_type ctx payload_ty with
    | TList item_ty when has_direct_recur_terms ctx target item_ty -> [ EName payload_name ]
    | _ -> []
  in
  match unfold_type ctx payload_ty with
  | TRecord fields ->
      base
      @ (fields
        |> List.filter_map (fun (field, field_ty) ->
               match unfold_type ctx field_ty with
               | TList item_ty when has_direct_recur_terms ctx target item_ty ->
                   Some (EField (EName payload_name, field))
               | _ -> None))
  | _ -> base

let fresh_wildcard_payload_name body =
  let taken = expr_names body |> List.sort_uniq String.compare in
  let rec loop i =
    let candidate = "__wildcard_payload" ^ string_of_int i in
    if List.exists (String.equal candidate) taken then loop (i + 1) else candidate
  in
  loop 0

let expand_variant_case_branches cases branches where =
  let case_names = List.map fst cases in
  let seen = Hashtbl.create 8 in
  let wildcard = ref None in
  let explicit = ref [] in
  List.iter
    (function
      | BBool _ -> fail "Bool branch in Variant case"
      | BWildcard body ->
          if Option.is_some !wildcard then fail (where ^ " duplicate wildcard branch");
          wildcard := Some body
      | (BVariant (con, _, _) | BVariantUnit (con, _)) as branch ->
          if Hashtbl.mem seen con then fail (where ^ " duplicate branch: " ^ con);
          if not (List.exists (String.equal con) case_names) then
            fail (where ^ " unknown branch: " ^ con);
          Hashtbl.add seen con ();
          explicit := branch :: !explicit)
    branches;
  let missing = List.filter (fun con -> not (Hashtbl.mem seen con)) case_names in
  let generated =
    match (missing, !wildcard) with
    | [], Some _ -> fail (where ^ " wildcard branch is unreachable")
    | [], None -> []
    | con :: _, None -> fail (where ^ " missing branch: " ^ con)
    | missing, Some body ->
        List.map
          (fun con -> BVariant (con, fresh_wildcard_payload_name body, body))
          missing
  in
  List.rev !explicit @ generated

let bool_branch_name = function true -> "true" | false -> "false"

let bool_case_branches branches =
  let true_branch = ref None and false_branch = ref None in
  let wildcard = ref None in
  List.iter
    (function
      | BBool (value, e) ->
          let target = if value then true_branch else false_branch in
          if Option.is_some !target then
            fail ("Bool case duplicate branch: " ^ bool_branch_name value);
          target := Some e
      | BWildcard e ->
          if Option.is_some !wildcard then fail "Bool case duplicate wildcard branch";
          wildcard := Some e
      | BVariant _ | BVariantUnit _ -> fail "variant branch in Bool case")
    branches;
  if Option.is_some !wildcard && Option.is_some !true_branch && Option.is_some !false_branch then
    fail "Bool case wildcard branch is unreachable";
  let wildcard_or_missing branch name =
    match (branch, !wildcard) with
    | Some e, _ -> e
    | None, Some e -> e
    | None, None -> fail ("Bool case missing " ^ name ^ " branch")
  in
  let t_expr = wildcard_or_missing !true_branch "true" in
  let f_expr = wildcard_or_missing !false_branch "false" in
  (t_expr, f_expr)

let recur_scope ctx =
  match ctx.fold_scope with
  | Some scope -> scope
  | None -> fail "recur outside foldVariant"

let require_unit_branch_payload con payload_ty =
  if not (equal_typ TUnit payload_ty) then
    fail
      ("variant branch " ^ con
     ^ " omits payload binder but constructor payload is " ^ string_of_typ payload_ty)

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

let require_type_expr expected actual where expr =
  if not (equal_typ expected actual) then
    fail
      (where ^ ": expected " ^ string_of_typ expected ^ ", got " ^ string_of_typ actual
     ^ ", expression " ^ string_of_expr expr)

let has_capability ctx cap = List.exists (String.equal cap) ctx.capabilities

let rec contains_process_type = function
  | TProcess _ -> true
  | TFun (a, b) -> contains_process_type a || contains_process_type b
  | TRecord fields | TVariant fields -> List.exists (fun (_, t) -> contains_process_type t) fields
  | TList t | TView t | TAttr t | TStream t | TCmd (_, t) | TSecretRef (_, t) ->
      contains_process_type t
  | TAutomaton (state, output) -> contains_process_type state || contains_process_type output
  | TForall (_, t) -> contains_process_type t
  | TNamed _ -> false
  | TVar _ | TUnit | TBool | TNat | TString -> false

let is_process_type = function TProcess _ -> true | _ -> false

let require_process_capabilities expected actual context =
  if not (equal_process_capabilities expected actual) then
    let show = function
      | None -> "unspecified"
      | Some [] -> "{}"
      | Some caps -> "{" ^ String.concat ", " (List.sort_uniq String.compare caps) ^ "}"
    in
    fail
      (context ^ ": expected Process capabilities " ^ show expected ^ ", got "
     ^ show actual)

let fold_branch_ctx ctx target result payload_name payload_ty =
  {
    ctx with
    locals = (payload_name, payload_ty) :: ctx.locals;
    fold_scope =
      Some
        {
          fold_target = target;
          fold_result = result;
          fold_allowed = direct_recur_terms ctx target payload_name payload_ty;
          fold_list_allowed = direct_recur_list_terms ctx target payload_name payload_ty;
        };
  }

let structural_list_recur_allowed ctx xs item_ty =
  match ctx.fold_scope with
  | Some s ->
      has_direct_recur_terms ctx s.fold_target item_ty
      && List.exists (expr_equal xs) s.fold_list_allowed
  | None -> false

let bind_recur_item ctx name typ =
  let scope =
    match shadow_fold_scope ctx.fold_scope name with
    | None -> None
    | Some s ->
        Some
          {
            s with
            fold_allowed =
              direct_recur_terms_for_value ctx s.fold_target (EName name) typ
              @ s.fold_allowed;
          }
  in
  { ctx with locals = (name, typ) :: ctx.locals; fold_scope = scope }

let stream_step_type state_ty item_ty =
  TFun (state_ty, TRecord [ ("head", item_ty); ("state", state_ty) ])

let automaton_transition_type state_ty output_ty =
  TFun (state_ty, TRecord [ ("output", output_ty); ("state", state_ty) ])

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
      TFun (t, infer (bind_lambda ctx x t) body)
  | ELambdaInfer _ -> fail "unannotated lambda requires an expected function type"
  | EApp (f, arg) -> (
      match infer ctx f with
      | TFun (a, b) ->
          let at = infer ctx arg in
          require_type_expr a at "application" arg;
          b
      | t -> fail ("application of non-function: " ^ string_of_typ t))
  | EStrict e -> infer ctx e
  | ELet (x, e, body) ->
      let t = infer ctx e in
      let body_ty = infer (bind_local ctx x t) body in
      if is_process_type t && not (is_process_type body_ty) then
        fail
          ("Process used as pure value in let " ^ x ^ ": expected Process flow, got "
         ^ string_of_typ body_ty ^ ", expression " ^ string_of_expr body);
      body_ty
  | ELetAnnot (x, annotation, e, body) ->
      let actual = infer ctx e in
      require_type annotation actual "let annotation";
      let body_ty = infer (bind_local ctx x annotation) body in
      if is_process_type annotation && not (is_process_type body_ty) then
        fail
          ("Process used as pure value in let " ^ x ^ ": expected Process flow, got "
         ^ string_of_typ body_ty ^ ", expression " ^ string_of_expr body);
      body_ty
  | ELetRecord (record, fields, body) -> infer ctx (desugar_let_record ctx record fields body)
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
  | ERecordUpdate (record, updates) -> infer_record_update ctx record updates
  | EField (e, field) -> (
      match unfold_type ctx (infer ctx e) with
      | TRecord fields -> (
          match assoc_opt field fields with
          | Some t -> t
          | None -> fail ("unknown record field: " ^ field))
      | t -> fail ("field access on non-record: " ^ string_of_typ t))
  | EVariant (ty, con, e) -> (
      match unfold_type ctx ty with
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
      match unfold_type ctx (infer ctx scrut) with
      | TBool -> infer_bool_case ctx branches
      | TVariant cases -> infer_variant_case ctx cases branches
      | t -> fail ("case on unsupported type: " ^ string_of_typ t))
  | EFoldNat (n, zero, step) ->
      require_type TNat (infer ctx n) "foldNat index";
      let result_ty = infer ctx zero in
      require_type (TFun (result_ty, result_ty)) (infer ctx step) "foldNat step";
      result_ty
  | EFoldVariant (target, result, scrut, branches) ->
      require_type target (infer ctx scrut) "foldVariant scrutinee";
      (match unfold_type ctx target with
      | TVariant cases ->
          let branches = expand_variant_case_branches cases branches "foldVariant" in
          List.iter
            (function
              | BBool _ -> assert false
              | BVariant (con, x, body) ->
                  let payload_ty = Option.get (assoc_opt con cases) in
                  let body_ty =
                    infer (fold_branch_ctx ctx target result x payload_ty) body
                  in
                  require_type result body_ty ("foldVariant branch " ^ con)
              | BVariantUnit (con, body) ->
                  let payload_ty = Option.get (assoc_opt con cases) in
                  require_unit_branch_payload con payload_ty;
                  let body_ty = infer ctx body in
                  require_type result body_ty ("foldVariant branch " ^ con)
              | BWildcard _ -> assert false)
            branches;
          result
      | t -> fail ("foldVariant target must be Variant, got " ^ string_of_typ t))
  | ERecur arg ->
      let scope = recur_scope ctx in
      if not (List.exists (expr_equal arg) scope.fold_allowed) then
        fail ("recur argument is not a direct structural subterm: " ^ string_of_expr arg);
      let actual = infer { ctx with fold_scope = None } arg in
      require_type scope.fold_target actual "recur argument";
      scope.fold_result
  | ENil t -> TList t
  | ENilInfer -> fail "Nil requires an expected List type"
  | ECons (t, head, tail) ->
      require_type t (infer ctx head) "Cons head";
      require_type (TList t) (infer ctx tail) "Cons tail";
      TList t
  | EConsInfer (head, tail) -> (
      match infer ctx tail with
      | TList item_ty ->
          require_type item_ty (infer ctx head) "Cons head";
          TList item_ty
      | t -> fail ("Cons tail must be List, got " ^ string_of_typ t))
  | EFoldList (xs, zero, step) -> (
      match infer ctx xs with
      | TList item_ty ->
          let result_ty = infer ctx zero in
          infer_fold_list_step ctx xs item_ty result_ty step;
          result_ty
      | t -> fail ("foldList target must be List, got " ^ string_of_typ t))
  | ECaseList (xs, nil_body, head, tail, cons_body) -> (
      match infer ctx xs with
      | TList item_ty ->
          let result_ty = infer ctx nil_body in
          let branch_ctx = bind_local (bind_local ctx tail (TList item_ty)) head item_ty in
          require_type result_ty (infer branch_ctx cons_body) "caseList branches";
          result_ty
      | t -> fail ("caseList target must be List, got " ^ string_of_typ t))
  | ECoiter (state_ty, item_ty, seed, step) ->
      require_type state_ty (infer ctx seed) "coiter seed";
      require_type (stream_step_type state_ty item_ty) (infer ctx step) "coiter step";
      TStream item_ty
  | EStreamHead stream -> (
      match infer ctx stream with
      | TStream item_ty -> item_ty
      | t -> fail ("streamHead target must be Stream, got " ^ string_of_typ t))
  | EStreamTail stream -> (
      match infer ctx stream with
      | TStream item_ty -> TStream item_ty
      | t -> fail ("streamTail target must be Stream, got " ^ string_of_typ t))
  | EStreamTake (count, stream) -> (
      require_type TNat (infer ctx count) "streamTake count";
      match infer ctx stream with
      | TStream item_ty -> TList item_ty
      | t -> fail ("streamTake target must be Stream, got " ^ string_of_typ t))
  | EAutomaton (state_ty, output_ty, initial, transition) ->
      require_type state_ty (infer ctx initial) "automaton initial";
      require_type
        (automaton_transition_type state_ty output_ty)
        (infer ctx transition) "automaton transition";
      TAutomaton (state_ty, output_ty)
  | EAutomatonRun (count, automaton) -> (
      require_type TNat (infer ctx count) "automatonRun count";
      match infer ctx automaton with
      | TAutomaton (_, output_ty) -> TList output_ty
      | t -> fail ("automatonRun target must be Automaton, got " ^ string_of_typ t))
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
  | ENode (tag, attrs, children) -> (
      require_type TString (infer ctx tag) "node tag";
      match (infer ctx attrs, infer ctx children) with
      | TList (TAttr attr_msg), TList (TView view_msg) ->
          require_type (TAttr attr_msg) (TAttr view_msg) "node attributes message";
          TView view_msg
      | TList (TAttr _), t -> fail ("node children must be List (View msg), got " ^ string_of_typ t)
      | t, _ -> fail ("node attributes must be List (Attr msg), got " ^ string_of_typ t))
  | EAttr (name, value) ->
      require_type TString (infer ctx name) "attr name";
      require_type TString (infer ctx value) "attr value";
      TAttr TUnit
  | EOn (event, msg) ->
      require_type TString (infer ctx event) "on event";
      TAttr (infer ctx msg)
  | EDone e -> TProcess (Some [], infer ctx e)
  | ERequest req ->
      let cap = req_capability req in
      if not (has_capability ctx cap) then fail ("missing capability: " ^ cap);
      TProcess (Some [ cap ], req_result_type req)
  | EBind (p, x, annotation, body) -> (
      match infer ctx p with
      | TProcess (process_caps, a) ->
          require_type a annotation "bind annotation";
          let body_ty = infer (bind_local ctx x a) body in
          (match body_ty with
          | TProcess (body_caps, value_ty) ->
              TProcess (merge_process_capabilities process_caps body_caps, value_ty)
          | t -> fail ("bind body must return Process, got " ^ string_of_typ t))
      | t -> fail ("bind on non-process: " ^ string_of_typ t))
  | EBindInfer (p, x, body) -> (
      match infer ctx p with
      | TProcess (process_caps, a) ->
          let body_ty = infer (bind_local ctx x a) body in
          (match body_ty with
          | TProcess (body_caps, value_ty) ->
              TProcess (merge_process_capabilities process_caps body_caps, value_ty)
          | t -> fail ("bind body must return Process, got " ^ string_of_typ t))
      | t -> fail ("bind on non-process: " ^ string_of_typ t))

and infer_record_update ctx record updates =
  if updates = [] then fail "recordUpdate requires at least one field";
  let record_ty = infer ctx record in
  match unfold_type ctx record_ty with
  | TRecord fields ->
      List.iter
        (fun (name, expr) ->
          match assoc_opt name fields with
          | None -> fail ("unknown record update field: " ^ name)
          | Some expected ->
              let actual = infer ctx expr in
              require_type_expr expected actual ("record update " ^ name) expr;
              if contains_process_type actual then
                fail
                  ("Process used as pure record update field " ^ name ^ ": got "
                 ^ string_of_typ actual ^ ", expression " ^ string_of_expr expr))
        updates;
      record_ty
  | t -> fail ("record update on non-record: " ^ string_of_typ t)

and infer_fold_list_step ctx xs item_ty result_ty step =
  let expected = TFun (item_ty, TFun (result_ty, result_ty)) in
  let infer_structural_step item actual_item_ty acc actual_acc_ty body =
    require_type item_ty actual_item_ty "foldList step item";
    require_type result_ty actual_acc_ty "foldList step accumulator";
    let item_ctx = bind_recur_item ctx item actual_item_ty in
    let acc_ctx = bind_local item_ctx acc actual_acc_ty in
    require_type result_ty (infer acc_ctx body) "foldList structural step"
  in
  if structural_list_recur_allowed ctx xs item_ty then
    match step with
    | ELambda (item, actual_item_ty, ELambda (acc, actual_acc_ty, body)) ->
        infer_structural_step item actual_item_ty acc actual_acc_ty body
    | ELambda (item, actual_item_ty, ELambdaInfer (acc, body)) ->
        infer_structural_step item actual_item_ty acc result_ty body
    | ELambdaInfer (item, ELambda (acc, actual_acc_ty, body)) ->
        infer_structural_step item item_ty acc actual_acc_ty body
    | ELambdaInfer (item, ELambdaInfer (acc, body)) ->
        infer_structural_step item item_ty acc result_ty body
    | _ -> require_type expected (infer ctx step) "foldList step"
  else require_type expected (infer ctx step) "foldList step"

and infer_bool_case ctx branches =
  let t_expr, f_expr = bool_case_branches branches in
  let ty = infer ctx t_expr in
  require_type ty (infer ctx f_expr) "Bool case branches";
  ty

and infer_variant_case ctx cases branches =
  let branches = expand_variant_case_branches cases branches "Variant case" in
  let result = ref None in
  List.iter
    (function
      | BBool _ -> assert false
      | BVariant (con, x, body) ->
          let payload_ty = Option.get (assoc_opt con cases) in
          let ty = infer (bind_local ctx x payload_ty) body in
          (match !result with
          | None -> result := Some ty
          | Some expected -> require_type expected ty "Variant case branches")
      | BVariantUnit (con, body) ->
          let payload_ty = Option.get (assoc_opt con cases) in
          require_unit_branch_payload con payload_ty;
          let ty = infer ctx body in
          (match !result with
          | None -> result := Some ty
          | Some expected -> require_type expected ty "Variant case branches")
      | BWildcard _ -> assert false)
    branches;
  option_or_fail "empty Variant case" !result

let app_spine expr =
  let rec loop args = function
    | EApp (f, arg) -> loop (arg :: args) f
    | e -> (e, args)
  in
  loop [] expr

let rec infer_elab ctx expr =
  match expr with
  | EUnit | EBool _ | ENat _ | EString _ | EName _ | ERequest _ | ENil _ ->
      (infer ctx expr, expr)
  | ENilInfer -> fail "Nil requires an expected List type"
  | ELambda (x, t, body) ->
      let body_ty, body = infer_elab (bind_lambda ctx x t) body in
      (TFun (t, body_ty), ELambda (x, t, body))
  | ELambdaInfer _ -> fail "unannotated lambda requires an expected function type"
  | EApp (f, arg) -> (
      match poly_app_elab ctx None expr with
      | Some r -> r
      | None ->
          let fn_ty, f = infer_elab ctx f in
          (match fn_ty with
          | TFun (a, b) ->
              let _, arg = check_elab ctx a arg in
              (b, EApp (f, arg))
          | t -> fail ("application of non-function: " ^ string_of_typ t)))
  | EStrict e ->
      let typ, e = infer_elab ctx e in
      (typ, EStrict e)
  | ELet (x, e, body) ->
      let t, e = infer_elab ctx e in
      let body_ty, body = infer_elab (bind_local ctx x t) body in
      if is_process_type t && not (is_process_type body_ty) then
        fail
          ("Process used as pure value in let " ^ x ^ ": expected Process flow, got "
         ^ string_of_typ body_ty ^ ", expression " ^ string_of_expr body);
      (body_ty, ELet (x, e, body))
  | ELetAnnot (x, annotation, e, body) ->
      let _, e = check_elab ctx annotation e in
      let body_ty, body = infer_elab (bind_local ctx x annotation) body in
      if is_process_type annotation && not (is_process_type body_ty) then
        fail
          ("Process used as pure value in let " ^ x ^ ": expected Process flow, got "
         ^ string_of_typ body_ty ^ ", expression " ^ string_of_expr body);
      (body_ty, ELet (x, e, body))
  | ELetRecord (record, fields, body) -> infer_elab ctx (desugar_let_record ctx record fields body)
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
  | ERecordUpdate (record, updates) -> elaborate_record_update ctx record updates
  | EField (e, field) -> (
      let t, e = infer_elab ctx e in
      match unfold_type ctx t with
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
      match unfold_type ctx scrut_ty with
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
  | EFoldVariant (target, result, scrut, branches) ->
      let _, scrut = check_elab ctx target scrut in
      let branches =
        match unfold_type ctx target with
        | TVariant cases ->
            let branches = expand_variant_case_branches cases branches "foldVariant" in
            List.map
              (function
                | BBool _ -> assert false
                | BVariant (con, x, body) ->
                    let payload_ty = Option.get (assoc_opt con cases) in
                    let _, body =
                      check_elab (fold_branch_ctx ctx target result x payload_ty) result body
                    in
                    BVariant (con, x, body)
                | BVariantUnit (con, body) ->
                    let payload_ty = Option.get (assoc_opt con cases) in
                    require_unit_branch_payload con payload_ty;
                    let _, body = check_elab ctx result body in
                    BVariant (con, "_", body)
                | BWildcard _ -> assert false)
              branches
        | t -> fail ("foldVariant target must be Variant, got " ^ string_of_typ t)
      in
      (result, EFoldVariant (target, result, scrut, branches))
  | ERecur arg ->
      let scope = recur_scope ctx in
      if not (List.exists (expr_equal arg) scope.fold_allowed) then
        fail ("recur argument is not a direct structural subterm: " ^ string_of_expr arg);
      let _, arg = check_elab { ctx with fold_scope = None } scope.fold_target arg in
      (scope.fold_result, ERecur arg)
  | ECons (t, head, tail) ->
      let _, head = check_elab ctx t head in
      let _, tail = check_elab ctx (TList t) tail in
      (TList t, ECons (t, head, tail))
  | EConsInfer (head, tail) -> (
      let tail_ty, tail = infer_elab ctx tail in
      match tail_ty with
      | TList item_ty ->
          let _, head = check_elab ctx item_ty head in
          (tail_ty, ECons (item_ty, head, tail))
      | t -> fail ("Cons tail must be List, got " ^ string_of_typ t))
  | EFoldList (xs, zero, step) -> (
      let xs_ty, xs = infer_elab ctx xs in
      match xs_ty with
      | TList item_ty ->
          let result_ty, zero = infer_elab ctx zero in
          let _, step = check_fold_list_step_elab ctx xs item_ty result_ty step in
          (result_ty, EFoldList (xs, zero, step))
      | t -> fail ("foldList target must be List, got " ^ string_of_typ t))
  | ECaseList (xs, nil_body, head, tail, cons_body) -> (
      let xs_ty, xs = infer_elab ctx xs in
      match xs_ty with
      | TList item_ty ->
          let result_ty, nil_body = infer_elab ctx nil_body in
          let branch_ctx = bind_local (bind_local ctx tail (TList item_ty)) head item_ty in
          let _, cons_body = check_elab branch_ctx result_ty cons_body in
          (result_ty, ECaseList (xs, nil_body, head, tail, cons_body))
      | t -> fail ("caseList target must be List, got " ^ string_of_typ t))
  | ECoiter (state_ty, item_ty, seed, step) ->
      let _, seed = check_elab ctx state_ty seed in
      let _, step = check_elab ctx (stream_step_type state_ty item_ty) step in
      (TStream item_ty, ECoiter (state_ty, item_ty, seed, step))
  | EStreamHead stream -> (
      let stream_ty, stream = infer_elab ctx stream in
      match stream_ty with
      | TStream item_ty -> (item_ty, EStreamHead stream)
      | t -> fail ("streamHead target must be Stream, got " ^ string_of_typ t))
  | EStreamTail stream -> (
      let stream_ty, stream = infer_elab ctx stream in
      match stream_ty with
      | TStream item_ty -> (TStream item_ty, EStreamTail stream)
      | t -> fail ("streamTail target must be Stream, got " ^ string_of_typ t))
  | EStreamTake (count, stream) -> (
      let _, count = check_elab ctx TNat count in
      let stream_ty, stream = infer_elab ctx stream in
      match stream_ty with
      | TStream item_ty -> (TList item_ty, EStreamTake (count, stream))
      | t -> fail ("streamTake target must be Stream, got " ^ string_of_typ t))
  | EAutomaton (state_ty, output_ty, initial, transition) ->
      let _, initial = check_elab ctx state_ty initial in
      let _, transition =
        check_elab ctx (automaton_transition_type state_ty output_ty) transition
      in
      (TAutomaton (state_ty, output_ty), EAutomaton (state_ty, output_ty, initial, transition))
  | EAutomatonRun (count, automaton) -> (
      let _, count = check_elab ctx TNat count in
      let automaton_ty, automaton = infer_elab ctx automaton in
      match automaton_ty with
      | TAutomaton (_, output_ty) -> (TList output_ty, EAutomatonRun (count, automaton))
      | t -> fail ("automatonRun target must be Automaton, got " ^ string_of_typ t))
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
  | ENode (tag, attrs, children) -> (
      let _, tag = check_elab ctx TString tag in
      let children_ty, children = infer_elab ctx children in
      match children_ty with
      | TList (TView msg_ty) ->
          let _, attrs = check_elab ctx (TList (TAttr msg_ty)) attrs in
          (TView msg_ty, ENode (tag, attrs, children))
      | t -> fail ("node children must be List (View msg), got " ^ string_of_typ t))
  | EAttr (name, value) ->
      let _, name = check_elab ctx TString name in
      let _, value = check_elab ctx TString value in
      (TAttr TUnit, EAttr (name, value))
  | EOn (event, msg) ->
      let _, event = check_elab ctx TString event in
      let msg_ty, msg = infer_elab ctx msg in
      (TAttr msg_ty, EOn (event, msg))
  | EDone e ->
      let t, e = infer_elab ctx e in
      (TProcess (Some [], t), EDone e)
  | EBind (p, x, annotation, body) ->
      let p_ty, p = infer_elab ctx p in
      (match p_ty with
      | TProcess (process_caps, a) ->
          require_type a annotation "bind annotation";
          let body_ty, body = infer_elab (bind_local ctx x a) body in
          (match body_ty with
          | TProcess (body_caps, value_ty) ->
              ( TProcess (merge_process_capabilities process_caps body_caps, value_ty),
                EBind (p, x, annotation, body) )
          | t -> fail ("bind body must return Process, got " ^ string_of_typ t))
      | t -> fail ("bind on non-process: " ^ string_of_typ t))
  | EBindInfer (p, x, body) ->
      let p_ty, p = infer_elab ctx p in
      (match p_ty with
      | TProcess (process_caps, a) ->
          let body_ty, body = infer_elab (bind_local ctx x a) body in
          (match body_ty with
          | TProcess (body_caps, value_ty) ->
              ( TProcess (merge_process_capabilities process_caps body_caps, value_ty),
                EBind (p, x, a, body) )
          | t -> fail ("bind body must return Process, got " ^ string_of_typ t))
      | t -> fail ("bind on non-process: " ^ string_of_typ t))

and elaborate_record_update ctx record updates =
  if updates = [] then fail "recordUpdate requires at least one field";
  let record_ty, record = infer_elab ctx record in
  match unfold_type ctx record_ty with
  | TRecord fields ->
      let updates =
        List.map
          (fun (name, expr) ->
            match assoc_opt name fields with
            | None -> fail ("unknown record update field: " ^ name)
            | Some expected ->
                let _, expr = check_elab ctx expected expr in
                (name, expr))
          (sort_fields updates)
      in
      let record_name = fresh_record_update_name ctx record updates in
      let rebuilt =
        ERecord
          (List.map
             (fun (name, _) ->
               match assoc_opt name updates with
               | Some expr -> (name, expr)
               | None -> (name, EField (EName record_name, name)))
             (sort_fields fields))
      in
      (record_ty, ELet (record_name, record, rebuilt))
  | t -> fail ("record update on non-record: " ^ string_of_typ t)

and check_elab ctx expected expr =
  match (unfold_type ctx expected, expr) with
  | TVariant _, EVariantInferred (con, e) ->
      let _, e = elaborate_variant_payload ctx expected con e in
      (expected, EVariant (expected, con, e))
  | TVariant _, EVariant (explicit_ty, con, e) ->
      require_type expected explicit_ty "variant expected type";
      let _, e = elaborate_variant_payload ctx expected con e in
      (expected, EVariant (expected, con, e))
  | TFun (expected_arg, expected_body), ELambda (x, actual_arg, body) ->
      require_type expected_arg actual_arg "lambda parameter";
      let _, body = check_elab (bind_lambda ctx x actual_arg) expected_body body in
      (expected, ELambda (x, actual_arg, body))
  | TFun (expected_arg, expected_body), ELambdaInfer (x, body) ->
      let _, body = check_elab (bind_lambda ctx x expected_arg) expected_body body in
      (expected, ELambda (x, expected_arg, body))
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
  | TProcess (expected_caps, expected_value), EDone e ->
      require_process_capabilities expected_caps (Some []) "done";
      let _, e = check_elab ctx expected_value e in
      (expected, EDone e)
  | TCmd _, EUnit -> (expected, EUnit)
  | TView msg_ty, ENode (tag, attrs, children) ->
      let _, tag = check_elab ctx TString tag in
      let _, attrs = check_elab ctx (TList (TAttr msg_ty)) attrs in
      let _, children = check_elab ctx (TList (TView msg_ty)) children in
      (expected, ENode (tag, attrs, children))
  | TList item_ty, ECons (actual_item_ty, head, tail) ->
      require_type item_ty actual_item_ty "Cons type";
      let _, head = check_elab ctx item_ty head in
      let _, tail = check_elab ctx expected tail in
      (expected, ECons (actual_item_ty, head, tail))
  | TList item_ty, ENilInfer -> (expected, ENil item_ty)
  | TList item_ty, EConsInfer (head, tail) ->
      let _, head = check_elab ctx item_ty head in
      let _, tail = check_elab ctx expected tail in
      (expected, ECons (item_ty, head, tail))
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
          let _, step = check_fold_list_step_elab ctx xs item_ty expected step in
          (expected, EFoldList (xs, zero, step))
      | t -> fail ("foldList target must be List, got " ^ string_of_typ t))
  | _, ECaseList (xs, nil_body, head, tail, cons_body) -> (
      let xs_ty, xs = infer_elab ctx xs in
      match xs_ty with
      | TList item_ty ->
          let _, nil_body = check_elab ctx expected nil_body in
          let branch_ctx = bind_local (bind_local ctx tail (TList item_ty)) head item_ty in
          let _, cons_body = check_elab branch_ctx expected cons_body in
          (expected, ECaseList (xs, nil_body, head, tail, cons_body))
      | t -> fail ("caseList target must be List, got " ^ string_of_typ t))
  | _, ELet (x, e, body) ->
      let t, e = infer_elab ctx e in
      let _, body = check_elab (bind_local ctx x t) expected body in
      if is_process_type t && not (is_process_type expected) then
        fail
          ("Process used as pure value in let " ^ x ^ ": expected Process flow, got "
         ^ string_of_typ expected ^ ", expression " ^ string_of_expr body);
      (expected, ELet (x, e, body))
  | _, ELetAnnot (x, annotation, e, body) ->
      let _, e = check_elab ctx annotation e in
      let _, body = check_elab (bind_local ctx x annotation) expected body in
      if is_process_type annotation && not (is_process_type expected) then
        fail
          ("Process used as pure value in let " ^ x ^ ": expected Process flow, got "
         ^ string_of_typ expected ^ ", expression " ^ string_of_expr body);
      (expected, ELet (x, e, body))
  | _, ELetRecord (record, fields, body) ->
      check_elab ctx expected (desugar_let_record ctx record fields body)
  | TProcess (expected_caps, expected_value), EBind (p, x, annotation, body) ->
      let p_ty, p = infer_elab ctx p in
      (match p_ty with
      | TProcess (process_caps, a) ->
          require_type a annotation "bind annotation";
          let body_ctx = bind_local ctx x a in
          let _, body = check_elab body_ctx (TProcess (None, expected_value)) body in
          let body_ty = infer body_ctx body in
          (match body_ty with
          | TProcess (body_caps, _) ->
              require_process_capabilities expected_caps
                (merge_process_capabilities process_caps body_caps)
                "bind"
          | _ -> ());
          (expected, EBind (p, x, annotation, body))
      | t -> fail ("bind on non-process: " ^ string_of_typ t))
  | TProcess (expected_caps, expected_value), EBindInfer (p, x, body) ->
      let p_ty, p = infer_elab ctx p in
      (match p_ty with
      | TProcess (process_caps, a) ->
          let body_ctx = bind_local ctx x a in
          let _, body = check_elab body_ctx (TProcess (None, expected_value)) body in
          let body_ty = infer body_ctx body in
          (match body_ty with
          | TProcess (body_caps, _) ->
              require_process_capabilities expected_caps
                (merge_process_capabilities process_caps body_caps)
                "bind"
          | _ -> ());
          (expected, EBind (p, x, a, body))
      | t -> fail ("bind on non-process: " ^ string_of_typ t))
  | _, EName _ | _, EApp _ -> (
      match poly_app_elab ctx (Some expected) expr with
      | Some (_, expr) -> (expected, expr)
      | None ->
          let actual, expr = infer_elab ctx expr in
          require_type_expr expected actual "expected context" expr;
          (expected, expr))
  | _, ECase (scrut, branches) ->
      let scrut_ty, scrut = infer_elab ctx scrut in
      let branches =
        match unfold_type ctx scrut_ty with
        | TBool -> check_bool_case_elab ctx expected branches
        | TVariant cases -> check_variant_case_elab ctx cases expected branches
        | t -> fail ("case on unsupported type: " ^ string_of_typ t)
      in
      (expected, ECase (scrut, branches))
  | _, EStrict e ->
      let _, e = check_elab ctx expected e in
      (expected, EStrict e)
  | _ ->
      let actual, expr = infer_elab ctx expr in
      require_type_expr expected actual "expected context" expr;
      (expected, expr)

and check_fold_list_step_elab ctx xs item_ty result_ty step =
  let expected = TFun (item_ty, TFun (result_ty, result_ty)) in
  let check_structural_step item actual_item_ty acc actual_acc_ty body =
    require_type item_ty actual_item_ty "foldList step item";
    require_type result_ty actual_acc_ty "foldList step accumulator";
    let item_ctx = bind_recur_item ctx item actual_item_ty in
    let acc_ctx = bind_local item_ctx acc actual_acc_ty in
    let _, body = check_elab acc_ctx result_ty body in
    (expected, ELambda (item, actual_item_ty, ELambda (acc, actual_acc_ty, body)))
  in
  if structural_list_recur_allowed ctx xs item_ty then
    match step with
    | ELambda (item, actual_item_ty, ELambda (acc, actual_acc_ty, body)) ->
        check_structural_step item actual_item_ty acc actual_acc_ty body
    | ELambda (item, actual_item_ty, ELambdaInfer (acc, body)) ->
        check_structural_step item actual_item_ty acc result_ty body
    | ELambdaInfer (item, ELambda (acc, actual_acc_ty, body)) ->
        check_structural_step item item_ty acc actual_acc_ty body
    | ELambdaInfer (item, ELambdaInfer (acc, body)) ->
        check_structural_step item item_ty acc result_ty body
    | _ -> check_elab ctx expected step
  else check_elab ctx expected step

and poly_app_elab ctx expected expr =
  let root, args = app_spine expr in
  match root with
  | EName name -> (
      match (assoc_opt name ctx.locals, lookup_global ctx name) with
      | Some _, _ -> None
      | None, Some g when g.global_type_params <> [] ->
          let arity = List.length g.global_type_params in
          let solutions = Array.make arity None in
          let bind_solution index typ =
            if index < 0 || index >= arity then ()
            else
              match solutions.(index) with
              | None -> solutions.(index) <- Some typ
              | Some existing ->
                  if not (equal_typ existing typ) then
                    fail
                      ("conflicting inferred type argument "
                      ^ List.nth g.global_type_params index ^ " for " ^ name ^ ": "
                      ^ string_of_typ existing ^ " vs " ^ string_of_typ typ)
          in
          let rec unify pattern actual =
            match pattern with
            | TVar i when i < arity -> bind_solution i actual
            | TFun (pa, pb) -> (
                match actual with
                | TFun (aa, ab) ->
                    unify pa aa;
                    unify pb ab
                | _ -> fail ("cannot infer " ^ name ^ ": expected function type"))
            | TRecord p_fields -> (
                match actual with
                | TRecord a_fields ->
                    let p_fields = sort_fields p_fields and a_fields = sort_fields a_fields in
                    if List.map fst p_fields <> List.map fst a_fields then
                      fail ("cannot infer " ^ name ^ ": record fields differ");
                    List.iter2 (fun (_, p) (_, a) -> unify p a) p_fields a_fields
                | _ -> fail ("cannot infer " ^ name ^ ": expected record type"))
            | TVariant p_cases -> (
                match actual with
                | TVariant a_cases ->
                    let p_cases = sort_fields p_cases and a_cases = sort_fields a_cases in
                    if List.map fst p_cases <> List.map fst a_cases then
                      fail ("cannot infer " ^ name ^ ": variant constructors differ");
                    List.iter2 (fun (_, p) (_, a) -> unify p a) p_cases a_cases
                | _ -> fail ("cannot infer " ^ name ^ ": expected variant type"))
            | TList p -> (
                match actual with TList a -> unify p a | _ -> fail ("cannot infer " ^ name ^ ": expected List"))
            | TView p -> (
                match actual with TView a -> unify p a | _ -> fail ("cannot infer " ^ name ^ ": expected View"))
            | TAttr p -> (
                match actual with TAttr a -> unify p a | _ -> fail ("cannot infer " ^ name ^ ": expected Attr"))
            | TStream p -> (
                match actual with
                | TStream a -> unify p a
                | _ -> fail ("cannot infer " ^ name ^ ": expected Stream"))
            | TAutomaton (p_state, p_output) -> (
                match actual with
                | TAutomaton (a_state, a_output) ->
                    unify p_state a_state;
                    unify p_output a_output
                | _ -> fail ("cannot infer " ^ name ^ ": expected Automaton"))
            | TProcess (pattern_caps, p) -> (
                match actual with
                | TProcess (actual_caps, a) ->
                    require_process_capabilities pattern_caps actual_caps
                      ("cannot infer " ^ name);
                    unify p a
                | _ -> fail ("cannot infer " ^ name ^ ": expected Process"))
            | TCmd (pattern_caps, p) -> (
                match actual with
                | TCmd (actual_caps, a) ->
                    require_process_capabilities pattern_caps actual_caps
                      ("cannot infer " ^ name);
                    unify p a
                | _ -> fail ("cannot infer " ^ name ^ ": expected Cmd"))
            | TNamed (pn, ps) -> (
                match actual with
                | TNamed (an, args) when String.equal pn an && List.length ps = List.length args ->
                    List.iter2 unify ps args
                | _ -> if not (equal_typ pattern actual) then fail ("cannot infer " ^ name))
            | TForall (pa, pb) -> (
                match actual with
                | TForall (aa, ab) when pa = aa -> unify pb ab
                | _ -> if not (equal_typ pattern actual) then fail ("cannot infer " ^ name))
            | _ ->
                if not (equal_typ pattern actual) then
                  fail
                    ("cannot infer " ^ name ^ ": expected " ^ string_of_typ pattern ^ ", got "
                   ^ string_of_typ actual)
          in
          let infer_type_opt expr =
            try Some (fst (infer_elab ctx expr)) with Error _ -> None
          in
          let unify_inferred pattern expr =
            match infer_type_opt expr with
            | Some actual -> unify pattern actual
            | None -> ()
          in
          let rec collect_expected_constraints pattern arg =
            match (unfold_type ctx pattern, arg) with
            | TList item_ty, ENil explicit_item_ty -> unify item_ty explicit_item_ty
            | TList _, ENilInfer -> ()
            | TList item_ty, ECons (explicit_item_ty, head, tail) ->
                unify item_ty explicit_item_ty;
                collect_expected_constraints item_ty head;
                collect_expected_constraints (TList item_ty) tail
            | TList item_ty, EConsInfer (head, tail) ->
                collect_expected_constraints item_ty head;
                collect_expected_constraints (TList item_ty) tail
            | _ -> unify_inferred pattern arg
          in
          let rec take_params typ remaining acc =
            match remaining with
            | [] -> (List.rev acc, typ)
            | _ :: rest -> (
                match typ with
                | TFun (arg, result) -> take_params result rest (arg :: acc)
                | _ -> fail ("too many arguments for polymorphic definition: " ^ name))
          in
          let params, result = take_params (type_body g.global_typ) args [] in
          (match expected with Some expected -> unify result expected | None -> ());
          List.iter2 collect_expected_constraints params args;
          List.iter2
            (fun param arg ->
              try
                let actual, _ = infer_elab ctx arg in
                unify param actual
              with Error _ -> ())
            params args;
          let missing =
            g.global_type_params
            |> List.mapi (fun i param -> (i, param))
            |> List.filter (fun (i, _) -> solutions.(i) = None)
          in
          (match missing with
          | [] ->
              let type_args =
                Array.to_list solutions
                |> List.map (option_or_fail ("internal missing inferred type argument for " ^ name))
              in
              let params = List.map (subst_type type_args) params in
              let result = subst_type type_args result in
              (match expected with Some expected -> require_type expected result "polymorphic result" | None -> ());
              let args =
                List.map2
                  (fun param arg ->
                    let _, arg = check_elab ctx param arg in
                    arg)
                  params args
              in
              let expr = List.fold_left (fun f arg -> EApp (f, arg)) (EInst (name, type_args)) args in
              Some (result, expr)
          | _ -> (
              match expected with
              | None -> None
              | Some _ ->
                  fail
                    ("cannot infer type argument(s) "
                    ^ String.concat ", " (List.map snd missing)
                    ^ " for " ^ name)))
      | _, _ -> None)
  | _ -> None

and elaborate_variant_payload ctx ty con e =
  match unfold_type ctx ty with
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
  let t_expr, f_expr = bool_case_branches branches in
  let ty, t_expr = infer_elab ctx t_expr in
  let _, f_expr = check_elab ctx ty f_expr in
  (ty, [ BBool (true, t_expr); BBool (false, f_expr) ])

and check_bool_case_elab ctx expected branches =
  let t_expr, f_expr = bool_case_branches branches in
  let _, t_expr = check_elab ctx expected t_expr in
  let _, f_expr = check_elab ctx expected f_expr in
  [ BBool (true, t_expr); BBool (false, f_expr) ]

and infer_variant_case_elab ctx cases branches =
  let ty = infer_variant_case ctx cases branches in
  let branches = check_variant_case_elab ctx cases ty branches in
  (ty, branches)

and check_variant_case_elab ctx cases expected branches =
  let branches = expand_variant_case_branches cases branches "Variant case" in
  List.map
    (function
      | BBool _ -> assert false
      | BVariant (con, x, body) ->
          let payload_ty = Option.get (assoc_opt con cases) in
          let _, body = check_elab (bind_local ctx x payload_ty) expected body in
          BVariant (con, x, body)
      | BVariantUnit (con, body) ->
          let payload_ty = Option.get (assoc_opt con cases) in
          require_unit_branch_payload con payload_ty;
          let _, body = check_elab ctx expected body in
          BVariant (con, "_", body)
      | BWildcard _ -> assert false)
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
  | CStrict of cterm
  | CLet of cterm * cterm
  | CRecord of (string * cterm) list
  | CField of cterm * string
  | CVariant of typ * string * cterm
  | CInst of string * typ list
  | CCase of cterm * cbranch list
  | CFoldNat of cterm * cterm * cterm
  | CFoldVariant of typ * typ * cterm * cbranch list
  | CRecur of cterm
  | CNil of typ
  | CCons of typ * cterm * cterm
  | CFoldList of cterm * cterm * cterm
  | CCaseList of cterm * cterm * cterm
  | CCoiter of typ * typ * cterm * cterm
  | CStreamHead of cterm
  | CStreamTail of cterm
  | CStreamTake of cterm * cterm
  | CAutomaton of typ * typ * cterm * cterm
  | CAutomatonRun of cterm * cterm
  | CText of cterm
  | CImage of cterm * cterm
  | CButton of cterm * cterm
  | CInput of cterm * cterm
  | CColumn of cterm
  | CRow of cterm
  | CListView of cterm * cterm
  | CWhenView of cterm * cterm
  | CNode of cterm * cterm * cterm
  | CAttr of cterm * cterm
  | COn of cterm * cterm
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
  | ELambdaInfer _ -> fail "unelaborated unannotated lambda in canonicalization"
  | EApp (f, x) -> CApp (canonical_expr env f, canonical_expr env x)
  | EStrict e -> CStrict (canonical_expr env e)
  | ELet (x, e, body) -> CLet (canonical_expr env e, canonical_expr (x :: env) body)
  | ELetAnnot _ -> fail "unelaborated annotated let in canonicalization"
  | ELetRecord _ -> fail "unelaborated record destructuring in canonicalization"
  | ERecord fields ->
      CRecord (sort_fields (List.map (fun (n, e) -> (n, canonical_expr env e)) fields))
  | ERecordUpdate _ -> fail "unelaborated record update in canonicalization"
  | EField (e, field) -> CField (canonical_expr env e, field)
  | EVariant (t, con, e) -> CVariant (t, con, canonical_expr env e)
  | EVariantInferred (con, _) -> fail ("unelaborated variant constructor in canonicalization: " ^ con)
  | EInst (name, args) ->
      if index_of name env 0 <> None then fail ("cannot instantiate local binding: " ^ name);
      CInst (name, args)
  | ECase (e, branches) -> CCase (canonical_expr env e, canonical_branches env branches)
  | EFoldNat (n, z, step) ->
      CFoldNat (canonical_expr env n, canonical_expr env z, canonical_expr env step)
  | EFoldVariant (target, result, scrut, branches) ->
      CFoldVariant
        (target, result, canonical_expr env scrut, canonical_branches env branches)
  | ERecur e -> CRecur (canonical_expr env e)
  | ENil t -> CNil t
  | ENilInfer -> fail "unelaborated inferred Nil in canonicalization"
  | ECons (t, head, tail) -> CCons (t, canonical_expr env head, canonical_expr env tail)
  | EConsInfer _ -> fail "unelaborated inferred Cons in canonicalization"
  | EFoldList (xs, z, step) ->
      CFoldList (canonical_expr env xs, canonical_expr env z, canonical_expr env step)
  | ECaseList (xs, nil_body, head, tail, cons_body) ->
      CCaseList
        ( canonical_expr env xs,
          canonical_expr env nil_body,
          canonical_expr (head :: tail :: env) cons_body )
  | ECoiter (state_ty, item_ty, seed, step) ->
      CCoiter (state_ty, item_ty, canonical_expr env seed, canonical_expr env step)
  | EStreamHead stream -> CStreamHead (canonical_expr env stream)
  | EStreamTail stream -> CStreamTail (canonical_expr env stream)
  | EStreamTake (count, stream) ->
      CStreamTake (canonical_expr env count, canonical_expr env stream)
  | EAutomaton (state_ty, output_ty, initial, transition) ->
      CAutomaton
        (state_ty, output_ty, canonical_expr env initial, canonical_expr env transition)
  | EAutomatonRun (count, automaton) ->
      CAutomatonRun (canonical_expr env count, canonical_expr env automaton)
  | EText e -> CText (canonical_expr env e)
  | EImage (src, alt) -> CImage (canonical_expr env src, canonical_expr env alt)
  | EButton (label, msg) -> CButton (canonical_expr env label, canonical_expr env msg)
  | EInput (value, handler) -> CInput (canonical_expr env value, canonical_expr env handler)
  | EColumn children -> CColumn (canonical_expr env children)
  | ERow children -> CRow (canonical_expr env children)
  | EListView (items, render) -> CListView (canonical_expr env items, canonical_expr env render)
  | EWhenView (cond, view) -> CWhenView (canonical_expr env cond, canonical_expr env view)
  | ENode (tag, attrs, children) ->
      CNode (canonical_expr env tag, canonical_expr env attrs, canonical_expr env children)
  | EAttr (name, value) -> CAttr (canonical_expr env name, canonical_expr env value)
  | EOn (event, msg) -> COn (canonical_expr env event, canonical_expr env msg)
  | EDone e -> CDone (canonical_expr env e)
  | ERequest req -> CRequest req
  | EBind (p, x, t, body) -> CBind (canonical_expr env p, t, canonical_expr (x :: env) body)
  | EBindInfer _ -> fail "unelaborated unannotated bind in canonicalization"

and canonical_branches env branches =
  let cbs =
    List.map
      (function
        | BBool (b, e) -> CBBool (b, canonical_expr env e)
        | BVariant (con, x, e) -> CBVariant (con, canonical_expr (x :: env) e)
        | BVariantUnit _ -> fail "unelaborated unit variant branch in canonicalization"
        | BWildcard _ -> fail "unelaborated wildcard branch in canonicalization")
      branches
  in
  List.sort
    (fun a b ->
      let ka = match a with CBBool (v, _) -> if v then "1" else "0" | CBVariant (c, _) -> c in
      let kb = match b with CBBool (v, _) -> if v then "1" else "0" | CBVariant (c, _) -> c in
      String.compare ka kb)
    cbs

let rec cterm_direct_capabilities = function
  | CRequest req -> [ req_capability req ]
  | CUnit | CBool _ | CNat _ | CString _ | CVar _ | CGlobal _ | CInst _ | CNil _ -> []
  | CLambda (_, body) -> cterm_direct_capabilities body
  | CStrict e -> cterm_direct_capabilities e
  | CApp (f, x) | CLet (f, x) | CImage (f, x) | CButton (f, x) | CInput (f, x)
  | CListView (f, x) | CWhenView (f, x) | CAttr (f, x) | COn (f, x) ->
      cterm_direct_capabilities f @ cterm_direct_capabilities x
  | CNode (tag, attrs, children) ->
      cterm_direct_capabilities tag @ cterm_direct_capabilities attrs
      @ cterm_direct_capabilities children
  | CRecord fields -> List.concat_map (fun (_, e) -> cterm_direct_capabilities e) fields
  | CField (e, _) | CVariant (_, _, e) | CRecur e | CText e | CColumn e | CRow e
  | CDone e ->
      cterm_direct_capabilities e
  | CCase (e, branches) ->
      cterm_direct_capabilities e @ List.concat_map cbranch_direct_capabilities branches
  | CFoldNat (n, zero, step) | CFoldList (n, zero, step) ->
      cterm_direct_capabilities n @ cterm_direct_capabilities zero
      @ cterm_direct_capabilities step
  | CCoiter (_, _, seed, step) | CAutomaton (_, _, seed, step) ->
      cterm_direct_capabilities seed @ cterm_direct_capabilities step
  | CStreamTake (count, stream) | CAutomatonRun (count, stream) ->
      cterm_direct_capabilities count @ cterm_direct_capabilities stream
  | CCaseList (xs, nil_body, cons_body) ->
      cterm_direct_capabilities xs @ cterm_direct_capabilities nil_body
      @ cterm_direct_capabilities cons_body
  | CStreamHead stream | CStreamTail stream -> cterm_direct_capabilities stream
  | CFoldVariant (_, _, scrut, branches) ->
      cterm_direct_capabilities scrut @ List.concat_map cbranch_direct_capabilities branches
  | CCons (_, head, tail) -> cterm_direct_capabilities head @ cterm_direct_capabilities tail
  | CBind (p, _, body) -> cterm_direct_capabilities p @ cterm_direct_capabilities body

and cbranch_direct_capabilities = function
  | CBBool (_, e) | CBVariant (_, e) -> cterm_direct_capabilities e

let rec cterm_global_refs = function
  | CGlobal n when not (is_builtin n) -> [ n ]
  | CInst (n, _) -> [ n ]
  | CUnit | CBool _ | CNat _ | CString _ | CVar _ | CGlobal _ | CRequest _ | CNil _ -> []
  | CLambda (_, body) -> cterm_global_refs body
  | CStrict e -> cterm_global_refs e
  | CApp (f, x) | CLet (f, x) | CImage (f, x) | CButton (f, x) | CInput (f, x)
  | CListView (f, x) | CWhenView (f, x) | CAttr (f, x) | COn (f, x) ->
      cterm_global_refs f @ cterm_global_refs x
  | CNode (tag, attrs, children) ->
      cterm_global_refs tag @ cterm_global_refs attrs @ cterm_global_refs children
  | CRecord fields -> List.concat_map (fun (_, e) -> cterm_global_refs e) fields
  | CField (e, _) | CVariant (_, _, e) | CRecur e | CText e | CColumn e | CRow e
  | CDone e ->
      cterm_global_refs e
  | CCase (e, branches) -> cterm_global_refs e @ List.concat_map cbranch_global_refs branches
  | CFoldNat (n, zero, step) | CFoldList (n, zero, step) ->
      cterm_global_refs n @ cterm_global_refs zero @ cterm_global_refs step
  | CCoiter (_, _, seed, step) | CAutomaton (_, _, seed, step) ->
      cterm_global_refs seed @ cterm_global_refs step
  | CStreamTake (count, stream) | CAutomatonRun (count, stream) ->
      cterm_global_refs count @ cterm_global_refs stream
  | CCaseList (xs, nil_body, cons_body) ->
      cterm_global_refs xs @ cterm_global_refs nil_body @ cterm_global_refs cons_body
  | CStreamHead stream | CStreamTail stream -> cterm_global_refs stream
  | CFoldVariant (_, _, scrut, branches) ->
      cterm_global_refs scrut @ List.concat_map cbranch_global_refs branches
  | CCons (_, head, tail) -> cterm_global_refs head @ cterm_global_refs tail
  | CBind (p, _, body) -> cterm_global_refs p @ cterm_global_refs body

and cbranch_global_refs = function
  | CBBool (_, e) | CBVariant (_, e) -> cterm_global_refs e

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
  | CStrict e -> "(strict " ^ cterm_to_string e ^ ")"
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
  | CFoldVariant (target, result, scrut, branches) ->
      "(foldVariant " ^ type_to_canonical target ^ " " ^ type_to_canonical result ^ " "
      ^ cterm_to_string scrut ^ " "
      ^ String.concat " " (List.map cbranch_to_string branches)
      ^ ")"
  | CRecur e -> "(recur " ^ cterm_to_string e ^ ")"
  | CNil t -> "(Nil " ^ type_to_canonical t ^ ")"
  | CCons (t, head, tail) ->
      "(Cons " ^ type_to_canonical t ^ " " ^ cterm_to_string head ^ " " ^ cterm_to_string tail
      ^ ")"
  | CFoldList (xs, z, step) ->
      "(foldList " ^ cterm_to_string xs ^ " " ^ cterm_to_string z ^ " "
      ^ cterm_to_string step ^ ")"
  | CCaseList (xs, nil_body, cons_body) ->
      "(caseList " ^ cterm_to_string xs ^ " (Nil " ^ cterm_to_string nil_body
      ^ ") (Cons " ^ cterm_to_string cons_body ^ "))"
  | CCoiter (state_ty, item_ty, seed, step) ->
      "(coiter " ^ type_to_canonical state_ty ^ " " ^ type_to_canonical item_ty ^ " "
      ^ cterm_to_string seed ^ " " ^ cterm_to_string step ^ ")"
  | CStreamHead stream -> "(streamHead " ^ cterm_to_string stream ^ ")"
  | CStreamTail stream -> "(streamTail " ^ cterm_to_string stream ^ ")"
  | CStreamTake (count, stream) ->
      "(streamTake " ^ cterm_to_string count ^ " " ^ cterm_to_string stream ^ ")"
  | CAutomaton (state_ty, output_ty, initial, transition) ->
      "(automaton " ^ type_to_canonical state_ty ^ " " ^ type_to_canonical output_ty
      ^ " " ^ cterm_to_string initial ^ " " ^ cterm_to_string transition ^ ")"
  | CAutomatonRun (count, automaton) ->
      "(automatonRun " ^ cterm_to_string count ^ " " ^ cterm_to_string automaton ^ ")"
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
  | CNode (tag, attrs, children) ->
      "(node " ^ cterm_to_string tag ^ " " ^ cterm_to_string attrs ^ " "
      ^ cterm_to_string children ^ ")"
  | CAttr (name, value) -> "(attr " ^ cterm_to_string name ^ " " ^ cterm_to_string value ^ ")"
  | COn (event, msg) -> "(on " ^ cterm_to_string event ^ " " ^ cterm_to_string msg ^ ")"
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

let executable_grammar_text =
  String.concat "\n"
    [
      "protoss-kernel-grammar-v1";
      "program ::= declaration*";
      "declaration ::= (capabilities Capability*) | (module Name) | (export Name*)";
      "declaration ::= (type Name type-params? type) | (record Name type-params? field*)";
      "declaration ::= (variant Name type-params? variant-case*)";
      "declaration ::= (def Name type expr)";
      "declaration ::= (defpoly Name (params TypeName*) type expr)";
      "declaration ::= (defcap Name (capabilities Capability*) type expr)";
      "declaration ::= (defpolycap Name (params TypeName*) (capabilities Capability*) type expr)";
      "declaration ::= (defrec Name type-params? type recursion-body)";
      "type ::= Unit | Bool | Nat | String | (-> type type) | (List type) | (Stream type)";
      "type ::= (Automaton state-type output-type) | (Process type)";
      "type ::= (Process (capabilities Capability*) type)";
      "type ::= (Cmd type) | (Cmd (capabilities Capability*) type)";
      "type ::= (Record field*) | (Variant variant-case*) | (Named Name type*) | TypeName";
      "expr ::= unit | true | false | Nat | String | Name | (lambda binder+ expr)";
      "expr ::= (let binding expr) | (record field*) | (get expr Field)";
      "expr ::= (variant type Constructor expr) | (inst Name type*)";
      "expr ::= (case expr branch*) | (foldNat expr expr expr) | (foldList expr expr expr)";
      "expr ::= (foldVariant type type expr branch*) | (strict expr) | recur";
      "expr ::= (Nil type) | (Cons type expr expr) | (caseList expr expr Name Name expr)";
      "expr ::= (coiter state-type output-type seed step)";
      "expr ::= (streamHead expr) | (streamTail expr) | (streamTake Nat expr)";
      "expr ::= (automaton state-type output-type initial transition)";
      "expr ::= (automatonRun Nat expr)";
      "expr ::= (text expr) | (image expr expr) | (button expr expr) | (input expr expr)";
      "expr ::= (column expr) | (row expr) | (list expr expr) | (when expr expr)";
      "expr ::= (node expr expr expr) | (attr expr expr) | (on expr expr)";
      "expr ::= (done expr) | (request request) | (bind expr binder expr)";
      "request ::= (AskHuman String) | (HttpGet String) | ReadClock";
      "request ::= (SaveLocal String String) | (LoadLocal String)";
      "request ::= (ServerRequest String String)";
      "field ::= (Name type) | (Name expr)";
      "variant-case ::= (Constructor type)";
      "branch ::= (true expr) | (false expr) | (Constructor binder? expr) | (_ expr)";
      "binder ::= Name | (Name type)";
      "type-params ::= (params TypeName*)";
      "";
    ]

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
  | CStrict e -> "(strict " ^ cterm_to_canonical_v2 def_id_of e ^ ")"
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
  | CFoldVariant (target, result, scrut, branches) ->
      "(foldVariant " ^ type_to_canonical target ^ " " ^ type_to_canonical result ^ " "
      ^ cterm_to_canonical_v2 def_id_of scrut ^ " "
      ^ String.concat " " (List.map (cbranch_to_canonical_v2 def_id_of) branches)
      ^ ")"
  | CRecur e -> "(recur " ^ cterm_to_canonical_v2 def_id_of e ^ ")"
  | CNil t -> "(Nil " ^ type_to_canonical t ^ ")"
  | CCons (t, head, tail) ->
      "(Cons " ^ type_to_canonical t ^ " " ^ cterm_to_canonical_v2 def_id_of head ^ " "
      ^ cterm_to_canonical_v2 def_id_of tail ^ ")"
  | CFoldList (xs, z, step) ->
      "(foldList " ^ cterm_to_canonical_v2 def_id_of xs ^ " "
      ^ cterm_to_canonical_v2 def_id_of z ^ " " ^ cterm_to_canonical_v2 def_id_of step ^ ")"
  | CCaseList (xs, nil_body, cons_body) ->
      "(caseList " ^ cterm_to_canonical_v2 def_id_of xs ^ " (Nil "
      ^ cterm_to_canonical_v2 def_id_of nil_body ^ ") (Cons "
      ^ cterm_to_canonical_v2 def_id_of cons_body ^ "))"
  | CCoiter (state_ty, item_ty, seed, step) ->
      "(coiter " ^ type_to_canonical state_ty ^ " " ^ type_to_canonical item_ty ^ " "
      ^ cterm_to_canonical_v2 def_id_of seed ^ " "
      ^ cterm_to_canonical_v2 def_id_of step ^ ")"
  | CStreamHead stream ->
      "(streamHead " ^ cterm_to_canonical_v2 def_id_of stream ^ ")"
  | CStreamTail stream ->
      "(streamTail " ^ cterm_to_canonical_v2 def_id_of stream ^ ")"
  | CStreamTake (count, stream) ->
      "(streamTake " ^ cterm_to_canonical_v2 def_id_of count ^ " "
      ^ cterm_to_canonical_v2 def_id_of stream ^ ")"
  | CAutomaton (state_ty, output_ty, initial, transition) ->
      "(automaton " ^ type_to_canonical state_ty ^ " " ^ type_to_canonical output_ty
      ^ " " ^ cterm_to_canonical_v2 def_id_of initial ^ " "
      ^ cterm_to_canonical_v2 def_id_of transition ^ ")"
  | CAutomatonRun (count, automaton) ->
      "(automatonRun " ^ cterm_to_canonical_v2 def_id_of count ^ " "
      ^ cterm_to_canonical_v2 def_id_of automaton ^ ")"
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
  | CNode (tag, attrs, children) ->
      "(node " ^ cterm_to_canonical_v2 def_id_of tag ^ " "
      ^ cterm_to_canonical_v2 def_id_of attrs ^ " "
      ^ cterm_to_canonical_v2 def_id_of children ^ ")"
  | CAttr (name, value) ->
      "(attr " ^ cterm_to_canonical_v2 def_id_of name ^ " "
      ^ cterm_to_canonical_v2 def_id_of value ^ ")"
  | COn (event, msg) ->
      "(on " ^ cterm_to_canonical_v2 def_id_of event ^ " "
      ^ cterm_to_canonical_v2 def_id_of msg ^ ")"
  | CDone e -> "(done " ^ cterm_to_canonical_v2 def_id_of e ^ ")"
  | CRequest req -> req_to_canonical req
  | CBind (p, t, body) ->
      "(bind " ^ cterm_to_canonical_v2 def_id_of p ^ " " ^ type_to_canonical t ^ " "
      ^ cterm_to_canonical_v2 def_id_of body ^ ")"

and cbranch_to_canonical_v2 def_id_of = function
  | CBBool (true, e) -> "(true " ^ cterm_to_canonical_v2 def_id_of e ^ ")"
  | CBBool (false, e) -> "(false " ^ cterm_to_canonical_v2 def_id_of e ^ ")"
  | CBVariant (con, e) -> "(" ^ con ^ " " ^ cterm_to_canonical_v2 def_id_of e ^ ")"

module Phys_cterm_tbl = Hashtbl.Make (struct
  type t = cterm

  let equal = ( == )

  let hash t = Hashtbl.hash_param 64 256 t
end)

module Phys_typ_tbl = Hashtbl.Make (struct
  type t = typ

  let equal = ( == )

  let hash t = Hashtbl.hash_param 64 256 t
end)

(* Whole-definition canonical serialization is requested once per def by
   def-id derivation, once per def by store/program serialization, and again
   by every re-check of a reloaded program. The serialized body is a pure
   function of the term plus the def ids its global refs resolve to, and the
   per-definition elaboration cache hands back physically shared cterms
   across checks - so memoizing by physical term identity plus the resolved
   ref signature removes whole-program re-serialization from those flows
   (byte-identical output: same term, same ref ids => same string). The refs
   of a term are themselves cached by physical identity, making cache hits
   O(refs) instead of O(term size). *)
let cterm_refs_cache : string list Phys_cterm_tbl.t = Phys_cterm_tbl.create 4096

let cterm_global_refs_cached term =
  match Phys_cterm_tbl.find_opt cterm_refs_cache term with
  | Some refs -> refs
  | None ->
      let refs = cterm_global_refs term in
      if Phys_cterm_tbl.length cterm_refs_cache >= 65536 then
        Phys_cterm_tbl.reset cterm_refs_cache;
      Phys_cterm_tbl.add cterm_refs_cache term refs;
      refs

let cterm_canon_v2_cache : (string * string) list ref Phys_cterm_tbl.t =
  Phys_cterm_tbl.create 4096

let cterm_to_canonical_v2_cached def_id_of term =
  let refs = cterm_global_refs_cached term in
  let refs_sig = String.concat "\x00" (List.map def_id_of refs) in
  let entries =
    match Phys_cterm_tbl.find_opt cterm_canon_v2_cache term with
    | Some entries -> entries
    | None ->
        let entries = ref [] in
        if Phys_cterm_tbl.length cterm_canon_v2_cache >= 16384 then
          Phys_cterm_tbl.reset cterm_canon_v2_cache;
        Phys_cterm_tbl.add cterm_canon_v2_cache term entries;
        entries
  in
  match List.assoc_opt refs_sig !entries with
  | Some serialized -> serialized
  | None ->
      let serialized = cterm_to_canonical_v2 def_id_of term in
      entries := (refs_sig, serialized) :: List.filteri (fun i _ -> i < 3) !entries;
      serialized

let serialize_def_payload name def_id typ body def_id_of =
  "(def " ^ name ^ " " ^ def_id ^ " " ^ type_to_canonical typ ^ " "
  ^ cterm_to_canonical_v2_cached def_id_of body ^ ")"

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

let canonical_graph_legacy_v1 = "protoss-canon-graph-v1"

let canonical_graph_version = "protoss-canon-graph-v2"

let canonical_node_graph_version = "protoss-canon-node-graph-v1"

let json_string = Ast.quote

let json_field name value = json_string name ^ ": " ^ value

let json_obj fields = "{ " ^ String.concat ", " fields ^ " }"

let json_array f xs = "[" ^ String.concat ", " (List.map f xs) ^ "]"

let json_bool b = if b then "true" else "false"

let canonical_node_id kind canonical =
  hash_string ("canonical-node-v1:" ^ kind ^ ":" ^ canonical)

let type_node_id typ = canonical_node_id "Type" (type_to_canonical typ)

let term_node_id def_id_of term =
  canonical_node_id "Term" (cterm_to_canonical_v2 def_id_of term)

let uniq_strings xs = List.sort_uniq String.compare xs

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
  | TAttr t ->
      json_obj [ json_field "tag" (json_string "Attr"); json_field "message" (type_to_graph_json t) ]
  | TStream t ->
      json_obj [ json_field "tag" (json_string "Stream"); json_field "item" (type_to_graph_json t) ]
  | TAutomaton (state, output) ->
      json_obj
        [
          json_field "tag" (json_string "Automaton");
          json_field "state" (type_to_graph_json state);
          json_field "output" (type_to_graph_json output);
        ]
  | TProcess (caps, t) ->
      json_obj
        ([ json_field "tag" (json_string "Process"); json_field "result" (type_to_graph_json t) ]
        @
        match normalize_process_capabilities caps with
        | None -> []
        | Some caps -> [ json_field "capabilities" (json_array json_string caps) ])
  | TCmd (caps, t) ->
      json_obj
        ([ json_field "tag" (json_string "Cmd"); json_field "message" (type_to_graph_json t) ]
        @
        match normalize_process_capabilities caps with
        | None -> []
        | Some caps -> [ json_field "capabilities" (json_array json_string caps) ])
  | TSecretRef (scope, t) ->
      json_obj
        [
          json_field "tag" (json_string "SecretRef");
          json_field "scope" (json_string scope);
          json_field "value" (type_to_graph_json t);
        ]
  | TVar i ->
      json_obj [ json_field "tag" (json_string "TypeVar"); json_field "index" (string_of_int i) ]
  | TForall (arity, body) ->
      json_obj
        [
          json_field "tag" (json_string "Forall");
          json_field "arity" (string_of_int arity);
          json_field "body" (type_to_graph_json body);
        ]
  | TNamed (n, args) ->
      json_obj
        [
          json_field "tag" (json_string "Named");
          json_field "name" (json_string n);
          json_field "args" (json_array type_to_graph_json args);
        ]

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
  let capability = req_capability req in
  let capability_ref =
    match capability_ref capability with
    | Some ref -> ref
    | None -> fail ("unknown capability in canonical graph request: " ^ capability)
  in
  json_obj
    (json_field "tag" (json_string tag)
    :: json_field "capability" (json_string capability)
    :: json_field "capabilityRef" (json_string capability_ref)
    :: json_field "requestSignatureRef" (json_string (req_signature_ref req))
    :: fields)

let capability_request_to_graph_json capability req =
  json_obj
    [
      json_field "ref" (json_string (capability_request_signature_ref capability req));
      json_field "tag" (json_string req.request_tag);
      json_field "payloadType" (type_to_graph_json req.request_payload_type);
      json_field "responseType" (type_to_graph_json req.response_type);
    ]

let capability_descriptor_to_graph_json desc =
  json_obj
    [
      json_field "ref" (json_string (capability_descriptor_ref desc));
      json_field "name" (json_string desc.capability_name);
      json_field "requests"
        (json_array (capability_request_to_graph_json desc.capability_name)
           desc.request_signatures);
    ]

let declared_capability_descriptors caps =
  caps
  |> List.sort_uniq String.compare
  |> List.filter_map capability_descriptor

let capabilities_to_graph_json caps =
  json_array capability_descriptor_to_graph_json (declared_capability_descriptors caps)

let rec cterm_to_graph_json_via recurse def_id_of = function
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
          json_field "body" (recurse body);
        ]
  | CApp (f, arg) ->
      json_obj
        [
          json_field "tag" (json_string "App");
          json_field "fn" (recurse f);
          json_field "arg" (recurse arg);
        ]
  | CStrict e ->
      json_obj
        [
          json_field "tag" (json_string "Strict");
          json_field "value" (recurse e);
        ]
  | CLet (e, body) ->
      json_obj
        [
          json_field "tag" (json_string "Let");
          json_field "value" (recurse e);
          json_field "body" (recurse body);
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
                     json_field "value" (recurse value);
                   ])
               fields);
        ]
  | CField (e, field) ->
      json_obj
        [
          json_field "tag" (json_string "Field");
          json_field "record" (recurse e);
          json_field "field" (json_string field);
        ]
  | CVariant (typ, con, payload) ->
      json_obj
        [
          json_field "tag" (json_string "Variant");
          json_field "type" (type_to_graph_json typ);
          json_field "constructor" (json_string con);
          json_field "payload" (recurse payload);
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
          json_field "scrutinee" (recurse scrutinee);
          json_field "branches" (json_array (cbranch_to_graph_json_via recurse def_id_of) branches);
        ]
  | CFoldNat (n, zero, step) ->
      json_obj
        [
          json_field "tag" (json_string "FoldNat");
          json_field "index" (recurse n);
          json_field "zero" (recurse zero);
          json_field "step" (recurse step);
        ]
  | CFoldVariant (target, result, scrutinee, branches) ->
      json_obj
        [
          json_field "tag" (json_string "FoldVariant");
          json_field "targetType" (type_to_graph_json target);
          json_field "resultType" (type_to_graph_json result);
          json_field "scrutinee" (recurse scrutinee);
          json_field "branches" (json_array (cbranch_to_graph_json_via recurse def_id_of) branches);
        ]
  | CRecur e ->
      json_obj
        [
          json_field "tag" (json_string "Recur");
          json_field "value" (recurse e);
        ]
  | CNil typ ->
      json_obj [ json_field "tag" (json_string "Nil"); json_field "type" (type_to_graph_json typ) ]
  | CCons (typ, head, tail) ->
      json_obj
        [
          json_field "tag" (json_string "Cons");
          json_field "type" (type_to_graph_json typ);
          json_field "head" (recurse head);
          json_field "tail" (recurse tail);
        ]
  | CFoldList (xs, zero, step) ->
      json_obj
        [
          json_field "tag" (json_string "FoldList");
          json_field "list" (recurse xs);
          json_field "zero" (recurse zero);
          json_field "step" (recurse step);
        ]
  | CCaseList (xs, nil_body, cons_body) ->
      json_obj
        [
          json_field "tag" (json_string "CaseList");
          json_field "list" (recurse xs);
          json_field "nil" (recurse nil_body);
          json_field "cons" (recurse cons_body);
        ]
  | CCoiter (state_ty, item_ty, seed, step) ->
      json_obj
        [
          json_field "tag" (json_string "Coiter");
          json_field "stateType" (type_to_graph_json state_ty);
          json_field "itemType" (type_to_graph_json item_ty);
          json_field "seed" (recurse seed);
          json_field "step" (recurse step);
        ]
  | CStreamHead stream ->
      json_obj [ json_field "tag" (json_string "StreamHead"); json_field "stream" (recurse stream) ]
  | CStreamTail stream ->
      json_obj [ json_field "tag" (json_string "StreamTail"); json_field "stream" (recurse stream) ]
  | CStreamTake (count, stream) ->
      json_obj
        [
          json_field "tag" (json_string "StreamTake");
          json_field "count" (recurse count);
          json_field "stream" (recurse stream);
        ]
  | CAutomaton (state_ty, output_ty, initial, transition) ->
      json_obj
        [
          json_field "tag" (json_string "Automaton");
          json_field "stateType" (type_to_graph_json state_ty);
          json_field "outputType" (type_to_graph_json output_ty);
          json_field "initial" (recurse initial);
          json_field "transition" (recurse transition);
        ]
  | CAutomatonRun (count, automaton) ->
      json_obj
        [
          json_field "tag" (json_string "AutomatonRun");
          json_field "count" (recurse count);
          json_field "automaton" (recurse automaton);
        ]
  | CText e ->
      json_obj [ json_field "tag" (json_string "Text"); json_field "value" (recurse e) ]
  | CImage (src, alt) ->
      json_obj
        [
          json_field "tag" (json_string "Image");
          json_field "src" (recurse src);
          json_field "alt" (recurse alt);
        ]
  | CButton (label, msg) ->
      json_obj
        [
          json_field "tag" (json_string "Button");
          json_field "label" (recurse label);
          json_field "message" (recurse msg);
        ]
  | CInput (value, handler) ->
      json_obj
        [
          json_field "tag" (json_string "Input");
          json_field "value" (recurse value);
          json_field "handler" (recurse handler);
        ]
  | CColumn children ->
      json_obj
        [ json_field "tag" (json_string "Column"); json_field "children" (recurse children) ]
  | CRow children ->
      json_obj
        [ json_field "tag" (json_string "Row"); json_field "children" (recurse children) ]
  | CListView (items, render) ->
      json_obj
        [
          json_field "tag" (json_string "ListView");
          json_field "items" (recurse items);
          json_field "render" (recurse render);
        ]
  | CWhenView (cond, view) ->
      json_obj
        [
          json_field "tag" (json_string "WhenView");
          json_field "condition" (recurse cond);
          json_field "view" (recurse view);
        ]
  | CNode (tag, attrs, children) ->
      json_obj
        [
          json_field "tag" (json_string "Node");
          json_field "tagName" (recurse tag);
          json_field "attributes" (recurse attrs);
          json_field "children" (recurse children);
        ]
  | CAttr (name, value) ->
      json_obj
        [
          json_field "tag" (json_string "Attr");
          json_field "name" (recurse name);
          json_field "value" (recurse value);
        ]
  | COn (event, msg) ->
      json_obj
        [
          json_field "tag" (json_string "On");
          json_field "event" (recurse event);
          json_field "message" (recurse msg);
        ]
  | CDone e ->
      json_obj [ json_field "tag" (json_string "Done"); json_field "value" (recurse e) ]
  | CRequest req ->
      json_obj [ json_field "tag" (json_string "Request"); json_field "request" (req_to_graph_json req) ]
  | CBind (p, typ, body) ->
      json_obj
        [
          json_field "tag" (json_string "Bind");
          json_field "process" (recurse p);
          json_field "valueType" (type_to_graph_json typ);
          json_field "body" (recurse body);
        ]

and cbranch_to_graph_json_via recurse _def_id_of = function
  | CBBool (b, body) ->
      json_obj
        [
          json_field "tag" (json_string "BoolBranch");
          json_field "value" (json_bool b);
          json_field "body" (recurse body);
        ]
  | CBVariant (con, body) ->
      json_obj
        [
          json_field "tag" (json_string "VariantBranch");
          json_field "constructor" (json_string con);
          json_field "body" (recurse body);
        ]

let rec cterm_to_graph_json def_id_of term =
  cterm_to_graph_json_via (cterm_to_graph_json def_id_of) def_id_of term

let type_node_tag = function
  | TUnit -> "Unit"
  | TBool -> "Bool"
  | TNat -> "Nat"
  | TString -> "String"
  | TFun _ -> "Fun"
  | TRecord _ -> "Record"
  | TVariant _ -> "Variant"
  | TList _ -> "List"
  | TView _ -> "View"
  | TAttr _ -> "Attr"
  | TStream _ -> "Stream"
  | TAutomaton _ -> "Automaton"
  | TProcess _ -> "Process"
  | TCmd _ -> "Cmd"
  | TSecretRef _ -> "SecretRef"
  | TVar _ -> "TypeVar"
  | TForall _ -> "Forall"
  | TNamed _ -> "Named"

let cterm_node_tag = function
  | CUnit -> "Unit"
  | CBool _ -> "Bool"
  | CNat _ -> "Nat"
  | CString _ -> "String"
  | CVar _ -> "Var"
  | CGlobal n when is_builtin n -> "Builtin"
  | CGlobal _ -> "Ref"
  | CLambda _ -> "Lambda"
  | CApp _ -> "App"
  | CStrict _ -> "Strict"
  | CLet _ -> "Let"
  | CRecord _ -> "Record"
  | CField _ -> "Field"
  | CVariant _ -> "Variant"
  | CInst _ -> "Inst"
  | CCase _ -> "Case"
  | CFoldNat _ -> "FoldNat"
  | CFoldVariant _ -> "FoldVariant"
  | CRecur _ -> "Recur"
  | CNil _ -> "Nil"
  | CCons _ -> "Cons"
  | CFoldList _ -> "FoldList"
  | CCaseList _ -> "CaseList"
  | CCoiter _ -> "Coiter"
  | CStreamHead _ -> "StreamHead"
  | CStreamTail _ -> "StreamTail"
  | CStreamTake _ -> "StreamTake"
  | CAutomaton _ -> "Automaton"
  | CAutomatonRun _ -> "AutomatonRun"
  | CText _ -> "Text"
  | CImage _ -> "Image"
  | CButton _ -> "Button"
  | CInput _ -> "Input"
  | CColumn _ -> "Column"
  | CRow _ -> "Row"
  | CListView _ -> "ListView"
  | CWhenView _ -> "WhenView"
  | CNode _ -> "Node"
  | CAttr _ -> "Attr"
  | COn _ -> "On"
  | CDone _ -> "Done"
  | CRequest _ -> "Request"
  | CBind _ -> "Bind"

(* Edge builders are parameterized over the node-id functions so graph
   construction can plug memoized variants; the unparameterized forms below
   keep the existing behavior for every other caller. *)
let type_node_edges_via type_id = function
  | TUnit | TBool | TNat | TString | TVar _ -> []
  | TFun (a, b) -> [ type_id a; type_id b ]
  | TRecord fields | TVariant fields -> fields |> List.map (fun (_, t) -> type_id t)
  | TList t | TView t | TAttr t | TStream t | TProcess (_, t) | TCmd (_, t)
  | TSecretRef (_, t) ->
      [ type_id t ]
  | TAutomaton (state, output) -> [ type_id state; type_id output ]
  | TForall (_, body) -> [ type_id body ]
  | TNamed (_, args) -> List.map type_id args

let type_node_edges = type_node_edges_via type_node_id

let cbranch_body_edges_via term_id = function
  | CBBool (_, body) | CBVariant (_, body) -> [ term_id body ]

let cterm_node_edges_via ~term_id ~type_id = function
  | CUnit | CBool _ | CNat _ | CString _ | CVar _ | CGlobal _ | CRequest _ -> []
  | CLambda (typ, body) -> [ type_id typ; term_id body ]
  | CStrict value -> [ term_id value ]
  | CApp (f, arg) -> [ term_id f; term_id arg ]
  | CLet (value, body) -> [ term_id value; term_id body ]
  | CRecord fields -> List.map (fun (_, value) -> term_id value) fields
  | CField (record, _) -> [ term_id record ]
  | CVariant (typ, _, payload) -> [ type_id typ; term_id payload ]
  | CInst (_, args) -> List.map type_id args
  | CCase (scrutinee, branches) ->
      term_id scrutinee :: List.concat_map (cbranch_body_edges_via term_id) branches
  | CFoldNat (index, zero, step) -> [ term_id index; term_id zero; term_id step ]
  | CFoldVariant (target, result, scrutinee, branches) ->
      type_id target :: type_id result :: term_id scrutinee
      :: List.concat_map (cbranch_body_edges_via term_id) branches
  | CRecur value -> [ term_id value ]
  | CNil typ -> [ type_id typ ]
  | CCons (typ, head, tail) -> [ type_id typ; term_id head; term_id tail ]
  | CFoldList (list, zero, step) -> [ term_id list; term_id zero; term_id step ]
  | CCaseList (list, nil_body, cons_body) ->
      [ term_id list; term_id nil_body; term_id cons_body ]
  | CCoiter (state_ty, item_ty, seed, step) ->
      [ type_id state_ty; type_id item_ty; term_id seed; term_id step ]
  | CStreamHead stream | CStreamTail stream -> [ term_id stream ]
  | CStreamTake (count, stream) -> [ term_id count; term_id stream ]
  | CAutomaton (state_ty, output_ty, initial, transition) ->
      [ type_id state_ty; type_id output_ty; term_id initial; term_id transition ]
  | CAutomatonRun (count, automaton) -> [ term_id count; term_id automaton ]
  | CText value | CColumn value | CRow value | CDone value -> [ term_id value ]
  | CImage (src, alt) | CButton (src, alt) | CInput (src, alt)
  | CListView (src, alt) | CWhenView (src, alt) | CAttr (src, alt) | COn (src, alt) ->
      [ term_id src; term_id alt ]
  | CNode (tag, attrs, children) -> [ term_id tag; term_id attrs; term_id children ]
  | CBind (process, typ, body) -> [ term_id process; type_id typ; term_id body ]

let cbranch_body_edges def_id_of = cbranch_body_edges_via (term_node_id def_id_of)

let cterm_node_edges def_id_of =
  cterm_node_edges_via ~term_id:(term_node_id def_id_of) ~type_id:type_node_id

let canonical_node_json id kind tag canonical payload edges =
  json_obj
    [
      json_field "id" (json_string id);
      json_field "kind" (json_string kind);
      json_field "tag" (json_string tag);
      json_field "canonical" (json_string canonical);
      json_field "payload" payload;
      json_field "edgeRefs" (json_array json_string (uniq_strings edges));
    ]

(* Node ids hash the full canonical serialization of each subterm, and edge
   construction needs the ids of every child again. Memoizing serializations
   and ids by physical node identity inside one graph construction removes the
   repeated full-subtree serializations (the output stays byte-identical). *)
let canonical_node_graph_json_uncached program_hash def_id_of defs =
  let nodes = Hashtbl.create 128 in
  let term_canon_memo = Phys_cterm_tbl.create 1024 in
  let term_canon term =
    match Phys_cterm_tbl.find_opt term_canon_memo term with
    | Some canonical -> canonical
    | None ->
        let canonical = cterm_to_canonical_v2 def_id_of term in
        Phys_cterm_tbl.add term_canon_memo term canonical;
        canonical
  in
  let term_id_memo = Phys_cterm_tbl.create 1024 in
  let term_id term =
    match Phys_cterm_tbl.find_opt term_id_memo term with
    | Some id -> id
    | None ->
        let id = canonical_node_id "Term" (term_canon term) in
        Phys_cterm_tbl.add term_id_memo term id;
        id
  in
  let type_canon_memo = Phys_typ_tbl.create 256 in
  let type_canon typ =
    match Phys_typ_tbl.find_opt type_canon_memo typ with
    | Some canonical -> canonical
    | None ->
        let canonical = type_to_canonical typ in
        Phys_typ_tbl.add type_canon_memo typ canonical;
        canonical
  in
  let type_id_memo = Phys_typ_tbl.create 256 in
  let type_id typ =
    match Phys_typ_tbl.find_opt type_id_memo typ with
    | Some id -> id
    | None ->
        let id = canonical_node_id "Type" (type_canon typ) in
        Phys_typ_tbl.add type_id_memo typ id;
        id
  in
  let term_json_memo = Phys_cterm_tbl.create 1024 in
  let rec term_json term =
    match Phys_cterm_tbl.find_opt term_json_memo term with
    | Some json -> json
    | None ->
        let json = cterm_to_graph_json_via term_json def_id_of term in
        Phys_cterm_tbl.add term_json_memo term json;
        json
  in
  let rec add_type typ =
    let canonical = type_canon typ in
    let id = type_id typ in
    if not (Hashtbl.mem nodes id) then (
      add_type_children typ;
      Hashtbl.add nodes id
        (canonical_node_json id "Type" (type_node_tag typ) canonical (type_to_graph_json typ)
           (type_node_edges_via type_id typ)))
  and add_type_children = function
    | TUnit | TBool | TNat | TString | TVar _ -> ()
    | TFun (a, b) ->
        add_type a;
        add_type b
    | TRecord fields | TVariant fields -> List.iter (fun (_, t) -> add_type t) fields
    | TList t | TView t | TAttr t | TStream t | TProcess (_, t) | TCmd (_, t)
    | TSecretRef (_, t)
    | TForall (_, t) ->
        add_type t
    | TAutomaton (state, output) ->
        add_type state;
        add_type output
    | TNamed (_, args) -> List.iter add_type args
  in
  let rec add_term term =
    let canonical = term_canon term in
    let id = term_id term in
    if not (Hashtbl.mem nodes id) then (
      add_term_children term;
      Hashtbl.add nodes id
        (canonical_node_json id "Term" (cterm_node_tag term) canonical
           (term_json term)
           (cterm_node_edges_via ~term_id ~type_id term)))
  and add_branch = function
    | CBBool (_, body) | CBVariant (_, body) -> add_term body
  and add_term_children = function
    | CUnit | CBool _ | CNat _ | CString _ | CVar _ | CGlobal _ | CRequest _ -> ()
    | CStrict value -> add_term value
    | CLambda (typ, body) ->
        add_type typ;
        add_term body
    | CApp (f, arg) | CImage (f, arg) | CButton (f, arg) | CInput (f, arg)
    | CListView (f, arg) | CWhenView (f, arg) | CAttr (f, arg) | COn (f, arg) ->
        add_term f;
        add_term arg
    | CNode (tag, attrs, children) ->
        add_term tag;
        add_term attrs;
        add_term children
    | CLet (value, body) ->
        add_term value;
        add_term body
    | CRecord fields -> List.iter (fun (_, value) -> add_term value) fields
    | CField (record, _) -> add_term record
    | CVariant (typ, _, payload) ->
        add_type typ;
        add_term payload
    | CInst (_, args) -> List.iter add_type args
    | CCase (scrutinee, branches) ->
        add_term scrutinee;
        List.iter add_branch branches
    | CFoldNat (index, zero, step) ->
        add_term index;
        add_term zero;
        add_term step
    | CFoldVariant (target, result, scrutinee, branches) ->
        add_type target;
        add_type result;
        add_term scrutinee;
        List.iter add_branch branches
    | CRecur value -> add_term value
    | CNil typ -> add_type typ
    | CCons (typ, head, tail) ->
        add_type typ;
        add_term head;
        add_term tail
    | CFoldList (list, zero, step) ->
        add_term list;
        add_term zero;
        add_term step
    | CCaseList (list, nil_body, cons_body) ->
        add_term list;
        add_term nil_body;
        add_term cons_body
    | CCoiter (state_ty, item_ty, seed, step) ->
        add_type state_ty;
        add_type item_ty;
        add_term seed;
        add_term step
    | CStreamHead stream | CStreamTail stream -> add_term stream
    | CStreamTake (count, stream) ->
        add_term count;
        add_term stream
    | CAutomaton (state_ty, output_ty, initial, transition) ->
        add_type state_ty;
        add_type output_ty;
        add_term initial;
        add_term transition
    | CAutomatonRun (count, automaton) ->
        add_term count;
        add_term automaton
    | CText value | CColumn value | CRow value | CDone value -> add_term value
    | CBind (process, typ, body) ->
        add_term process;
        add_type typ;
        add_term body
  in
  List.iter
    (fun (d : canonical_def) ->
      add_type d.ctyp;
      add_term d.cbody)
    defs;
  let node_json =
    nodes |> Hashtbl.to_seq |> List.of_seq |> List.sort (fun (a, _) (b, _) -> String.compare a b)
    |> List.map snd
  in
  let def_refs =
    defs
    |> List.sort (fun a b -> String.compare a.cname b.cname)
    |> List.map (fun d ->
           json_obj
             [
               json_field "name" (json_string d.cname);
               json_field "defId" (json_string d.cdef_id);
               json_field "typeRef" (json_string (type_id d.ctyp));
               json_field "termRef" (json_string (term_id d.cbody));
             ])
  in
  json_obj
    [
      json_field "version" (json_string canonical_node_graph_version);
      json_field "hashAlgorithm" (json_string hash_algorithm);
      json_field "hashPrefix" (json_string hash_prefix);
      json_field "rootProgramHash" (json_string program_hash);
      json_field "defs" ("[" ^ String.concat ", " def_refs ^ "]");
      json_field "nodes" ("[" ^ String.concat ", " node_json ^ "]");
    ]

(* Audit, patch and locked-build flows re-check the same program text under
   fresh physical identities, so the physical-identity memos above miss and
   the multi-megabyte node graph used to be rebuilt from scratch each time.
   The canonical program hash determines the node graph byte-for-byte (the
   whole content-addressed design rests on that), so it is a sound content
   key; every caller already pays for [hash_program]. Two slots cover a
   program and its patched variant without retaining more large strings. *)
let node_graph_by_hash_memo : (string * string) list ref = ref []

let canonical_node_graph_json program_hash def_id_of defs =
  match
    List.find_opt (fun (h, _) -> String.equal h program_hash) !node_graph_by_hash_memo
  with
  | Some (_, json) -> json
  | None ->
      let json = canonical_node_graph_json_uncached program_hash def_id_of defs in
      node_graph_by_hash_memo :=
        (program_hash, json) :: List.filteri (fun i _ -> i < 1) !node_graph_by_hash_memo;
      json

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
  | Sexp.List [ Sexp.Atom "Process"; t ] -> TProcess (None, type_of_canonical_sexp t)
  | Sexp.List [ Sexp.Atom "Process"; Sexp.List (Sexp.Atom "Capabilities" :: caps); t ] ->
      let caps =
        caps
        |> List.map (function
             | Sexp.Atom cap | Sexp.Str cap -> cap
             | x -> fail ("invalid Process capability in canonical type: " ^ Sexp.to_string x))
        |> List.sort_uniq String.compare
      in
      TProcess (Some caps, type_of_canonical_sexp t)
  | Sexp.List [ Sexp.Atom "Cmd"; t ] -> TCmd (None, type_of_canonical_sexp t)
  | Sexp.List [ Sexp.Atom "Cmd"; Sexp.List (Sexp.Atom "Capabilities" :: caps); t ] ->
      let caps =
        caps
        |> List.map (function
             | Sexp.Atom cap | Sexp.Str cap -> cap
             | x -> fail ("invalid Cmd capability in canonical type: " ^ Sexp.to_string x))
        |> List.sort_uniq String.compare
      in
      TCmd (Some caps, type_of_canonical_sexp t)
  | Sexp.List [ Sexp.Atom "SecretRef"; Sexp.Atom scope; t ] ->
      TSecretRef (scope, type_of_canonical_sexp t)
  | Sexp.List [ Sexp.Atom "List"; t ] -> TList (type_of_canonical_sexp t)
  | Sexp.List [ Sexp.Atom "View"; t ] -> TView (type_of_canonical_sexp t)
  | Sexp.List [ Sexp.Atom "Attr"; t ] -> TAttr (type_of_canonical_sexp t)
  | Sexp.List [ Sexp.Atom "Stream"; t ] -> TStream (type_of_canonical_sexp t)
  | Sexp.List [ Sexp.Atom "Automaton"; state; output ] ->
      TAutomaton (type_of_canonical_sexp state, type_of_canonical_sexp output)
  | Sexp.List [ Sexp.Atom "TVar"; Sexp.Atom i ] -> TVar (int_of_string i)
  | Sexp.List [ Sexp.Atom "Forall"; Sexp.Atom arity; body ] ->
      TForall (int_of_string arity, type_of_canonical_sexp body)
  | Sexp.List (Sexp.Atom "Named" :: Sexp.Atom n :: args) ->
      TNamed (n, List.map type_of_canonical_sexp args)
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
  | Sexp.List [ Sexp.Atom "strict"; e ] -> CStrict (cterm_of_canonical_sexp e)
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
  | Sexp.List (Sexp.Atom "foldVariant" :: target :: result :: scrutinee :: branches) ->
      CFoldVariant
        ( type_of_canonical_sexp target,
          type_of_canonical_sexp result,
          cterm_of_canonical_sexp scrutinee,
          List.map cbranch_of_canonical_sexp branches )
  | Sexp.List [ Sexp.Atom "recur"; e ] -> CRecur (cterm_of_canonical_sexp e)
  | Sexp.List [ Sexp.Atom "Nil"; typ ] -> CNil (type_of_canonical_sexp typ)
  | Sexp.List [ Sexp.Atom "Cons"; typ; head; tail ] ->
      CCons (type_of_canonical_sexp typ, cterm_of_canonical_sexp head, cterm_of_canonical_sexp tail)
  | Sexp.List [ Sexp.Atom "foldList"; xs; zero; step ] ->
      CFoldList
        (cterm_of_canonical_sexp xs, cterm_of_canonical_sexp zero, cterm_of_canonical_sexp step)
  | Sexp.List
      [
        Sexp.Atom "caseList";
        xs;
        Sexp.List [ Sexp.Atom "Nil"; nil_body ];
        Sexp.List [ Sexp.Atom "Cons"; cons_body ];
      ] ->
      CCaseList
        ( cterm_of_canonical_sexp xs,
          cterm_of_canonical_sexp nil_body,
          cterm_of_canonical_sexp cons_body )
  | Sexp.List [ Sexp.Atom "coiter"; state_ty; item_ty; seed; step ] ->
      CCoiter
        ( type_of_canonical_sexp state_ty,
          type_of_canonical_sexp item_ty,
          cterm_of_canonical_sexp seed,
          cterm_of_canonical_sexp step )
  | Sexp.List [ Sexp.Atom "streamHead"; stream ] ->
      CStreamHead (cterm_of_canonical_sexp stream)
  | Sexp.List [ Sexp.Atom "streamTail"; stream ] ->
      CStreamTail (cterm_of_canonical_sexp stream)
  | Sexp.List [ Sexp.Atom "streamTake"; count; stream ] ->
      CStreamTake (cterm_of_canonical_sexp count, cterm_of_canonical_sexp stream)
  | Sexp.List [ Sexp.Atom "automaton"; state_ty; output_ty; initial; transition ] ->
      CAutomaton
        ( type_of_canonical_sexp state_ty,
          type_of_canonical_sexp output_ty,
          cterm_of_canonical_sexp initial,
          cterm_of_canonical_sexp transition )
  | Sexp.List [ Sexp.Atom "automatonRun"; count; automaton ] ->
      CAutomatonRun (cterm_of_canonical_sexp count, cterm_of_canonical_sexp automaton)
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
  | Sexp.List [ Sexp.Atom "node"; tag; attrs; children ] ->
      CNode
        ( cterm_of_canonical_sexp tag,
          cterm_of_canonical_sexp attrs,
          cterm_of_canonical_sexp children )
  | Sexp.List [ Sexp.Atom "attr"; name; value ] ->
      CAttr (cterm_of_canonical_sexp name, cterm_of_canonical_sexp value)
  | Sexp.List [ Sexp.Atom "on"; event; msg ] ->
      COn (cterm_of_canonical_sexp event, cterm_of_canonical_sexp msg)
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
  capabilities : string list;
}

type checked = {
  program : program;
  defs : checked_def list;
}

let secret_leak_risks checked =
  let has cap caps = List.exists (String.equal cap) caps in
  let outbound_sinks caps =
    [ "Http.get"; "Server.request" ] |> List.filter (fun cap -> has cap caps)
  in
  let risk_for_scope scope caps =
    let caps = List.sort_uniq String.compare caps in
    let sinks = outbound_sinks caps in
    if has "Local.storage" caps && sinks <> [] then
      [
        "SecretLeakRisk scope=" ^ scope ^ " sources=[Local.storage] sinks=["
        ^ String.concat "," sinks ^ "]";
      ]
    else []
  in
  let program_risks = risk_for_scope "program" checked.program.capabilities in
  let def_risks =
    checked.defs
    |> List.concat_map (fun d -> risk_for_scope ("def:" ^ d.def.name) d.capabilities)
  in
  program_risks @ def_risks

let ensure_unique_canonical_defs defs =
  let names = Hashtbl.create 32 in
  List.iter
    (fun d ->
      if is_builtin d.cname then fail ("canonical definition shadows builtin: " ^ d.cname);
      if Hashtbl.mem names d.cname then fail ("duplicate canonical definition: " ^ d.cname);
      Hashtbl.add names d.cname ())
    defs

let canonical_def_by_ref defs ref =
  List.find_opt (fun d -> String.equal d.cname ref || String.equal d.cdef_id ref) defs

let canonical_def_id_of defs ref =
  if is_builtin ref then "builtin:" ^ ref
  else
    match canonical_def_by_ref defs ref with
    | Some d -> d.cdef_id
    | None -> fail ("canonical graph references missing definition: " ^ ref)

let canonical_def_name_of defs ref =
  if is_builtin ref then ref
  else
    match canonical_def_by_ref defs ref with
    | Some d -> d.cname
    | None -> fail ("canonical graph references missing definition: " ^ ref)

let canonical_type_params = function
  | TForall (arity, _) -> List.init arity (fun i -> "T" ^ string_of_int i)
  | _ -> []

let canonical_surface_expr defs term =
  let def_name_of = canonical_def_name_of defs in
  let rec nth env i =
    match (env, i) with
    | x :: _, 0 -> x
    | _ :: rest, n when n > 0 -> nth rest (n - 1)
    | _ -> fail ("canonical term has unbound variable #" ^ string_of_int i)
  in
  let rec expr depth env = function
    | CUnit -> EUnit
    | CBool b -> EBool b
    | CNat n -> ENat n
    | CString s -> EString s
    | CVar i -> EName (nth env i)
    | CGlobal ref -> EName (def_name_of ref)
    | CLambda (typ, body) ->
        let x = "__x" ^ string_of_int depth in
        ELambda (x, typ, expr (depth + 1) (x :: env) body)
    | CApp (f, arg) -> EApp (expr depth env f, expr depth env arg)
    | CStrict value -> EStrict (expr depth env value)
    | CLet (value, body) ->
        let x = "__let" ^ string_of_int depth in
        ELet (x, expr depth env value, expr (depth + 1) (x :: env) body)
    | CRecord fields ->
        ERecord (sort_fields (List.map (fun (name, value) -> (name, expr depth env value)) fields))
    | CField (record, field) -> EField (expr depth env record, field)
    | CVariant (typ, con, value) -> EVariant (typ, con, expr depth env value)
    | CInst (ref, args) -> EInst (def_name_of ref, args)
    | CCase (scrut, branches) -> ECase (expr depth env scrut, List.map (branch depth env) branches)
    | CFoldNat (n, zero, step) ->
        EFoldNat (expr depth env n, expr depth env zero, expr depth env step)
    | CFoldVariant (target, result, scrut, branches) ->
        EFoldVariant
          (target, result, expr depth env scrut, List.map (branch depth env) branches)
    | CRecur value -> ERecur (expr depth env value)
    | CNil typ -> ENil typ
    | CCons (typ, head, tail) -> ECons (typ, expr depth env head, expr depth env tail)
    | CFoldList (xs, zero, step) ->
        EFoldList (expr depth env xs, expr depth env zero, expr depth env step)
    | CCaseList (xs, nil_body, cons_body) ->
        let head = "__head" ^ string_of_int depth in
        let tail = "__tail" ^ string_of_int depth in
        ECaseList
          ( expr depth env xs,
            expr depth env nil_body,
            head,
            tail,
            expr (depth + 1) (head :: tail :: env) cons_body )
    | CCoiter (state_ty, item_ty, seed, step) ->
        ECoiter (state_ty, item_ty, expr depth env seed, expr depth env step)
    | CStreamHead stream -> EStreamHead (expr depth env stream)
    | CStreamTail stream -> EStreamTail (expr depth env stream)
    | CStreamTake (count, stream) -> EStreamTake (expr depth env count, expr depth env stream)
    | CAutomaton (state_ty, output_ty, initial, transition) ->
        EAutomaton
          (state_ty, output_ty, expr depth env initial, expr depth env transition)
    | CAutomatonRun (count, automaton) ->
        EAutomatonRun (expr depth env count, expr depth env automaton)
    | CText value -> EText (expr depth env value)
    | CImage (src, alt) -> EImage (expr depth env src, expr depth env alt)
    | CButton (label, msg) -> EButton (expr depth env label, expr depth env msg)
    | CInput (value, handler) -> EInput (expr depth env value, expr depth env handler)
    | CColumn children -> EColumn (expr depth env children)
    | CRow children -> ERow (expr depth env children)
    | CListView (items, render) -> EListView (expr depth env items, expr depth env render)
    | CWhenView (cond, view) -> EWhenView (expr depth env cond, expr depth env view)
    | CNode (tag, attrs, children) ->
        ENode (expr depth env tag, expr depth env attrs, expr depth env children)
    | CAttr (name, value) -> EAttr (expr depth env name, expr depth env value)
    | COn (event, msg) -> EOn (expr depth env event, expr depth env msg)
    | CDone value -> EDone (expr depth env value)
    | CRequest req -> ERequest req
    | CBind (process, typ, body) ->
        let x = "__bind" ^ string_of_int depth in
        EBind (expr depth env process, x, typ, expr (depth + 1) (x :: env) body)
  and branch depth env = function
    | CBBool (b, body) -> BBool (b, expr depth env body)
    | CBVariant (con, body) ->
        let payload = "__payload" ^ string_of_int depth in
        BVariant (con, payload, expr (depth + 1) (payload :: env) body)
  in
  expr 0 [] term

let validate_canonical_refs defs =
  List.iter
    (fun d ->
      cterm_global_refs d.cbody
      |> List.iter (fun ref -> ignore (canonical_def_id_of defs ref)))
    defs

let validate_canonical_def_ids defs =
  List.iter
    (fun d ->
      let body = cterm_to_canonical_v2 (canonical_def_id_of defs) d.cbody in
      let expected = hash_string ("defid-v2:" ^ type_to_canonical d.ctyp ^ ":" ^ body) in
      if not (String.equal expected d.cdef_id) then
        fail
          ("canonical DefId mismatch for " ^ d.cname ^ ": expected " ^ expected ^ ", got "
         ^ d.cdef_id))
    defs

let canonical_capabilities_of_defs caps defs =
  let declared cap = List.exists (String.equal cap) caps in
  let memo = Hashtbl.create 32 in
  let visiting = Hashtbl.create 32 in
  let rec capabilities_of_ref ref =
    match canonical_def_by_ref defs ref with
    | Some d -> capabilities_of_def d
    | None -> if is_builtin ref then [] else fail ("canonical graph references missing definition: " ^ ref)
  and capabilities_of_def d =
    match Hashtbl.find_opt memo d.cdef_id with
    | Some caps -> caps
    | None ->
        if Hashtbl.mem visiting d.cdef_id then
          fail ("canonical graph cyclic capability dependency: " ^ d.cname);
        Hashtbl.add visiting d.cdef_id ();
        let direct = cterm_direct_capabilities d.cbody in
        let inherited = cterm_global_refs d.cbody |> List.concat_map capabilities_of_ref in
        let actual = List.sort_uniq String.compare (direct @ inherited) in
        List.iter
          (fun cap ->
            if not (declared cap) then
              fail
                ("canonical graph uses undeclared capability in " ^ d.cname ^ ": " ^ cap))
          actual;
        Hashtbl.remove visiting d.cdef_id;
        Hashtbl.add memo d.cdef_id actual;
        actual
  in
  List.iter (fun d -> ignore (capabilities_of_def d)) defs;
  fun d -> capabilities_of_def d

let checked_of_canonical caps defs =
  let caps = List.sort_uniq String.compare caps in
  validate_capabilities caps;
  ensure_unique_canonical_defs defs;
  validate_canonical_refs defs;
  validate_canonical_def_ids defs;
  let capabilities_of = canonical_capabilities_of_defs caps defs in
  let checked_defs =
    defs
    |> List.sort (fun a b -> String.compare a.cname b.cname)
    |> List.map (fun d ->
           let canonical = serialize_def d.cname d.cdef_id d.ctyp d.cbody (canonical_def_id_of defs) in
           {
             def =
               {
                 name = d.cname;
                 type_params = canonical_type_params d.ctyp;
                 declared_capabilities = None;
                 typ = d.ctyp;
                 body = canonical_surface_expr defs d.cbody;
               };
             def_id = d.cdef_id;
             cterm = d.cbody;
             canonical;
             hash = hash_string canonical;
             capabilities = capabilities_of d;
           })
  in
  {
    program =
      {
        imports = [];
        capabilities = caps;
        module_name = None;
        exports = None;
        type_aliases = [];
        defs = List.map (fun d -> d.def) checked_defs;
      };
    defs = checked_defs;
  }

(* Per-definition incremental checking. Elaborating one definition depends
   only on the definition itself plus what it actually consults in the
   type-level environment - never on other definitions' bodies. The cache key
   has two parts:

   - a static key: digest of the resolved definition AST, the program's type
     aliases (consulted via [unfold_type]), its declared capabilities
     (consulted via [has_capability]), and the rare "__record*" global names
     (consulted by the letRecord fresh-binder desugaring);
   - a dynamic part: the trace of every [lookup_global] the elaborator made,
     recorded via [global_lookup_trace]. A cached elaboration is reusable in
     a new program iff each recorded lookup yields the same binding there -
     so adding, removing or editing *unrelated* definitions never
     invalidates, while changing the type of a referenced one always does.

   Audit/patch/build flows re-check programs sharing almost every definition
   with one this process already elaborated, and the typical edit changes a
   single body. Only successful elaborations are cached - failing
   definitions re-run and keep their exact error messages. Reusing one cterm
   per content also feeds the physical-identity memos downstream (graphs,
   serializations). *)
type def_elab_entry = {
  entry_trace : (string * global_type option) list;
  entry_def : def;
  entry_cterm : cterm;
}

let def_elab_cache : (string, def_elab_entry list ref) Hashtbl.t = Hashtbl.create 1024

let def_elab_cache_cap = 8192

let def_elab_entries_cap = 4

(* Hit/miss counters for the per-def and whole-program check caches, dumped
   at exit when PROTOSS_PERF_STATS=1. Observability only - no behavior. *)
let def_elab_hits = ref 0

let def_elab_misses = ref 0

let check_program_hits = ref 0

let check_program_misses = ref 0

let () =
  at_exit (fun () ->
      if Sys.getenv_opt "PROTOSS_PERF_STATS" = Some "1" then
        Printf.eprintf
          "[perf] check_program memo: %d hits, %d misses; def elab cache: %d hits, %d misses\n%!"
          !check_program_hits !check_program_misses !def_elab_hits !def_elab_misses)

let check_program_uncached (program : program) =
  let program = resolve_program_types program in
  validate_capabilities program.capabilities;
  List.iter
    (fun (d : def) ->
      match d.declared_capabilities with
      | None -> ()
      | Some caps ->
          validate_capabilities caps;
          List.iter
            (fun cap ->
              if not (List.exists (String.equal cap) program.capabilities) then
                fail
                  ("definition " ^ d.name ^ ": declares undeclared capability: " ^ cap))
            caps)
    program.defs;
  check_duplicate_names program.defs;
  reject_cycles program.defs;
  let globals =
    List.map (fun d -> (d.name, { global_type_params = d.type_params; global_typ = d.typ })) program.defs
  in
  let ctx =
    {
      type_aliases = program.type_aliases;
      globals;
      capabilities = program.capabilities;
      locals = [];
      fold_scope = None;
    }
  in
  let record_binder_names =
    List.filter_map
      (fun (n, _) ->
        if String.length n >= 8 && String.equal (String.sub n 0 8) "__record" then Some n
        else None)
      globals
  in
  let static_env =
    Marshal.to_string (program.type_aliases, program.capabilities, record_binder_names) []
  in
  let elaborate_def d =
    try
      let expected = match d.typ with TForall (_, body) -> body | t -> t in
      let actual, body = check_elab ctx expected d.body in
      if not (equal_typ expected actual) then
        fail
          ("definition " ^ d.name ^ ": expected " ^ string_of_typ expected ^ ", got "
         ^ string_of_typ actual ^ ", expression " ^ string_of_expr d.body);
      { d with body }
    with Error msg ->
      fail ("definition " ^ d.name ^ ": " ^ msg ^ ", expression " ^ string_of_expr d.body)
  in
  let elaborated_defs =
    List.map
      (fun d ->
        let static_key = Hashcons.digest (Marshal.to_string d [] ^ static_env) in
        let entries =
          match Hashtbl.find_opt def_elab_cache static_key with
          | Some entries -> entries
          | None ->
              let entries = ref [] in
              if Hashtbl.length def_elab_cache >= def_elab_cache_cap then
                Hashtbl.reset def_elab_cache;
              Hashtbl.replace def_elab_cache static_key entries;
              entries
        in
        let trace_valid entry =
          List.for_all (fun (n, r) -> assoc_opt n ctx.globals = r) entry.entry_trace
        in
        match List.find_opt trace_valid !entries with
        | Some e ->
            incr def_elab_hits;
            (e.entry_def, e.entry_cterm)
        | None ->
            incr def_elab_misses;
            let trace_tbl = Hashtbl.create 64 in
            global_lookup_trace := Some trace_tbl;
            let elaborated =
              Fun.protect
                ~finally:(fun () -> global_lookup_trace := None)
                (fun () -> elaborate_def d)
            in
            let cterm = canonical_expr [] elaborated.body in
            let trace = Hashtbl.fold (fun n r acc -> (n, r) :: acc) trace_tbl [] in
            let entry = { entry_trace = trace; entry_def = elaborated; entry_cterm = cterm } in
            entries := entry :: List.filteri (fun i _ -> i < def_elab_entries_cap - 1) !entries;
            (elaborated, cterm))
      program.defs
  in
  let defs = List.map fst elaborated_defs in
  let program = { program with defs } in
  let cterms = Hashtbl.create 32 in
  List.iter (fun (d, cterm) -> Hashtbl.add cterms d.name cterm) elaborated_defs;
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
              let body = cterm_to_canonical_v2_cached def_id_of cterm in
              let id = hash_string ("defid-v2:" ^ type_to_canonical d.typ ^ ":" ^ body) in
              Hashtbl.add def_ids name id;
              id
          | _ -> fail ("unknown global definition in canonicalization: " ^ name))
  in
  let cap_memo = Hashtbl.create 32 in
  let rec capabilities_of_name name =
    if is_builtin name then []
    else
      match Hashtbl.find_opt cap_memo name with
      | Some caps -> caps
      | None -> (
          match Hashtbl.find_opt cterms name with
          | None -> []
          | Some cterm ->
              let direct = cterm_direct_capabilities cterm in
              let inherited =
                cterm_global_refs cterm |> List.concat_map capabilities_of_name
              in
              let caps = List.sort_uniq String.compare (direct @ inherited) in
              Hashtbl.add cap_memo name caps;
              caps)
  in
  let defs =
    List.map
      (fun d ->
        let cterm = Hashtbl.find cterms d.name in
        let def_id = def_id_of d.name in
        let c = serialize_def d.name def_id d.typ cterm def_id_of in
        let _ = Hashcons.intern c in
        let capabilities = capabilities_of_name d.name in
        (match d.declared_capabilities with
        | None -> ()
        | Some declared ->
            let declared = List.sort_uniq String.compare declared in
            if declared <> capabilities then
              fail
                ("definition " ^ d.name ^ ": capability scope mismatch: declared ["
               ^ String.concat ", " declared ^ "], actual ["
               ^ String.concat ", " capabilities ^ "]"));
        {
          def = d;
          def_id;
          cterm;
          canonical = c;
          hash = hash_string c;
          capabilities;
        })
      program.defs
  in
  { program; defs }

(* Checking is pure and total over the surface program, and audit/patch/
   locked-build flows re-load byte-identical programs from the store under
   fresh physical identities — re-running the whole elaborator each time
   dominated those flows. The key is the digest of the marshaled AST (plain
   data, sharing preserved): structurally equal programs built differently
   may miss (never incorrect), and only successful checks are cached, so
   failing programs keep their exact error behavior. Sharing one [checked]
   per content also feeds every physical-identity memo downstream. *)
let check_program_by_content_memo : (string * checked) list ref = ref []

let check_program (program : program) =
  let key = Hashcons.digest (Marshal.to_string program []) in
  match
    List.find_opt (fun (k, _) -> String.equal k key) !check_program_by_content_memo
  with
  | Some (_, checked) ->
      incr check_program_hits;
      checked
  | None ->
      incr check_program_misses;
      let checked = check_program_uncached program in
      check_program_by_content_memo :=
        (key, checked) :: List.filteri (fun i _ -> i < 3) !check_program_by_content_memo;
      checked

let canonical_defs_of_checked checked =
  let defs =
    checked.defs
    |> List.map (fun d -> { cname = d.def.name; cdef_id = d.def_id; ctyp = d.def.typ; cbody = d.cterm })
  in
  defs

(* The whole-program canonical serialization and its hash are requested by
   build ids, cache scopes, graph fields, locks and audits — re-serializing
   and re-hashing megabytes per call dominated those flows. Same few-slot,
   physical-identity memoization as the graph artifacts below. *)
let serialize_checked_memo : (checked * string) list ref = ref []

let serialize_checked_program checked =
  match List.find_opt (fun (c, _) -> c == checked) !serialize_checked_memo with
  | Some (_, serialized) -> serialized
  | None ->
      let serialized =
        serialize_program checked.program.capabilities (canonical_defs_of_checked checked)
      in
      serialize_checked_memo :=
        (checked, serialized) :: List.filteri (fun i _ -> i < 5) !serialize_checked_memo;
      serialized

let hash_program_memo : (checked * string) list ref = ref []

let hash_program checked =
  match List.find_opt (fun (c, _) -> c == checked) !hash_program_memo with
  | Some (_, hash) -> hash
  | None ->
      let hash = hash_string (serialize_checked_program checked) in
      hash_program_memo := (checked, hash) :: List.filteri (fun i _ -> i < 5) !hash_program_memo;
      hash

let checked_to_graph_json_fields_uncached ~version ~include_capability_scope_ref checked =
  let defs = checked.defs |> List.sort (fun a b -> String.compare a.def.name b.def.name) in
  let name_to_def_id = Hashtbl.create (List.length defs) in
  let preferred_name_by_def_id = Hashtbl.create (List.length defs) in
  List.iter
    (fun d ->
      Hashtbl.replace name_to_def_id d.def.name d.def_id;
      if not (Hashtbl.mem preferred_name_by_def_id d.def_id) then
        Hashtbl.add preferred_name_by_def_id d.def_id d.def.name)
    defs;
  let def_id_of name =
    if is_builtin name then "builtin:" ^ name
    else
      (* A known name maps to its def_id; a def_id (or unknown name) passes
         through unchanged — same result as the previous linear scan. *)
      match Hashtbl.find_opt name_to_def_id name with
      | Some id -> id
      | None -> name
  in
  let canonical_dependency_names term =
    cterm_global_refs term
    |> List.map (fun ref ->
           let def_id = Option.value (Hashtbl.find_opt name_to_def_id ref) ~default:ref in
           match Hashtbl.find_opt preferred_name_by_def_id def_id with
           | Some name -> name
           | None -> ref)
    |> List.sort_uniq String.compare
  in
  let def_json d =
    let canonical_payload = cterm_to_canonical_v2 def_id_of d.cterm in
    let capability_scope_refs =
      d.capabilities
      |> List.map (fun cap ->
             match capability_ref cap with
             | Some ref -> ref
             | None -> fail ("unknown capability in canonical graph scope: " ^ cap))
    in
    let scope_fields =
      [
        json_field "capabilityScope" (json_array json_string d.capabilities);
        json_field "capabilityScopeRefs" (json_array json_string capability_scope_refs);
      ]
      @ (if include_capability_scope_ref then
        [ json_field "capabilityScopeRef" (json_string (capability_scope_ref d.capabilities)) ]
        else [])
    in
    json_obj
      ([
         json_field "name" (json_string d.def.name);
         json_field "defId" (json_string d.def_id);
         json_field "hash" (json_string d.hash);
         json_field "typeRef" (json_string (type_node_id d.def.typ));
         json_field "termRef" (json_string (term_node_id def_id_of d.cterm));
       ]
      @ scope_fields
      @ [
          json_field "type" (type_to_graph_json d.def.typ);
          json_field "typeCanonical" (json_string (type_to_canonical d.def.typ));
          json_field "deps" (json_array json_string (canonical_dependency_names d.cterm));
          json_field "term" (cterm_to_graph_json def_id_of d.cterm);
          json_field "termCanonical" (json_string canonical_payload);
        ])
  in
  let declared_capabilities = List.sort_uniq String.compare checked.program.capabilities in
  let declared_capability_refs =
    declared_capabilities
    |> List.map (fun cap ->
           match capability_ref cap with
           | Some ref -> ref
           | None -> fail ("unknown capability in canonical graph: " ^ cap))
  in
  [
    json_field "version" (json_string version);
    json_field "canonicalVersion" (json_string canonical_version);
    json_field "hashAlgorithm" (json_string hash_algorithm);
    json_field "hashPrefix" (json_string hash_prefix);
    json_field "programHash" (json_string (hash_program checked));
    json_field "capabilities" (json_array json_string declared_capabilities);
    json_field "capabilityRefs" (json_array json_string declared_capability_refs);
    json_field "capabilityDescriptors" (capabilities_to_graph_json checked.program.capabilities);
    json_field "defs" (json_array def_json defs);
    json_field "nodeGraph"
      (canonical_node_graph_json (hash_program checked) def_id_of (canonical_defs_of_checked checked));
  ]

(* One build computes the graph content hash, serializes the graph for the
   store, and re-validates it — each call used to rebuild every field
   (including the quadratic node graph) from scratch. Fields are pure in
   [checked], so they are memoized by physical program identity; a few slots
   cover diff/package flows that juggle two or three programs at once. *)
let graph_json_fields_memo : (checked * string * bool * string list) list ref = ref []

(* Second-level cache by canonical program hash: audit/patch/locked flows
   re-check identical program text under fresh physical identities, and the
   def entries alone re-serialize every canonical body. [hash_program] is
   already computed by every caller (it is a field of the graph itself), so
   the content key costs nothing new. *)
let graph_json_fields_by_content_memo : (string * string * bool * string list) list ref = ref []

let checked_to_graph_json_fields ?(version = canonical_graph_version)
    ?(include_capability_scope_ref = true) checked =
  match
    List.find_opt
      (fun (c, v, i, _) ->
        c == checked && String.equal v version && Bool.equal i include_capability_scope_ref)
      !graph_json_fields_memo
  with
  | Some (_, _, _, fields) -> fields
  | None ->
      let program_hash = hash_program checked in
      let fields =
        match
          List.find_opt
            (fun (h, v, i, _) ->
              String.equal h program_hash && String.equal v version
              && Bool.equal i include_capability_scope_ref)
            !graph_json_fields_by_content_memo
        with
        | Some (_, _, _, fields) -> fields
        | None ->
            let fields =
              checked_to_graph_json_fields_uncached ~version ~include_capability_scope_ref checked
            in
            graph_json_fields_by_content_memo :=
              (program_hash, version, include_capability_scope_ref, fields)
              :: List.filteri (fun i _ -> i < 1) !graph_json_fields_by_content_memo;
            fields
      in
      graph_json_fields_memo :=
        (checked, version, include_capability_scope_ref, fields)
        :: List.filteri (fun i _ -> i < 5) !graph_json_fields_memo;
      fields

(* Lock, package and validation flows each need the multi-megabyte payload,
   its content hash and the final graph JSON for the same program; rebuilding
   and re-hashing the strings on every call dominated those flows, so all
   three are memoized alongside the fields (same few-slot, physical-identity
   scheme). *)
let graph_payload_memo : (checked * string * bool * string) list ref = ref []

let checked_to_graph_payload_json_for ~version ~include_capability_scope_ref checked =
  match
    List.find_opt
      (fun (c, v, i, _) ->
        c == checked && String.equal v version && Bool.equal i include_capability_scope_ref)
      !graph_payload_memo
  with
  | Some (_, _, _, payload) -> payload
  | None ->
      let payload =
        json_obj (checked_to_graph_json_fields ~version ~include_capability_scope_ref checked)
        ^ "\n"
      in
      graph_payload_memo :=
        (checked, version, include_capability_scope_ref, payload)
        :: List.filteri (fun i _ -> i < 5) !graph_payload_memo;
      payload

let checked_to_graph_payload_json checked =
  checked_to_graph_payload_json_for ~version:canonical_graph_version
    ~include_capability_scope_ref:true checked

let graph_content_hash_memo : (checked * string * bool * string) list ref = ref []

(* Content-keyed second level (same scheme as the fields cache): the graph
   content hash is a pure function of the canonical program, and re-checked
   identical programs would otherwise re-concatenate and re-hash megabytes.
   Keys and values are short strings, so a few slots cost nothing. *)
let graph_content_hash_by_content_memo : (string * string * bool * string) list ref = ref []

let checked_to_graph_content_hash_for ~version ~include_capability_scope_ref checked =
  match
    List.find_opt
      (fun (c, v, i, _) ->
        c == checked && String.equal v version && Bool.equal i include_capability_scope_ref)
      !graph_content_hash_memo
  with
  | Some (_, _, _, hash) -> hash
  | None ->
      let program_hash = hash_program checked in
      let hash =
        match
          List.find_opt
            (fun (h, v, i, _) ->
              String.equal h program_hash && String.equal v version
              && Bool.equal i include_capability_scope_ref)
            !graph_content_hash_by_content_memo
        with
        | Some (_, _, _, hash) -> hash
        | None ->
            let hash =
              hash_string
                (checked_to_graph_payload_json_for ~version ~include_capability_scope_ref checked)
            in
            graph_content_hash_by_content_memo :=
              (program_hash, version, include_capability_scope_ref, hash)
              :: List.filteri (fun i _ -> i < 5) !graph_content_hash_by_content_memo;
            hash
      in
      graph_content_hash_memo :=
        (checked, version, include_capability_scope_ref, hash)
        :: List.filteri (fun i _ -> i < 5) !graph_content_hash_memo;
      hash

let checked_to_graph_content_hash checked =
  checked_to_graph_content_hash_for ~version:canonical_graph_version
    ~include_capability_scope_ref:true checked

let graph_json_memo : (checked * string * bool * string) list ref = ref []

let checked_to_graph_json_for ~version ~include_capability_scope_ref checked =
  match
    List.find_opt
      (fun (c, v, i, _) ->
        c == checked && String.equal v version && Bool.equal i include_capability_scope_ref)
      !graph_json_memo
  with
  | Some (_, _, _, json) -> json
  | None ->
      let fields = checked_to_graph_json_fields ~version ~include_capability_scope_ref checked in
      let graph_hash =
        checked_to_graph_content_hash_for ~version ~include_capability_scope_ref checked
      in
      let program_hash_prefix = "\"programHash\"" in
      let program_hash_prefix_len = String.length program_hash_prefix in
      let rec insert_graph_hash = function
        | [] -> [ json_field "graphHash" (json_string graph_hash) ]
        | field :: rest
          when String.length field >= program_hash_prefix_len
               && String.sub field 0 program_hash_prefix_len = program_hash_prefix ->
            field :: json_field "graphHash" (json_string graph_hash) :: rest
        | field :: rest -> field :: insert_graph_hash rest
      in
      let json = json_obj (insert_graph_hash fields) ^ "\n" in
      graph_json_memo :=
        (checked, version, include_capability_scope_ref, json)
        :: List.filteri (fun i _ -> i < 5) !graph_json_memo;
      json

let checked_to_graph_json checked =
  checked_to_graph_json_for ~version:canonical_graph_version
    ~include_capability_scope_ref:true checked

let checked_to_graph_json_legacy_v1 checked =
  checked_to_graph_json_for ~version:canonical_graph_legacy_v1
    ~include_capability_scope_ref:false checked

let checked_to_graph_content_hash_legacy_v1 checked =
  checked_to_graph_content_hash_for ~version:canonical_graph_legacy_v1
    ~include_capability_scope_ref:false checked

let checked_def_by_name checked name =
  checked.defs |> List.find_opt (fun d -> String.equal d.def.name name || String.equal d.def_id name)

type termination_counts = {
  fold_nat : int;
  fold_list : int;
  fold_variant : int;
  recur : int;
}

type termination_static_size = {
  type_nodes : int;
  arrow_arity : int;
  sized_arguments : (int * string) list;
}

let termination_zero = { fold_nat = 0; fold_list = 0; fold_variant = 0; recur = 0 }

let termination_add a b =
  {
    fold_nat = a.fold_nat + b.fold_nat;
    fold_list = a.fold_list + b.fold_list;
    fold_variant = a.fold_variant + b.fold_variant;
    recur = a.recur + b.recur;
  }

let termination_inc_fold_nat c = { c with fold_nat = c.fold_nat + 1 }

let termination_inc_fold_list c = { c with fold_list = c.fold_list + 1 }

let termination_inc_fold_variant c = { c with fold_variant = c.fold_variant + 1 }

let termination_inc_recur c = { c with recur = c.recur + 1 }

let rec termination_counts_term = function
  | CUnit | CBool _ | CNat _ | CString _ | CVar _ | CGlobal _ | CNil _ | CRequest _ ->
      termination_zero
  | CLambda (_, body) | CStrict body | CField (body, _) | CVariant (_, _, body)
  | CText body | CColumn body | CRow body | CDone body ->
      termination_counts_term body
  | CRecur body -> termination_inc_recur (termination_counts_term body)
  | CApp (a, b) | CLet (a, b) | CCons (_, a, b) | CImage (a, b) | CButton (a, b)
  | CInput (a, b) | CListView (a, b) | CWhenView (a, b) | CAttr (a, b) | COn (a, b)
  | CBind (a, _, b) ->
      termination_add (termination_counts_term a) (termination_counts_term b)
  | CRecord fields ->
      List.fold_left
        (fun acc (_, value) -> termination_add acc (termination_counts_term value))
        termination_zero fields
  | CInst _ -> termination_zero
  | CCase (scrutinee, branches) ->
      termination_add (termination_counts_term scrutinee)
        (termination_counts_branches branches)
  | CFoldNat (index, zero, step) ->
      termination_inc_fold_nat
        (List.fold_left
           (fun acc term -> termination_add acc (termination_counts_term term))
           termination_zero [ index; zero; step ])
  | CFoldVariant (_, _, scrutinee, branches) ->
      termination_inc_fold_variant
        (termination_add (termination_counts_term scrutinee)
           (termination_counts_branches branches))
  | CFoldList (items, zero, step) ->
      termination_inc_fold_list
        (List.fold_left
           (fun acc term -> termination_add acc (termination_counts_term term))
           termination_zero [ items; zero; step ])
  | CCoiter (_, _, seed, step) | CAutomaton (_, _, seed, step) ->
      termination_add (termination_counts_term seed) (termination_counts_term step)
  | CStreamHead stream | CStreamTail stream -> termination_counts_term stream
  | CStreamTake (count, stream) | CAutomatonRun (count, stream) ->
      termination_add (termination_counts_term count) (termination_counts_term stream)
  | CCaseList (items, nil_body, cons_body) ->
      List.fold_left
        (fun acc term -> termination_add acc (termination_counts_term term))
        termination_zero [ items; nil_body; cons_body ]
  | CNode (tag, attrs, children) ->
      List.fold_left
        (fun acc term -> termination_add acc (termination_counts_term term))
        termination_zero [ tag; attrs; children ]

and termination_counts_branches branches =
  List.fold_left
    (fun acc -> function
      | CBBool (_, body) | CBVariant (_, body) ->
          termination_add acc (termination_counts_term body))
    termination_zero branches

let termination_status counts =
  if counts.recur > 0 then "structural-recursive"
  else if counts.fold_nat + counts.fold_list + counts.fold_variant > 0 then "structural-fold"
  else "trivial"

let rec type_node_count = function
  | TUnit | TBool | TNat | TString | TVar _ -> 1
  | TFun (a, b) -> 1 + type_node_count a + type_node_count b
  | TRecord fields | TVariant fields ->
      1 + List.fold_left (fun acc (_, t) -> acc + type_node_count t) 0 fields
  | TList t | TView t | TAttr t | TStream t | TProcess (_, t) | TCmd (_, t)
  | TSecretRef (_, t)
  | TForall (_, t) ->
      1 + type_node_count t
  | TAutomaton (state, output) -> 1 + type_node_count state + type_node_count output
  | TNamed (_, args) ->
      1 + List.fold_left (fun acc t -> acc + type_node_count t) 0 args

let rec curried_argument_types = function
  | TForall (_, body) -> curried_argument_types body
  | TFun (arg, result) -> arg :: curried_argument_types result
  | _ -> []

let rec type_static_measure = function
  | TNat -> Some "Nat.value"
  | TList _ -> Some "List.length"
  | TStream _ -> Some "Stream.productive"
  | TAutomaton _ -> Some "Automaton.productive"
  | TVariant _ -> Some "Variant.height"
  | TNamed (name, _) -> Some (name ^ ".height")
  | TRecord fields ->
      let field_measures =
        fields
        |> List.filter_map (fun (field, t) ->
               type_static_measure t |> Option.map (fun measure -> field ^ ":" ^ measure))
      in
      if field_measures = [] then None
      else Some ("Record{" ^ String.concat "," field_measures ^ "}")
  | TForall (_, body) -> type_static_measure body
  | _ -> None

let termination_static_size_type typ =
  let args = curried_argument_types typ in
  {
    type_nodes = type_node_count typ;
    arrow_arity = List.length args;
    sized_arguments =
      args
      |> List.mapi (fun i t -> (i, type_static_measure t))
      |> List.filter_map (fun (i, measure) -> measure |> Option.map (fun m -> (i, m)));
  }

let termination_static_sized_arguments_to_string = function
  | [] -> "none"
  | args ->
      args
      |> List.map (fun (i, measure) -> "arg" ^ string_of_int i ^ ":" ^ measure)
      |> String.concat ","

let termination_explanation_text checked name =
  match checked_def_by_name checked name with
  | None -> fail ("unknown definition for termination explanation: " ^ name)
  | Some def ->
      let counts = termination_counts_term def.cterm in
      let static_size = termination_static_size_type def.def.typ in
      let explanation =
        match termination_status counts with
        | "trivial" -> "no recursion or structural fold in canonical body"
        | "structural-recursive" ->
            "recursive calls are guarded by structural foldVariant recur"
        | _ -> "definition uses structural fold(s); general recursion is rejected before canonicalization"
      in
      "definition=" ^ def.def.name ^ "\ndef-id=" ^ def.def_id ^ "\ntype="
      ^ string_of_typ def.def.typ ^ "\nstatus=" ^ termination_status counts ^ "\nfoldNat="
      ^ string_of_int counts.fold_nat ^ "\nfoldList=" ^ string_of_int counts.fold_list
      ^ "\nfoldVariant=" ^ string_of_int counts.fold_variant ^ "\nrecur="
      ^ string_of_int counts.recur ^ "\nstaticTypeNodes="
      ^ string_of_int static_size.type_nodes ^ "\nstaticArrowArity="
      ^ string_of_int static_size.arrow_arity ^ "\nstaticSizedArguments="
      ^ termination_static_sized_arguments_to_string static_size.sized_arguments
      ^ "\nexplanation=" ^ explanation ^ "\n"

let rec shift amount cutoff = function
  | CUnit -> CUnit
  | CBool b -> CBool b
  | CNat n -> CNat n
  | CString s -> CString s
  | CVar i -> if i >= cutoff then CVar (i + amount) else CVar i
  | CGlobal n -> CGlobal n
  | CLambda (t, body) -> CLambda (t, shift amount (cutoff + 1) body)
  | CApp (f, x) -> CApp (shift amount cutoff f, shift amount cutoff x)
  | CStrict e -> CStrict (shift amount cutoff e)
  | CLet (e, body) -> CLet (shift amount cutoff e, shift amount (cutoff + 1) body)
  | CRecord fields -> CRecord (List.map (fun (n, e) -> (n, shift amount cutoff e)) fields)
  | CField (e, field) -> CField (shift amount cutoff e, field)
  | CVariant (t, con, e) -> CVariant (t, con, shift amount cutoff e)
  | CInst (name, args) -> CInst (name, args)
  | CCase (e, branches) -> CCase (shift amount cutoff e, List.map (shift_branch amount cutoff) branches)
  | CFoldNat (n, zero, step) ->
      CFoldNat (shift amount cutoff n, shift amount cutoff zero, shift amount cutoff step)
  | CFoldVariant (target, result, scrut, branches) ->
      CFoldVariant
        (target, result, shift amount cutoff scrut, List.map (shift_branch amount cutoff) branches)
  | CRecur e -> CRecur (shift amount cutoff e)
  | CNil t -> CNil t
  | CCons (t, head, tail) -> CCons (t, shift amount cutoff head, shift amount cutoff tail)
  | CFoldList (xs, zero, step) ->
      CFoldList (shift amount cutoff xs, shift amount cutoff zero, shift amount cutoff step)
  | CCaseList (xs, nil_body, cons_body) ->
      CCaseList
        (shift amount cutoff xs, shift amount cutoff nil_body, shift amount (cutoff + 2) cons_body)
  | CCoiter (state_ty, item_ty, seed, step) ->
      CCoiter (state_ty, item_ty, shift amount cutoff seed, shift amount cutoff step)
  | CStreamHead stream -> CStreamHead (shift amount cutoff stream)
  | CStreamTail stream -> CStreamTail (shift amount cutoff stream)
  | CStreamTake (count, stream) ->
      CStreamTake (shift amount cutoff count, shift amount cutoff stream)
  | CAutomaton (state_ty, output_ty, initial, transition) ->
      CAutomaton
        (state_ty, output_ty, shift amount cutoff initial, shift amount cutoff transition)
  | CAutomatonRun (count, automaton) ->
      CAutomatonRun (shift amount cutoff count, shift amount cutoff automaton)
  | CText e -> CText (shift amount cutoff e)
  | CImage (src, alt) -> CImage (shift amount cutoff src, shift amount cutoff alt)
  | CButton (label, msg) -> CButton (shift amount cutoff label, shift amount cutoff msg)
  | CInput (value, handler) -> CInput (shift amount cutoff value, shift amount cutoff handler)
  | CColumn children -> CColumn (shift amount cutoff children)
  | CRow children -> CRow (shift amount cutoff children)
  | CListView (items, render) -> CListView (shift amount cutoff items, shift amount cutoff render)
  | CWhenView (cond, view) -> CWhenView (shift amount cutoff cond, shift amount cutoff view)
  | CNode (tag, attrs, children) ->
      CNode (shift amount cutoff tag, shift amount cutoff attrs, shift amount cutoff children)
  | CAttr (name, value) -> CAttr (shift amount cutoff name, shift amount cutoff value)
  | COn (event, msg) -> COn (shift amount cutoff event, shift amount cutoff msg)
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
  | CStrict e -> CStrict (subst index replacement e)
  | CLet (e, body) -> CLet (subst index replacement e, subst (index + 1) (shift 1 0 replacement) body)
  | CRecord fields -> CRecord (List.map (fun (n, e) -> (n, subst index replacement e)) fields)
  | CField (e, field) -> CField (subst index replacement e, field)
  | CVariant (t, con, e) -> CVariant (t, con, subst index replacement e)
  | CInst (name, args) -> CInst (name, args)
  | CCase (e, branches) -> CCase (subst index replacement e, List.map (subst_branch index replacement) branches)
  | CFoldNat (n, zero, step) ->
      CFoldNat
        (subst index replacement n, subst index replacement zero, subst index replacement step)
  | CFoldVariant (target, result, scrut, branches) ->
      CFoldVariant
        ( target,
          result,
          subst index replacement scrut,
          List.map (subst_branch index replacement) branches )
  | CRecur e -> CRecur (subst index replacement e)
  | CNil t -> CNil t
  | CCons (t, head, tail) -> CCons (t, subst index replacement head, subst index replacement tail)
  | CFoldList (xs, zero, step) ->
      CFoldList
        (subst index replacement xs, subst index replacement zero, subst index replacement step)
  | CCaseList (xs, nil_body, cons_body) ->
      CCaseList
        ( subst index replacement xs,
          subst index replacement nil_body,
          subst (index + 2) (shift 2 0 replacement) cons_body )
  | CCoiter (state_ty, item_ty, seed, step) ->
      CCoiter (state_ty, item_ty, subst index replacement seed, subst index replacement step)
  | CStreamHead stream -> CStreamHead (subst index replacement stream)
  | CStreamTail stream -> CStreamTail (subst index replacement stream)
  | CStreamTake (count, stream) ->
      CStreamTake (subst index replacement count, subst index replacement stream)
  | CAutomaton (state_ty, output_ty, initial, transition) ->
      CAutomaton
        (state_ty, output_ty, subst index replacement initial, subst index replacement transition)
  | CAutomatonRun (count, automaton) ->
      CAutomatonRun (subst index replacement count, subst index replacement automaton)
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
  | CNode (tag, attrs, children) ->
      CNode
        ( subst index replacement tag,
          subst index replacement attrs,
          subst index replacement children )
  | CAttr (name, value) -> CAttr (subst index replacement name, subst index replacement value)
  | COn (event, msg) -> COn (subst index replacement event, subst index replacement msg)
  | CDone e -> CDone (subst index replacement e)
  | CRequest req -> CRequest req
  | CBind (p, t, body) -> CBind (subst index replacement p, t, subst (index + 1) (shift 1 0 replacement) body)

and subst_branch index replacement = function
  | CBBool (b, e) -> CBBool (b, subst index replacement e)
  | CBVariant (con, e) -> CBVariant (con, subst (index + 1) (shift 1 0 replacement) e)

let subst_top replacement body = shift (-1) 0 (subst 0 (shift 1 0 replacement) body)

let subst_top2 first second body =
  shift (-2) 0 (subst 0 (shift 2 0 first) (subst 1 (shift 2 0 second) body))

let rec subst_type_in_cterm args = function
  | CUnit -> CUnit
  | CBool b -> CBool b
  | CNat n -> CNat n
  | CString s -> CString s
  | CVar i -> CVar i
  | CGlobal n -> CGlobal n
  | CLambda (t, body) -> CLambda (subst_type args t, subst_type_in_cterm args body)
  | CApp (f, x) -> CApp (subst_type_in_cterm args f, subst_type_in_cterm args x)
  | CStrict e -> CStrict (subst_type_in_cterm args e)
  | CLet (e, body) -> CLet (subst_type_in_cterm args e, subst_type_in_cterm args body)
  | CRecord fields -> CRecord (List.map (fun (n, e) -> (n, subst_type_in_cterm args e)) fields)
  | CField (e, field) -> CField (subst_type_in_cterm args e, field)
  | CVariant (t, con, e) -> CVariant (subst_type args t, con, subst_type_in_cterm args e)
  | CInst (name, nested_args) -> CInst (name, List.map (subst_type args) nested_args)
  | CCase (e, branches) -> CCase (subst_type_in_cterm args e, List.map (subst_type_in_branch args) branches)
  | CFoldNat (n, zero, step) ->
      CFoldNat
        (subst_type_in_cterm args n, subst_type_in_cterm args zero, subst_type_in_cterm args step)
  | CFoldVariant (target, result, scrut, branches) ->
      CFoldVariant
        ( subst_type args target,
          subst_type args result,
          subst_type_in_cterm args scrut,
          List.map (subst_type_in_branch args) branches )
  | CRecur e -> CRecur (subst_type_in_cterm args e)
  | CNil t -> CNil (subst_type args t)
  | CCons (t, head, tail) ->
      CCons (subst_type args t, subst_type_in_cterm args head, subst_type_in_cterm args tail)
  | CFoldList (xs, zero, step) ->
      CFoldList
        (subst_type_in_cterm args xs, subst_type_in_cterm args zero, subst_type_in_cterm args step)
  | CCaseList (xs, nil_body, cons_body) ->
      CCaseList
        ( subst_type_in_cterm args xs,
          subst_type_in_cterm args nil_body,
          subst_type_in_cterm args cons_body )
  | CCoiter (state_ty, item_ty, seed, step) ->
      CCoiter
        ( subst_type args state_ty,
          subst_type args item_ty,
          subst_type_in_cterm args seed,
          subst_type_in_cterm args step )
  | CStreamHead stream -> CStreamHead (subst_type_in_cterm args stream)
  | CStreamTail stream -> CStreamTail (subst_type_in_cterm args stream)
  | CStreamTake (count, stream) ->
      CStreamTake (subst_type_in_cterm args count, subst_type_in_cterm args stream)
  | CAutomaton (state_ty, output_ty, initial, transition) ->
      CAutomaton
        ( subst_type args state_ty,
          subst_type args output_ty,
          subst_type_in_cterm args initial,
          subst_type_in_cterm args transition )
  | CAutomatonRun (count, automaton) ->
      CAutomatonRun (subst_type_in_cterm args count, subst_type_in_cterm args automaton)
  | CText e -> CText (subst_type_in_cterm args e)
  | CImage (src, alt) -> CImage (subst_type_in_cterm args src, subst_type_in_cterm args alt)
  | CButton (label, msg) -> CButton (subst_type_in_cterm args label, subst_type_in_cterm args msg)
  | CInput (value, handler) -> CInput (subst_type_in_cterm args value, subst_type_in_cterm args handler)
  | CColumn children -> CColumn (subst_type_in_cterm args children)
  | CRow children -> CRow (subst_type_in_cterm args children)
  | CListView (items, render) -> CListView (subst_type_in_cterm args items, subst_type_in_cterm args render)
  | CWhenView (cond, view) -> CWhenView (subst_type_in_cterm args cond, subst_type_in_cterm args view)
  | CNode (tag, attrs, children) ->
      CNode
        ( subst_type_in_cterm args tag,
          subst_type_in_cterm args attrs,
          subst_type_in_cterm args children )
  | CAttr (name, value) -> CAttr (subst_type_in_cterm args name, subst_type_in_cterm args value)
  | COn (event, msg) -> COn (subst_type_in_cterm args event, subst_type_in_cterm args msg)
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
      | CGlobal "prim.Nat.toString", CNat n -> CString (string_of_int n)
      | CApp (CGlobal "prim.Nat.eq", CNat a), CNat b -> CBool (a = b)
      | CApp (CGlobal "prim.String.concat", CString a), CString b -> CString (a ^ b)
      | CApp (CGlobal "prim.String.eq", CString a), CString b -> CBool (String.equal a b)
      | CGlobal "prim.String.length", CString s -> CNat (String_prim.length s)
      | CApp (CApp (CGlobal "prim.String.slice", CString s), CNat start), CNat count ->
          CString (String_prim.slice s start count)
      | _ -> CApp (nf, nx))
  | CStrict e -> normalize_cterm checked e
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
  | CFoldVariant (target, result, scrut, branches) ->
      let rec fold value =
        match normalize_cterm checked value with
        | CVariant (_, con, payload) -> (
            match
              List.find_map
                (function
                  | CBVariant (con', body) when String.equal con con' -> Some body
                  | _ -> None)
                branches
            with
            | Some body -> normalize_cterm checked (rewrite_recur (subst_top payload body))
            | None -> CFoldVariant (target, result, CVariant (target, con, payload), branches))
        | other -> CFoldVariant (target, result, other, branches)
      and rewrite_recur = function
        | CRecur e -> fold e
        | CUnit -> CUnit
        | CBool b -> CBool b
        | CNat n -> CNat n
        | CString s -> CString s
        | CVar i -> CVar i
        | CGlobal n -> CGlobal n
        | CLambda (t, body) -> CLambda (t, rewrite_recur body)
        | CApp (f, x) -> CApp (rewrite_recur f, rewrite_recur x)
        | CStrict e -> CStrict (rewrite_recur e)
        | CLet (e, body) -> CLet (rewrite_recur e, rewrite_recur body)
        | CRecord fields -> CRecord (List.map (fun (n, e) -> (n, rewrite_recur e)) fields)
        | CField (e, field) -> CField (rewrite_recur e, field)
        | CVariant (t, con, e) -> CVariant (t, con, rewrite_recur e)
        | CInst (name, args) -> CInst (name, args)
        | CCase (e, branches) ->
            CCase (rewrite_recur e, List.map rewrite_recur_branch branches)
        | CFoldNat (n, zero, step) ->
            CFoldNat (rewrite_recur n, rewrite_recur zero, rewrite_recur step)
        | CFoldVariant _ as nested -> normalize_cterm checked nested
        | CNil t -> CNil t
        | CCons (t, head, tail) -> CCons (t, rewrite_recur head, rewrite_recur tail)
        | CFoldList (xs, zero, step) ->
            CFoldList (rewrite_recur xs, rewrite_recur zero, rewrite_recur step)
        | CCaseList (xs, nil_body, cons_body) ->
            CCaseList (rewrite_recur xs, rewrite_recur nil_body, rewrite_recur cons_body)
        | CCoiter (state_ty, item_ty, seed, step) ->
            CCoiter (state_ty, item_ty, rewrite_recur seed, rewrite_recur step)
        | CStreamHead stream -> CStreamHead (rewrite_recur stream)
        | CStreamTail stream -> CStreamTail (rewrite_recur stream)
        | CStreamTake (count, stream) -> CStreamTake (rewrite_recur count, rewrite_recur stream)
        | CAutomaton (state_ty, output_ty, initial, transition) ->
            CAutomaton (state_ty, output_ty, rewrite_recur initial, rewrite_recur transition)
        | CAutomatonRun (count, automaton) ->
            CAutomatonRun (rewrite_recur count, rewrite_recur automaton)
        | CText e -> CText (rewrite_recur e)
        | CImage (src, alt) -> CImage (rewrite_recur src, rewrite_recur alt)
        | CButton (label, msg) -> CButton (rewrite_recur label, rewrite_recur msg)
        | CInput (value, handler) -> CInput (rewrite_recur value, rewrite_recur handler)
        | CColumn children -> CColumn (rewrite_recur children)
        | CRow children -> CRow (rewrite_recur children)
        | CListView (items, render) -> CListView (rewrite_recur items, rewrite_recur render)
        | CWhenView (cond, view) -> CWhenView (rewrite_recur cond, rewrite_recur view)
        | CNode (tag, attrs, children) ->
            CNode (rewrite_recur tag, rewrite_recur attrs, rewrite_recur children)
        | CAttr (name, value) -> CAttr (rewrite_recur name, rewrite_recur value)
        | COn (event, msg) -> COn (rewrite_recur event, rewrite_recur msg)
        | CDone e -> CDone (rewrite_recur e)
        | CRequest req -> CRequest req
        | CBind (p, t, body) -> CBind (rewrite_recur p, t, rewrite_recur body)
      and rewrite_recur_branch = function
        | CBBool (b, e) -> CBBool (b, rewrite_recur e)
        | CBVariant (con, e) -> CBVariant (con, rewrite_recur e)
      in
      fold scrut
  | CRecur _ -> fail "recur outside foldVariant"
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
  | CCaseList (xs, nil_body, cons_body) -> (
      match normalize_cterm checked xs with
      | CNil _ -> normalize_cterm checked nil_body
      | CCons (_, head, tail) ->
          normalize_cterm checked
            (subst_top2 (normalize_cterm checked head) (normalize_cterm checked tail) cons_body)
      | xs ->
          CCaseList
            ( xs,
              normalize_cterm checked nil_body,
              normalize_cterm checked cons_body ))
  | CCoiter (state_ty, item_ty, seed, step) ->
      CCoiter (state_ty, item_ty, normalize_cterm checked seed, normalize_cterm checked step)
  | CStreamHead stream -> (
      match normalize_cterm checked stream with
      | CCoiter (state_ty, item_ty, state, step) -> (
          match normalize_cterm checked (CApp (step, state)) with
          | CRecord fields -> (
              match assoc_opt "head" fields with
              | Some head -> normalize_cterm checked head
              | None -> CStreamHead (CCoiter (state_ty, item_ty, state, step)))
          | _ as stream -> CStreamHead stream)
      | stream -> CStreamHead stream)
  | CStreamTail stream -> (
      match normalize_cterm checked stream with
      | CCoiter (state_ty, item_ty, state, step) -> (
          match normalize_cterm checked (CApp (step, state)) with
          | CRecord fields -> (
              match assoc_opt "state" fields with
              | Some next_state -> CCoiter (state_ty, item_ty, normalize_cterm checked next_state, step)
              | None -> CStreamTail (CCoiter (state_ty, item_ty, state, step)))
          | _ as stream -> CStreamTail stream)
      | stream -> CStreamTail stream)
  | CStreamTake (count, stream) -> (
      match (normalize_cterm checked count, normalize_cterm checked stream) with
      | CNat n, CCoiter (state_ty, item_ty, state, step) ->
          let rec loop remaining state acc =
            if remaining <= 0 then
              List.fold_left (fun tail head -> CCons (item_ty, head, tail)) (CNil item_ty) acc
            else
              match normalize_cterm checked (CApp (step, state)) with
              | CRecord fields -> (
                  match (assoc_opt "head" fields, assoc_opt "state" fields) with
                  | Some head, Some next_state ->
                      loop (remaining - 1) (normalize_cterm checked next_state)
                        (normalize_cterm checked head :: acc)
                  | _ -> CStreamTake (CNat remaining, CCoiter (state_ty, item_ty, state, step)))
              | _ -> CStreamTake (CNat remaining, CCoiter (state_ty, item_ty, state, step))
          in
          loop n state []
      | count, stream -> CStreamTake (count, stream))
  | CAutomaton (state_ty, output_ty, initial, transition) ->
      CAutomaton
        (state_ty, output_ty, normalize_cterm checked initial, normalize_cterm checked transition)
  | CAutomatonRun (count, automaton) -> (
      match (normalize_cterm checked count, normalize_cterm checked automaton) with
      | CNat n, CAutomaton (state_ty, output_ty, state, transition) ->
          let rec loop remaining state acc =
            if remaining <= 0 then
              List.fold_left (fun tail output -> CCons (output_ty, output, tail)) (CNil output_ty) acc
            else
              match normalize_cterm checked (CApp (transition, state)) with
              | CRecord fields -> (
                  match (assoc_opt "output" fields, assoc_opt "state" fields) with
                  | Some output, Some next_state ->
                      loop (remaining - 1) (normalize_cterm checked next_state)
                        (normalize_cterm checked output :: acc)
                  | _ ->
                      CAutomatonRun
                        (CNat remaining, CAutomaton (state_ty, output_ty, state, transition)))
              | _ ->
                  CAutomatonRun
                    (CNat remaining, CAutomaton (state_ty, output_ty, state, transition))
          in
          loop n state []
      | count, automaton -> CAutomatonRun (count, automaton))
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
  | CNode (tag, attrs, children) ->
      CNode
        ( normalize_cterm checked tag,
          normalize_cterm checked attrs,
          normalize_cterm checked children )
  | CAttr (name, value) -> CAttr (normalize_cterm checked name, normalize_cterm checked value)
  | COn (event, msg) -> COn (normalize_cterm checked event, normalize_cterm checked msg)
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
