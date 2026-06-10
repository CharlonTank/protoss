type typ =
  | TUnit
  | TBool
  | TNat
  | TString
  | TFun of typ * typ
  | TRecord of (string * typ) list
  | TVariant of (string * typ) list
  | TList of typ
  | TView of typ
  | TAttr of typ
  | TProcess of typ
  | TVar of int
  | TForall of int * typ
  | TNamed of string * typ list

type req =
  | AskHuman of string
  | HttpGet of string
  | ReadClock
  | SaveLocal of string * string
  | LoadLocal of string
  | ServerRequest of string * string

type expr =
  | EUnit
  | EBool of bool
  | ENat of int
  | EString of string
  | EName of string
  | ELambda of string * typ * expr
  | ELambdaInfer of string * expr
  | EApp of expr * expr
  | ELet of string * expr * expr
  | ELetAnnot of string * typ * expr * expr
  | ELetRecord of expr * (string * string) list * expr
  | ERecord of (string * expr) list
  | ERecordUpdate of expr * (string * expr) list
  | EField of expr * string
  | EVariant of typ * string * expr
  | EVariantInferred of string * expr
  | EInst of string * typ list
  | ECase of expr * branch list
  | EFoldNat of expr * expr * expr
  | EFoldVariant of typ * typ * expr * branch list
  | ERecur of expr
  | ENil of typ
  | ENilInfer
  | ECons of typ * expr * expr
  | EConsInfer of expr * expr
  | EFoldList of expr * expr * expr
  | ECaseList of expr * expr * string * string * expr
  | EText of expr
  | EImage of expr * expr
  | EButton of expr * expr
  | EInput of expr * expr
  | EColumn of expr
  | ERow of expr
  | EListView of expr * expr
  | EWhenView of expr * expr
  | ENode of expr * expr * expr
  | EAttr of expr * expr
  | EOn of expr * expr
  | EDone of expr
  | ERequest of req
  | EBind of expr * string * typ * expr
  | EBindInfer of expr * string * expr

and branch =
  | BBool of bool * expr
  | BVariant of string * string * expr
  | BVariantUnit of string * expr
  | BWildcard of expr

type def = {
  name : string;
  type_params : string list;
  declared_capabilities : string list option;
  typ : typ;
  body : expr;
}

type type_alias = {
  type_name : string;
  type_params : string list;
  type_body : typ;
}

type program = {
  imports : string list;
  capabilities : string list;
  module_name : string option;
  exports : string list option;
  type_aliases : type_alias list;
  defs : def list;
}

let compare_field (a, _) (b, _) = String.compare a b

(* Field lists are usually built from already-canonical (sorted) terms, so the
   common case returns the list unchanged without allocating. *)
let rec fields_sorted = function
  | [] | [ _ ] -> true
  | a :: (b :: _ as rest) -> compare_field a b <= 0 && fields_sorted rest

let sort_fields xs = if fields_sorted xs then xs else List.sort compare_field xs

let rec equal_typ a b =
  match (a, b) with
  | TUnit, TUnit | TBool, TBool | TNat, TNat | TString, TString -> true
  | TFun (a1, b1), TFun (a2, b2) -> equal_typ a1 a2 && equal_typ b1 b2
  | TList a, TList b -> equal_typ a b
  | TView TUnit, TView _ | TView _, TView TUnit -> true
  | TView a, TView b -> equal_typ a b
  | TAttr TUnit, TAttr _ | TAttr _, TAttr TUnit -> true
  | TAttr a, TAttr b -> equal_typ a b
  | TProcess a, TProcess b -> equal_typ a b
  | TVar a, TVar b -> a = b
  | TForall (arity_a, body_a), TForall (arity_b, body_b) ->
      arity_a = arity_b && equal_typ body_a body_b
  | TNamed (a, args_a), TNamed (b, args_b) ->
      String.equal a b
      && List.length args_a = List.length args_b
      && List.for_all2 equal_typ args_a args_b
  | TRecord fs1, TRecord fs2 | TVariant fs1, TVariant fs2 ->
      let fs1 = sort_fields fs1 and fs2 = sort_fields fs2 in
      List.length fs1 = List.length fs2
      && List.for_all2
           (fun (n1, t1) (n2, t2) -> String.equal n1 n2 && equal_typ t1 t2)
           fs1 fs2
  | _ -> false

let quote s =
  let b = Buffer.create (String.length s + 2) in
  Buffer.add_char b '"';
  String.iter
    (function
      | '"' -> Buffer.add_string b "\\\""
      | '\\' -> Buffer.add_string b "\\\\"
      | '\n' -> Buffer.add_string b "\\n"
      | '\r' -> Buffer.add_string b "\\r"
      | '\t' -> Buffer.add_string b "\\t"
      | c -> Buffer.add_char b c)
    s;
  Buffer.add_char b '"';
  Buffer.contents b

let rec string_of_typ_with_params params = function
  | TUnit -> "Unit"
  | TBool -> "Bool"
  | TNat -> "Nat"
  | TString -> "String"
  | TFun (a, b) ->
      "(-> " ^ string_of_typ_with_params params a ^ " " ^ string_of_typ_with_params params b ^ ")"
  | TRecord fields ->
      "(Record "
      ^ String.concat " "
          (List.map
             (fun (n, t) -> "(" ^ n ^ " " ^ string_of_typ_with_params params t ^ ")")
             (sort_fields fields))
      ^ ")"
  | TVariant cases ->
      "(Variant "
      ^ String.concat " "
          (List.map
             (fun (n, t) -> "(" ^ n ^ " " ^ string_of_typ_with_params params t ^ ")")
             (sort_fields cases))
      ^ ")"
  | TList t -> "(List " ^ string_of_typ_with_params params t ^ ")"
  | TView t -> "(View " ^ string_of_typ_with_params params t ^ ")"
  | TAttr t -> "(Attr " ^ string_of_typ_with_params params t ^ ")"
  | TProcess t -> "(Process " ^ string_of_typ_with_params params t ^ ")"
  | TVar i -> (
      match List.nth_opt params i with Some name -> name | None -> "(TVar " ^ string_of_int i ^ ")")
  | TForall (arity, body) ->
      "(Forall " ^ string_of_int arity ^ " " ^ string_of_typ_with_params params body ^ ")"
  | TNamed (n, []) -> n
  | TNamed (n, args) ->
      "(" ^ n ^ " " ^ String.concat " " (List.map (string_of_typ_with_params params) args) ^ ")"

let string_of_typ t = string_of_typ_with_params [] t

let string_of_req = function
  | AskHuman prompt -> "(Human.ask " ^ quote prompt ^ ")"
  | HttpGet url -> "(Http.get " ^ quote url ^ ")"
  | ReadClock -> "(Clock.read)"
  | SaveLocal (key, value) -> "(Local.save " ^ quote key ^ " " ^ quote value ^ ")"
  | LoadLocal key -> "(Local.load " ^ quote key ^ ")"
  | ServerRequest (route, payload) ->
      "(Server.request " ^ quote route ^ " " ^ quote payload ^ ")"

let rec string_of_expr_with_params params = function
  | EUnit -> "unit"
  | EBool true -> "true"
  | EBool false -> "false"
  | ENat n -> string_of_int n
  | EString s -> quote s
  | EName n -> n
  | ELambda (x, t, body) ->
      "(lambda (" ^ x ^ " " ^ string_of_typ_with_params params t ^ ") "
      ^ string_of_expr_with_params params body ^ ")"
  | ELambdaInfer (x, body) ->
      "(lambda " ^ x ^ " " ^ string_of_expr_with_params params body ^ ")"
  | EApp (f, x) ->
      "(" ^ string_of_expr_with_params params f ^ " " ^ string_of_expr_with_params params x ^ ")"
  | ELet (x, e, body) ->
      "(let (" ^ x ^ " " ^ string_of_expr_with_params params e ^ ") "
      ^ string_of_expr_with_params params body ^ ")"
  | ELetAnnot (x, t, e, body) ->
      "(let (" ^ x ^ " " ^ string_of_typ_with_params params t ^ " "
      ^ string_of_expr_with_params params e ^ ") "
      ^ string_of_expr_with_params params body ^ ")"
  | ELetRecord (record, fields, body) ->
      "(letRecord " ^ string_of_expr_with_params params record ^ " ("
      ^ String.concat " "
          (List.map
             (fun (field, binder) ->
               if String.equal field binder then field else "(" ^ field ^ " " ^ binder ^ ")")
             fields)
      ^ ") " ^ string_of_expr_with_params params body ^ ")"
  | ERecord fields ->
      "(record "
      ^ String.concat " "
          (List.map
             (fun (n, e) -> "(" ^ n ^ " " ^ string_of_expr_with_params params e ^ ")")
             (sort_fields fields))
      ^ ")"
  | ERecordUpdate (record, updates) ->
      "(recordUpdate " ^ string_of_expr_with_params params record ^ " "
      ^ String.concat " "
          (List.map
             (fun (n, e) -> "(" ^ n ^ " " ^ string_of_expr_with_params params e ^ ")")
             (sort_fields updates))
      ^ ")"
  | EField (e, field) -> "(get " ^ string_of_expr_with_params params e ^ " " ^ field ^ ")"
  | EVariant (t, con, e) ->
      "(variant " ^ string_of_typ_with_params params t ^ " " ^ con ^ " "
      ^ string_of_expr_with_params params e ^ ")"
  | EVariantInferred (con, e) ->
      "(variant " ^ con ^ " " ^ string_of_expr_with_params params e ^ ")"
  | EInst (name, args) ->
      "(inst " ^ name
      ^ (match args with
        | [] -> ""
        | _ -> " " ^ String.concat " " (List.map (string_of_typ_with_params params) args))
      ^ ")"
  | ECase (e, branches) ->
      "(case " ^ string_of_expr_with_params params e ^ " "
      ^ String.concat " " (List.map (string_of_branch_with_params params) branches)
      ^ ")"
  | EFoldNat (n, z, step) ->
      "(foldNat " ^ string_of_expr_with_params params n ^ " "
      ^ string_of_expr_with_params params z ^ " " ^ string_of_expr_with_params params step ^ ")"
  | EFoldVariant (target, result, scrut, branches) ->
      "(foldVariant " ^ string_of_typ_with_params params target ^ " "
      ^ string_of_typ_with_params params result ^ " "
      ^ string_of_expr_with_params params scrut ^ " "
      ^ String.concat " " (List.map (string_of_branch_with_params params) branches)
      ^ ")"
  | ERecur e -> "(recur " ^ string_of_expr_with_params params e ^ ")"
  | ENil t -> "(Nil " ^ string_of_typ_with_params params t ^ ")"
  | ENilInfer -> "Nil"
  | ECons (t, head, tail) ->
      "(Cons " ^ string_of_typ_with_params params t ^ " "
      ^ string_of_expr_with_params params head ^ " " ^ string_of_expr_with_params params tail ^ ")"
  | EConsInfer (head, tail) ->
      "(Cons " ^ string_of_expr_with_params params head ^ " "
      ^ string_of_expr_with_params params tail ^ ")"
  | EFoldList (xs, zero, step) ->
      "(foldList " ^ string_of_expr_with_params params xs ^ " "
      ^ string_of_expr_with_params params zero ^ " " ^ string_of_expr_with_params params step ^ ")"
  | ECaseList (xs, nil_body, head, tail, cons_body) ->
      "(caseList " ^ string_of_expr_with_params params xs ^ " (Nil "
      ^ string_of_expr_with_params params nil_body ^ ") (Cons " ^ head ^ " " ^ tail ^ " "
      ^ string_of_expr_with_params params cons_body ^ "))"
  | EText e -> "(text " ^ string_of_expr_with_params params e ^ ")"
  | EImage (src, alt) ->
      "(image " ^ string_of_expr_with_params params src ^ " "
      ^ string_of_expr_with_params params alt ^ ")"
  | EButton (label, msg) ->
      "(button " ^ string_of_expr_with_params params label ^ " "
      ^ string_of_expr_with_params params msg ^ ")"
  | EInput (value, handler) ->
      "(input " ^ string_of_expr_with_params params value ^ " "
      ^ string_of_expr_with_params params handler ^ ")"
  | EColumn children -> "(column " ^ string_of_expr_with_params params children ^ ")"
  | ERow children -> "(row " ^ string_of_expr_with_params params children ^ ")"
  | EListView (items, render) ->
      "(list " ^ string_of_expr_with_params params items ^ " "
      ^ string_of_expr_with_params params render ^ ")"
  | EWhenView (cond, view) ->
      "(when " ^ string_of_expr_with_params params cond ^ " "
      ^ string_of_expr_with_params params view ^ ")"
  | ENode (tag, attrs, children) ->
      "(node " ^ string_of_expr_with_params params tag ^ " "
      ^ string_of_expr_with_params params attrs ^ " "
      ^ string_of_expr_with_params params children ^ ")"
  | EAttr (name, value) ->
      "(attr " ^ string_of_expr_with_params params name ^ " "
      ^ string_of_expr_with_params params value ^ ")"
  | EOn (event, msg) ->
      "(on " ^ string_of_expr_with_params params event ^ " "
      ^ string_of_expr_with_params params msg ^ ")"
  | EDone e -> "(done " ^ string_of_expr_with_params params e ^ ")"
  | ERequest req -> string_of_req req
  | EBind (p, x, t, body) ->
      "(bind " ^ string_of_expr_with_params params p ^ " (lambda (" ^ x ^ " "
      ^ string_of_typ_with_params params t ^ ") " ^ string_of_expr_with_params params body ^ "))"
  | EBindInfer (p, x, body) ->
      "(bind " ^ string_of_expr_with_params params p ^ " (lambda " ^ x ^ " "
      ^ string_of_expr_with_params params body ^ "))"

and string_of_branch_with_params params = function
  | BBool (true, e) -> "(true " ^ string_of_expr_with_params params e ^ ")"
  | BBool (false, e) -> "(false " ^ string_of_expr_with_params params e ^ ")"
  | BVariant (con, x, e) ->
      "(" ^ con ^ " " ^ x ^ " " ^ string_of_expr_with_params params e ^ ")"
  | BVariantUnit (con, e) -> "(" ^ con ^ " " ^ string_of_expr_with_params params e ^ ")"
  | BWildcard e -> "(_ " ^ string_of_expr_with_params params e ^ ")"

let string_of_expr e = string_of_expr_with_params [] e

let string_of_branch branch = string_of_branch_with_params [] branch

let string_of_def (d : def) =
  let capability_clause =
    match d.declared_capabilities with
    | None -> None
    | Some caps -> Some ("(capabilities " ^ String.concat " " caps ^ ")")
  in
  match (d.type_params, capability_clause) with
  | [], None -> "(def " ^ d.name ^ " " ^ string_of_typ d.typ ^ " " ^ string_of_expr d.body ^ ")"
  | [], Some caps ->
      "(defcap " ^ d.name ^ " " ^ caps ^ " " ^ string_of_typ d.typ ^ " "
      ^ string_of_expr d.body ^ ")"
  | params, None ->
      let body_ty = match d.typ with TForall (_, body) -> body | t -> t in
      "(defpoly " ^ d.name ^ " (params " ^ String.concat " " params ^ ") "
      ^ string_of_typ_with_params params body_ty ^ " " ^ string_of_expr_with_params params d.body
      ^ ")"
  | params, Some caps ->
      let body_ty = match d.typ with TForall (_, body) -> body | t -> t in
      "(defpolycap " ^ d.name ^ " (params " ^ String.concat " " params ^ ") " ^ caps
      ^ " " ^ string_of_typ_with_params params body_ty ^ " "
      ^ string_of_expr_with_params params d.body ^ ")"

let string_of_type_alias a =
  match a.type_params with
  | [] -> "(type " ^ a.type_name ^ " " ^ string_of_typ a.type_body ^ ")"
  | params ->
      "(type " ^ a.type_name ^ " (" ^ String.concat " " params ^ ") "
      ^ string_of_typ a.type_body ^ ")"

let string_of_program p =
  let module_form =
    match p.module_name with None -> [] | Some name -> [ "(module " ^ name ^ ")" ]
  in
  let exports =
    match p.exports with
    | None -> []
    | Some names -> [ "(export " ^ String.concat " " names ^ ")" ]
  in
  let caps =
    match p.capabilities with
    | [] -> []
    | caps -> [ "(capabilities " ^ String.concat " " caps ^ ")" ]
  in
  let imports = List.map (fun path -> "(import " ^ quote path ^ ")") p.imports in
  String.concat "\n"
    (module_form @ imports @ exports @ caps @ List.map string_of_type_alias p.type_aliases
   @ List.map string_of_def p.defs)
  ^ "\n"
