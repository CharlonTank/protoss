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
  | TProcess of typ
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
  | EApp of expr * expr
  | ELet of string * expr * expr
  | ERecord of (string * expr) list
  | EField of expr * string
  | EVariant of typ * string * expr
  | EVariantInferred of string * expr
  | ECase of expr * branch list
  | EFoldNat of expr * expr * expr
  | ENil of typ
  | ECons of typ * expr * expr
  | EFoldList of expr * expr * expr
  | EText of expr
  | EImage of expr * expr
  | EButton of expr * expr
  | EInput of expr * expr
  | EColumn of expr
  | ERow of expr
  | EListView of expr * expr
  | EWhenView of expr * expr
  | EDone of expr
  | ERequest of req
  | EBind of expr * string * typ * expr

and branch =
  | BBool of bool * expr
  | BVariant of string * string * expr

type def = {
  name : string;
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

let sort_fields xs = List.sort compare_field xs

let rec equal_typ a b =
  match (a, b) with
  | TUnit, TUnit | TBool, TBool | TNat, TNat | TString, TString -> true
  | TFun (a1, b1), TFun (a2, b2) -> equal_typ a1 a2 && equal_typ b1 b2
  | TList a, TList b -> equal_typ a b
  | TView TUnit, TView _ | TView _, TView TUnit -> true
  | TView a, TView b -> equal_typ a b
  | TProcess a, TProcess b -> equal_typ a b
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

let rec string_of_typ = function
  | TUnit -> "Unit"
  | TBool -> "Bool"
  | TNat -> "Nat"
  | TString -> "String"
  | TFun (a, b) -> "(-> " ^ string_of_typ a ^ " " ^ string_of_typ b ^ ")"
  | TRecord fields ->
      "(Record "
      ^ String.concat " "
          (List.map
             (fun (n, t) -> "(" ^ n ^ " " ^ string_of_typ t ^ ")")
             (sort_fields fields))
      ^ ")"
  | TVariant cases ->
      "(Variant "
      ^ String.concat " "
          (List.map
             (fun (n, t) -> "(" ^ n ^ " " ^ string_of_typ t ^ ")")
             (sort_fields cases))
      ^ ")"
  | TList t -> "(List " ^ string_of_typ t ^ ")"
  | TView t -> "(View " ^ string_of_typ t ^ ")"
  | TProcess t -> "(Process " ^ string_of_typ t ^ ")"
  | TNamed (n, []) -> n
  | TNamed (n, args) ->
      "(" ^ n ^ " " ^ String.concat " " (List.map string_of_typ args) ^ ")"

let string_of_req = function
  | AskHuman prompt -> "(Human.ask " ^ quote prompt ^ ")"
  | HttpGet url -> "(Http.get " ^ quote url ^ ")"
  | ReadClock -> "(Clock.read)"
  | SaveLocal (key, value) -> "(Local.save " ^ quote key ^ " " ^ quote value ^ ")"
  | LoadLocal key -> "(Local.load " ^ quote key ^ ")"
  | ServerRequest (route, payload) ->
      "(Server.request " ^ quote route ^ " " ^ quote payload ^ ")"

let rec string_of_expr = function
  | EUnit -> "unit"
  | EBool true -> "true"
  | EBool false -> "false"
  | ENat n -> string_of_int n
  | EString s -> quote s
  | EName n -> n
  | ELambda (x, t, body) ->
      "(lambda (" ^ x ^ " " ^ string_of_typ t ^ ") " ^ string_of_expr body ^ ")"
  | EApp (f, x) -> "(" ^ string_of_expr f ^ " " ^ string_of_expr x ^ ")"
  | ELet (x, e, body) ->
      "(let (" ^ x ^ " " ^ string_of_expr e ^ ") " ^ string_of_expr body ^ ")"
  | ERecord fields ->
      "(record "
      ^ String.concat " "
          (List.map
             (fun (n, e) -> "(" ^ n ^ " " ^ string_of_expr e ^ ")")
             (sort_fields fields))
      ^ ")"
  | EField (e, field) -> "(get " ^ string_of_expr e ^ " " ^ field ^ ")"
  | EVariant (t, con, e) ->
      "(variant " ^ string_of_typ t ^ " " ^ con ^ " " ^ string_of_expr e ^ ")"
  | EVariantInferred (con, e) -> "(variant " ^ con ^ " " ^ string_of_expr e ^ ")"
  | ECase (e, branches) ->
      "(case " ^ string_of_expr e ^ " "
      ^ String.concat " " (List.map string_of_branch branches)
      ^ ")"
  | EFoldNat (n, z, step) ->
      "(foldNat " ^ string_of_expr n ^ " " ^ string_of_expr z ^ " "
      ^ string_of_expr step ^ ")"
  | ENil t -> "(Nil " ^ string_of_typ t ^ ")"
  | ECons (t, head, tail) ->
      "(Cons " ^ string_of_typ t ^ " " ^ string_of_expr head ^ " " ^ string_of_expr tail ^ ")"
  | EFoldList (xs, zero, step) ->
      "(foldList " ^ string_of_expr xs ^ " " ^ string_of_expr zero ^ " "
      ^ string_of_expr step ^ ")"
  | EText e -> "(text " ^ string_of_expr e ^ ")"
  | EImage (src, alt) ->
      "(image " ^ string_of_expr src ^ " " ^ string_of_expr alt ^ ")"
  | EButton (label, msg) ->
      "(button " ^ string_of_expr label ^ " " ^ string_of_expr msg ^ ")"
  | EInput (value, handler) ->
      "(input " ^ string_of_expr value ^ " " ^ string_of_expr handler ^ ")"
  | EColumn children -> "(column " ^ string_of_expr children ^ ")"
  | ERow children -> "(row " ^ string_of_expr children ^ ")"
  | EListView (items, render) ->
      "(list " ^ string_of_expr items ^ " " ^ string_of_expr render ^ ")"
  | EWhenView (cond, view) ->
      "(when " ^ string_of_expr cond ^ " " ^ string_of_expr view ^ ")"
  | EDone e -> "(done " ^ string_of_expr e ^ ")"
  | ERequest req -> string_of_req req
  | EBind (p, x, t, body) ->
      "(bind " ^ string_of_expr p ^ " (lambda (" ^ x ^ " " ^ string_of_typ t
      ^ ") " ^ string_of_expr body ^ "))"

and string_of_branch = function
  | BBool (true, e) -> "(true " ^ string_of_expr e ^ ")"
  | BBool (false, e) -> "(false " ^ string_of_expr e ^ ")"
  | BVariant (con, x, e) -> "(" ^ con ^ " " ^ x ^ " " ^ string_of_expr e ^ ")"

let string_of_def d =
  "(def " ^ d.name ^ " " ^ string_of_typ d.typ ^ " " ^ string_of_expr d.body ^ ")"

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
