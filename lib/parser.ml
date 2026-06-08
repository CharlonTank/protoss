open Ast

exception Error of string

let fail msg = raise (Error msg)

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

let rec parse_type = function
  | Sexp.Atom "Unit" -> TUnit
  | Sexp.Atom "Bool" -> TBool
  | Sexp.Atom "Nat" -> TNat
  | Sexp.Atom "String" -> TString
  | Sexp.List [ Sexp.Atom "List"; t ] -> TList (parse_type t)
  | Sexp.List [ Sexp.Atom "View"; t ] -> TView (parse_type t)
  | Sexp.List [ Sexp.Atom "Process"; t ] -> TProcess (parse_type t)
  | Sexp.List [ Sexp.Atom "->"; a; b ] -> TFun (parse_type a, parse_type b)
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
  | x -> fail ("invalid type: " ^ Sexp.to_string x)

let parse_binding = function
  | Sexp.List [ Sexp.Atom x; ty ] -> (x, parse_type ty)
  | x -> fail ("invalid binding: " ^ Sexp.to_string x)

let rec parse_expr = function
  | Sexp.Atom "unit" -> EUnit
  | Sexp.Atom "true" -> EBool true
  | Sexp.Atom "false" -> EBool false
  | Sexp.Atom s -> (
      match int_atom s with Some n when n >= 0 -> ENat n | _ -> EName s)
  | Sexp.Str s -> EString s
  | Sexp.List [ Sexp.Atom "lambda"; binding; body ] ->
      let x, ty = parse_binding binding in
      ELambda (x, ty, parse_expr body)
  | Sexp.List [ Sexp.Atom "let"; Sexp.List [ Sexp.Atom x; e ]; body ] ->
      ELet (x, parse_expr e, parse_expr body)
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
  | Sexp.List [ Sexp.Atom "get"; e; Sexp.Atom field ] -> EField (parse_expr e, field)
  | Sexp.List [ Sexp.Atom "variant"; ty; Sexp.Atom con; e ] ->
      EVariant (parse_type ty, con, parse_expr e)
  | Sexp.List (Sexp.Atom "case" :: scrut :: branches) ->
      ECase (parse_expr scrut, List.map parse_branch branches)
  | Sexp.List [ Sexp.Atom "foldNat"; n; z; step ] ->
      EFoldNat (parse_expr n, parse_expr z, parse_expr step)
  | Sexp.List [ Sexp.Atom "Nil"; ty ] -> ENil (parse_type ty)
  | Sexp.List [ Sexp.Atom "Cons"; ty; head; tail ] ->
      ECons (parse_type ty, parse_expr head, parse_expr tail)
  | Sexp.List [ Sexp.Atom "foldList"; xs; z; step ] ->
      EFoldList (parse_expr xs, parse_expr z, parse_expr step)
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
      let x, ty = parse_binding binding in
      EBind (parse_expr p, x, ty, parse_expr body)
  | Sexp.List [] -> fail "empty expression list"
  | Sexp.List (f :: args) ->
      List.fold_left (fun acc arg -> EApp (acc, parse_expr arg)) (parse_expr f) args

and parse_branch = function
  | Sexp.List [ Sexp.Atom "true"; e ] -> BBool (true, parse_expr e)
  | Sexp.List [ Sexp.Atom "false"; e ] -> BBool (false, parse_expr e)
  | Sexp.List [ Sexp.Atom con; Sexp.Atom x; e ] -> BVariant (con, x, parse_expr e)
  | x -> fail ("invalid case branch: " ^ Sexp.to_string x)

let parse_toplevel = function
  | Sexp.List [ Sexp.Atom "import"; Sexp.Str path ] -> `Import path
  | Sexp.List (Sexp.Atom "capabilities" :: caps) ->
      `Capabilities (List.map atom caps)
  | Sexp.List [ Sexp.Atom "def"; Sexp.Atom n; ty; body ] ->
      `Def { name = n; typ = parse_type ty; body = parse_expr body }
  | Sexp.List (Sexp.Atom "defrec" :: _) ->
      fail "defrec is not supported: general recursion is rejected"
  | x -> fail ("invalid top-level form: " ^ Sexp.to_string x)

let parse_string input =
  let forms =
    try Sexp.parse input with Sexp.Error msg -> fail msg
  in
  let imports = ref [] and caps = ref [] and defs = ref [] in
  List.iter
    (fun form ->
      match parse_toplevel form with
      | `Import path -> imports := path :: !imports
      | `Capabilities xs -> caps := xs @ !caps
      | `Def d -> defs := d :: !defs)
    forms;
  ensure_unique "definition" (List.map (fun d -> d.name) !defs);
  {
    imports = List.rev !imports;
    capabilities = List.sort_uniq String.compare !caps;
    defs = List.rev !defs;
  }

let parse_file path =
  let ic = open_in path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () ->
      let len = in_channel_length ic in
      let input = really_input_string ic len in
      parse_string input)
