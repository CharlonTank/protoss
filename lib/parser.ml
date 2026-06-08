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
  | Sexp.List (Sexp.Atom n :: args) when n <> "" ->
      TNamed (n, List.map parse_type args)
  | Sexp.Atom s when s <> "" -> TNamed (s, [])
  | x -> fail ("invalid type: " ^ Sexp.to_string x)

let parse_binding = function
  | Sexp.List [ Sexp.Atom x; ty ] -> (x, parse_type ty)
  | x -> fail ("invalid binding: " ^ Sexp.to_string x)

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
  | TNamed (n, args) ->
      let args = List.map (qualify_type local_types params) args in
      if List.exists (String.equal n) params then TNamed (n, args)
      else (
        match assoc_opt n local_types with
        | Some q -> TNamed (q, args)
        | None -> TNamed (n, args))

let rec qualify_expr local_defs local_types bound = function
  | EUnit -> EUnit
  | EBool b -> EBool b
  | ENat n -> ENat n
  | EString s -> EString s
  | EName n ->
      if List.exists (String.equal n) bound then EName n
      else (
        match assoc_opt n local_defs with Some q -> EName q | None -> EName n)
  | ELambda (x, t, body) ->
      ELambda (x, qualify_type local_types [] t, qualify_expr local_defs local_types (x :: bound) body)
  | EApp (f, x) ->
      EApp (qualify_expr local_defs local_types bound f, qualify_expr local_defs local_types bound x)
  | ELet (x, e, body) ->
      ELet
        ( x,
          qualify_expr local_defs local_types bound e,
          qualify_expr local_defs local_types (x :: bound) body )
  | ERecord fields ->
      ERecord (sort_fields (List.map (fun (n, e) -> (n, qualify_expr local_defs local_types bound e)) fields))
  | EField (e, field) -> EField (qualify_expr local_defs local_types bound e, field)
  | EVariant (t, con, e) ->
      EVariant (qualify_type local_types [] t, con, qualify_expr local_defs local_types bound e)
  | ECase (e, branches) ->
      ECase
        ( qualify_expr local_defs local_types bound e,
          List.map (qualify_branch local_defs local_types bound) branches )
  | EFoldNat (n, z, step) ->
      EFoldNat
        ( qualify_expr local_defs local_types bound n,
          qualify_expr local_defs local_types bound z,
          qualify_expr local_defs local_types bound step )
  | ENil t -> ENil (qualify_type local_types [] t)
  | ECons (t, head, tail) ->
      ECons
        ( qualify_type local_types [] t,
          qualify_expr local_defs local_types bound head,
          qualify_expr local_defs local_types bound tail )
  | EFoldList (xs, z, step) ->
      EFoldList
        ( qualify_expr local_defs local_types bound xs,
          qualify_expr local_defs local_types bound z,
          qualify_expr local_defs local_types bound step )
  | EText e -> EText (qualify_expr local_defs local_types bound e)
  | EImage (src, alt) ->
      EImage
        (qualify_expr local_defs local_types bound src, qualify_expr local_defs local_types bound alt)
  | EButton (label, msg) ->
      EButton
        ( qualify_expr local_defs local_types bound label,
          qualify_expr local_defs local_types bound msg )
  | EInput (value, handler) ->
      EInput
        ( qualify_expr local_defs local_types bound value,
          qualify_expr local_defs local_types bound handler )
  | EColumn children -> EColumn (qualify_expr local_defs local_types bound children)
  | ERow children -> ERow (qualify_expr local_defs local_types bound children)
  | EListView (items, render) ->
      EListView
        ( qualify_expr local_defs local_types bound items,
          qualify_expr local_defs local_types bound render )
  | EWhenView (cond, view) ->
      EWhenView
        (qualify_expr local_defs local_types bound cond, qualify_expr local_defs local_types bound view)
  | EDone e -> EDone (qualify_expr local_defs local_types bound e)
  | ERequest req -> ERequest req
  | EBind (p, x, t, body) ->
      EBind
        ( qualify_expr local_defs local_types bound p,
          x,
          qualify_type local_types [] t,
          qualify_expr local_defs local_types (x :: bound) body )

and qualify_branch local_defs local_types bound = function
  | BBool (b, e) -> BBool (b, qualify_expr local_defs local_types bound e)
  | BVariant (con, x, e) -> BVariant (con, x, qualify_expr local_defs local_types (x :: bound) e)

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
          typ = qualify_type local_types [] d.typ;
          body = qualify_expr local_defs local_types [] d.body;
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
      `Def { name = n; typ = parse_type ty; body = parse_expr body }
  | Sexp.List (Sexp.Atom "defrec" :: _) ->
      fail "defrec is not supported: general recursion is rejected"
  | x -> fail ("invalid top-level form: " ^ Sexp.to_string x)

let parse_string input =
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

let parse_file path =
  let ic = open_in path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () ->
      let len = in_channel_length ic in
      let input = really_input_string ic len in
      parse_string input)
