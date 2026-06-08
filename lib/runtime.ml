open Ast

let fail = Kernel.fail

let runtime_version = "protoss-runtime-v2"

type value =
  | VUnit
  | VBool of bool
  | VNat of int
  | VString of string
  | VList of Ast.typ * value list
  | VRecord of (string * value) list
  | VVariant of typ * string * value
  | VView of view
  | VClosure of typ * Kernel.cterm * value list
  | VBuiltinSucc
  | VProcessDone of value
  | VProcessRequest of suspended

and suspended = {
  req : req;
  cont : continuation;
  cap_scope : string list;
}

and view =
  | VText of string
  | VImage of string * string
  | VButton of string * value
  | VInput of string * value
  | VColumn of view list
  | VRow of view list

and continuation =
  | KDone
  | KBind of continuation * Kernel.cterm * value list

type eval_state = {
  checked : Kernel.checked;
  def_cache : (string, value) Hashtbl.t;
  app_cache : (string, value) Hashtbl.t;
  cache_dir : string option;
  cache_scope : string;
  mutable trace : string list;
  trace_cache : bool;
}

let rec value_to_string = function
  | VUnit -> "unit"
  | VBool true -> "true"
  | VBool false -> "false"
  | VNat n -> string_of_int n
  | VString s -> Ast.quote s
  | VList (_, xs) -> "[" ^ String.concat ", " (List.map value_to_string xs) ^ "]"
  | VRecord fields ->
      "{"
      ^ String.concat ", "
          (List.map (fun (n, v) -> n ^ " = " ^ value_to_string v) (sort_fields fields))
      ^ "}"
  | VVariant (_, con, v) -> con ^ " " ^ value_to_string v
  | VView view -> view_to_string view
  | VClosure (t, body, _) ->
      "<lambda:" ^ Kernel.type_to_canonical t ^ ":" ^ Kernel.cterm_to_string body ^ ">"
  | VBuiltinSucc -> "<builtin:succ>"
  | VProcessDone v -> "Done " ^ value_to_string v
  | VProcessRequest s -> "Request " ^ Kernel.req_to_canonical s.req

and view_to_string = function
  | VText s -> "(text " ^ Ast.quote s ^ ")"
  | VImage (src, alt) -> "(image " ^ Ast.quote src ^ " " ^ Ast.quote alt ^ ")"
  | VButton (label, msg) -> "(button " ^ Ast.quote label ^ " " ^ value_to_string msg ^ ")"
  | VInput (value, _) -> "(input " ^ Ast.quote value ^ " <handler>)"
  | VColumn children ->
      "(column [" ^ String.concat ", " (List.map view_to_string children) ^ "])"
  | VRow children -> "(row [" ^ String.concat ", " (List.map view_to_string children) ^ "])"

let trace st line = if st.trace_cache then st.trace <- line :: st.trace

let has_prefix prefix s =
  let lp = String.length prefix in
  String.length s >= lp && String.sub s 0 lp = prefix

let strip_prefix prefix s =
  if has_prefix prefix s then
    Some (String.sub s (String.length prefix) (String.length s - String.length prefix))
  else None

let rec cache_value_to_canonical = function
  | VUnit -> Some "(Unit)"
  | VBool true -> Some "(Bool true)"
  | VBool false -> Some "(Bool false)"
  | VNat n -> Some ("(Nat " ^ string_of_int n ^ ")")
  | VString s -> Some ("(String " ^ Ast.quote s ^ ")")
  | VList (typ, xs) -> (
      match cache_values_to_canonical xs with
      | None -> None
      | Some xs ->
          Some
            ("(List " ^ Kernel.type_to_canonical typ ^ " " ^ String.concat " " xs ^ ")"))
  | VRecord fields -> (
      match cache_fields_to_canonical (sort_fields fields) with
      | None -> None
      | Some fields -> Some ("(Record " ^ String.concat " " fields ^ ")"))
  | VVariant (typ, con, v) -> (
      match cache_value_to_canonical v with
      | None -> None
      | Some v -> Some ("(Variant " ^ Kernel.type_to_canonical typ ^ " " ^ con ^ " " ^ v ^ ")"))
  | VView _ -> None
  | VClosure _ | VBuiltinSucc | VProcessDone _ | VProcessRequest _ -> None

and cache_values_to_canonical = function
  | [] -> Some []
  | v :: rest -> (
      match (cache_value_to_canonical v, cache_values_to_canonical rest) with
      | Some v, Some rest -> Some (v :: rest)
      | _ -> None)

and cache_fields_to_canonical = function
  | [] -> Some []
  | (name, value) :: rest -> (
      match (cache_value_to_canonical value, cache_fields_to_canonical rest) with
      | Some value, Some rest -> Some (("(" ^ name ^ " " ^ value ^ ")") :: rest)
      | _ -> None)

let rec cache_value_of_canonical_sexp = function
  | Sexp.List [ Sexp.Atom "Unit" ] -> VUnit
  | Sexp.List [ Sexp.Atom "Bool"; Sexp.Atom "true" ] -> VBool true
  | Sexp.List [ Sexp.Atom "Bool"; Sexp.Atom "false" ] -> VBool false
  | Sexp.List [ Sexp.Atom "Nat"; Sexp.Atom n ] -> VNat (int_of_string n)
  | Sexp.List [ Sexp.Atom "String"; Sexp.Str s ] -> VString s
  | Sexp.List (Sexp.Atom "List" :: typ :: xs) ->
      VList (Kernel.type_of_canonical_sexp typ, List.map cache_value_of_canonical_sexp xs)
  | Sexp.List (Sexp.Atom "Record" :: fields) ->
      VRecord
        (sort_fields
           (List.map
              (function
                | Sexp.List [ Sexp.Atom n; v ] -> (n, cache_value_of_canonical_sexp v)
                | x -> fail ("invalid cached record value: " ^ Sexp.to_string x))
              fields))
  | Sexp.List [ Sexp.Atom "Variant"; typ; Sexp.Atom con; v ] ->
      VVariant (Kernel.type_of_canonical_sexp typ, con, cache_value_of_canonical_sexp v)
  | x -> fail ("invalid cached value: " ^ Sexp.to_string x)

let cache_value_of_canonical input =
  match Kernel.single_sexp input with
  | sexp -> cache_value_of_canonical_sexp sexp

let cache_stats_path dir = Filename.concat dir "stats"

let read_cache_stats dir =
  let path = cache_stats_path dir in
  if not (Sys.file_exists path) then (0, 0)
  else
    Store.read_file path |> String.split_on_char '\n'
    |> List.fold_left
         (fun (hits, misses) line ->
           match String.split_on_char '=' line with
           | [ "hits"; n ] -> (int_of_string n, misses)
           | [ "misses"; n ] -> (hits, int_of_string n)
           | _ -> (hits, misses))
         (0, 0)

let write_cache_stats dir hits misses =
  Store.ensure_dir dir;
  Store.write_file_atomic (cache_stats_path dir)
    ("hits=" ^ string_of_int hits ^ "\nmisses=" ^ string_of_int misses ^ "\n")

let bump_cache_stats dir hit =
  let hits, misses = read_cache_stats dir in
  if hit then write_cache_stats dir (hits + 1) misses
  else write_cache_stats dir hits (misses + 1)

let cache_file dir key = Filename.concat dir (key ^ ".cache")

let persistent_cache_get st key =
  match st.cache_dir with
  | None -> None
  | Some dir ->
      let path = cache_file dir key in
      if Sys.file_exists path then (
        let value = cache_value_of_canonical (Store.read_file path) in
        bump_cache_stats dir true;
        Some value)
      else (
        bump_cache_stats dir false;
        None)

let persistent_cache_put st key value =
  match (st.cache_dir, cache_value_to_canonical value) with
  | Some dir, Some payload ->
      Store.ensure_dir dir;
      Store.write_file_atomic (cache_file dir key) (payload ^ "\n")
  | _ -> ()

let persistent_cache_stats dir =
  let hits, misses = read_cache_stats dir in
  let entries =
    if not (Sys.file_exists dir) then 0
    else
      Sys.readdir dir |> Array.to_list
      |> List.filter (fun f -> Filename.check_suffix f ".cache")
      |> List.length
  in
  (hits, misses, entries)

let def_by_ref checked n =
  checked.Kernel.defs
  |> List.find_opt (fun d -> String.equal d.Kernel.def.name n || String.equal d.Kernel.def_id n)

let rec nth_env env i =
  match (env, i) with
  | v :: _, 0 -> v
  | _ :: rest, n when n > 0 -> nth_env rest (n - 1)
  | _ -> fail ("unbound canonical variable #" ^ string_of_int i)

let rec eval_cterm st env = function
  | Kernel.CUnit -> VUnit
  | Kernel.CBool b -> VBool b
  | Kernel.CNat n -> VNat n
  | Kernel.CString s -> VString s
  | Kernel.CVar i -> nth_env env i
  | Kernel.CGlobal n ->
      if String.equal n "succ" then VBuiltinSucc
      else if List.exists (String.equal n) [ "prim.Nat.eq"; "prim.String.concat"; "prim.String.eq" ]
      then VClosure (TUnit, Kernel.CGlobal n, [])
      else eval_def st n
  | Kernel.CInst (n, args) -> (
      match def_by_ref st.checked n with
      | None -> fail ("unknown polymorphic definition at runtime: " ^ n)
      | Some d ->
          let canonical = Kernel.parse_serialized_def d.canonical in
          eval_cterm st [] (Kernel.subst_type_in_cterm args canonical.cbody))
  | Kernel.CLambda (t, body) -> VClosure (t, body, env)
  | Kernel.CApp (f, arg) ->
      let fv = eval_cterm st env f in
      let av = eval_cterm st env arg in
      eval_app st fv av
  | Kernel.CLet (e, body) ->
      let v = eval_cterm st env e in
      eval_cterm st (v :: env) body
  | Kernel.CRecord fields ->
      VRecord (sort_fields (List.map (fun (n, e) -> (n, eval_cterm st env e)) fields))
  | Kernel.CField (e, field) -> (
      match eval_cterm st env e with
      | VRecord fields -> (
          match Kernel.assoc_opt field fields with
          | Some v -> v
          | None -> fail ("unknown record field at runtime: " ^ field))
      | v -> fail ("field access on non-record at runtime: " ^ value_to_string v))
  | Kernel.CVariant (t, con, e) -> VVariant (t, con, eval_cterm st env e)
  | Kernel.CCase (e, branches) -> (
      match eval_cterm st env e with
      | VBool b ->
          let body =
            branches
            |> List.find_map (function
                 | Kernel.CBBool (b', e) when b = b' -> Some e
                 | _ -> None)
          in
          eval_cterm st env (Kernel.option_or_fail "missing Bool branch at runtime" body)
      | VVariant (_, con, payload) ->
          let body =
            branches
            |> List.find_map (function
                 | Kernel.CBVariant (con', e) when String.equal con con' -> Some e
                 | _ -> None)
          in
          eval_cterm st (payload :: env)
            (Kernel.option_or_fail ("missing Variant branch at runtime: " ^ con) body)
      | v -> fail ("case on invalid runtime value: " ^ value_to_string v))
  | Kernel.CFoldNat (n, zero, step) -> (
      match eval_cterm st env n with
      | VNat count ->
          let step_value = eval_cterm st env step in
          let rec loop i acc =
            if i <= 0 then acc else loop (i - 1) (eval_app st step_value acc)
          in
          loop count (eval_cterm st env zero)
      | v -> fail ("foldNat on non-Nat runtime value: " ^ value_to_string v))
  | Kernel.CNil t -> VList (t, [])
  | Kernel.CCons (t, head, tail) -> (
      match eval_cterm st env tail with
      | VList (_, xs) -> VList (t, eval_cterm st env head :: xs)
      | v -> fail ("Cons tail on non-List runtime value: " ^ value_to_string v))
  | Kernel.CFoldList (xs, zero, step) -> (
      match eval_cterm st env xs with
      | VList (_, items) ->
          let step_value = eval_cterm st env step in
          List.fold_right
            (fun item acc -> eval_app st (eval_app st step_value item) acc)
            items (eval_cterm st env zero)
      | v -> fail ("foldList on non-List runtime value: " ^ value_to_string v))
  | Kernel.CText e -> (
      match eval_cterm st env e with
      | VString s -> VView (VText s)
      | v -> fail ("text on non-String runtime value: " ^ value_to_string v))
  | Kernel.CImage (src, alt) -> (
      match (eval_cterm st env src, eval_cterm st env alt) with
      | VString src, VString alt -> VView (VImage (src, alt))
      | VString _, v -> fail ("image alt on non-String runtime value: " ^ value_to_string v)
      | v, _ -> fail ("image src on non-String runtime value: " ^ value_to_string v))
  | Kernel.CButton (label, msg) -> (
      match eval_cterm st env label with
      | VString s -> VView (VButton (s, eval_cterm st env msg))
      | v -> fail ("button label on non-String runtime value: " ^ value_to_string v))
  | Kernel.CInput (value, handler) -> (
      match eval_cterm st env value with
      | VString s -> VView (VInput (s, eval_cterm st env handler))
      | v -> fail ("input value on non-String runtime value: " ^ value_to_string v))
  | Kernel.CColumn children -> (
      match eval_cterm st env children with
      | VList (_, items) -> VView (VColumn (List.map expect_view items))
      | v -> fail ("column on non-List runtime value: " ^ value_to_string v))
  | Kernel.CRow children -> (
      match eval_cterm st env children with
      | VList (_, items) -> VView (VRow (List.map expect_view items))
      | v -> fail ("row on non-List runtime value: " ^ value_to_string v))
  | Kernel.CListView (items, render) -> (
      match eval_cterm st env items with
      | VList (_, items) ->
          let render = eval_cterm st env render in
          VView (VColumn (List.map (fun item -> expect_view (eval_app st render item)) items))
      | v -> fail ("list view on non-List runtime value: " ^ value_to_string v))
  | Kernel.CWhenView (cond, view) -> (
      match eval_cterm st env cond with
      | VBool true -> eval_cterm st env view
      | VBool false -> VView (VColumn [])
      | v -> fail ("when condition on non-Bool runtime value: " ^ value_to_string v))
  | Kernel.CDone e -> VProcessDone (eval_cterm st env e)
  | Kernel.CRequest req -> VProcessRequest { req; cont = KDone; cap_scope = [] }
  | Kernel.CBind (p, _, body) -> (
      match eval_cterm st env p with
      | VProcessDone v -> eval_cterm st (v :: env) body
      | VProcessRequest s -> VProcessRequest { s with cont = KBind (s.cont, body, env) }
      | other -> fail ("bind on non-process runtime value: " ^ value_to_string other))

and expect_view = function
  | VView view -> view
  | v -> fail ("expected View runtime value, got " ^ value_to_string v)

and eval_app st fv av =
  let key =
    Kernel.hash_string
      ("app:" ^ st.cache_scope ^ ":" ^ value_to_string fv ^ ":" ^ value_to_string av)
  in
  match Hashtbl.find_opt st.app_cache key with
  | Some v ->
      trace st ("cache hit " ^ key);
      v
  | None -> (
      match persistent_cache_get st key with
      | Some v ->
          trace st ("cache hit persistent " ^ key);
          Hashtbl.add st.app_cache key v;
          v
      | None ->
          trace st ("cache miss " ^ key);
          let result =
            match fv with
            | VBuiltinSucc -> (
                match av with
                | VNat n -> VNat (n + 1)
                | v -> fail ("builtin on " ^ value_to_string v))
            | VClosure (_, Kernel.CGlobal "prim.Nat.eq", closure_env) -> (
                match (closure_env, av) with
                | [], VNat _ -> VClosure (TNat, Kernel.CGlobal "prim.Nat.eq", [ av ])
                | [ VNat a ], VNat b -> VBool (a = b)
                | _ -> fail "prim.Nat.eq expects Nat Nat")
            | VClosure (_, Kernel.CGlobal "prim.String.concat", closure_env) -> (
                match (closure_env, av) with
                | [], VString _ -> VClosure (TString, Kernel.CGlobal "prim.String.concat", [ av ])
                | [ VString a ], VString b -> VString (a ^ b)
                | _ -> fail "prim.String.concat expects String String")
            | VClosure (_, Kernel.CGlobal "prim.String.eq", closure_env) -> (
                match (closure_env, av) with
                | [], VString _ -> VClosure (TString, Kernel.CGlobal "prim.String.eq", [ av ])
                | [ VString a ], VString b -> VBool (String.equal a b)
                | _ -> fail "prim.String.eq expects String String")
            | VClosure (_, body, closure_env) -> eval_cterm st (av :: closure_env) body
            | v -> fail ("application of non-function runtime value: " ^ value_to_string v)
          in
          Hashtbl.add st.app_cache key result;
          persistent_cache_put st key result;
          result)

and eval_def st n =
  match Hashtbl.find_opt st.def_cache n with
  | Some v -> v
  | None -> (
      match def_by_ref st.checked n with
      | None -> fail ("unknown definition at runtime: " ^ n)
      | Some d ->
          let canonical = Kernel.parse_serialized_def d.canonical in
          let v = eval_cterm st [] canonical.cbody in
          Hashtbl.add st.def_cache n v;
          v)

let state ?(trace_cache = false) ?cache_dir ?cache_scope checked =
  let cache_scope =
    match cache_scope with
    | Some scope -> scope
    | None -> Kernel.hash_program checked
  in
  {
    checked;
    def_cache = Hashtbl.create 32;
    app_cache = Hashtbl.create 64;
    cache_dir;
    cache_scope;
    trace = [];
    trace_cache;
  }

let eval_entry ?(trace_cache = false) ?cache_dir ?cache_scope checked entry =
  let st = state ~trace_cache ?cache_dir ?cache_scope checked in
  let value = eval_def st entry in
  (value, List.rev st.trace)

let apply checked f arg =
  let st = state checked in
  eval_app st f arg

let normalize_def ?(trace_cache = false) ?cache_dir ?cache_scope checked name =
  eval_entry ~trace_cache ?cache_dir ?cache_scope checked name

let normalize_all checked =
  List.map
    (fun d -> (d.Kernel.def.Ast.name, fst (normalize_def checked d.Kernel.def.name)))
    checked.Kernel.defs

let rec value_to_canonical = function
  | VUnit -> "(Unit)"
  | VBool true -> "(Bool true)"
  | VBool false -> "(Bool false)"
  | VNat n -> "(Nat " ^ string_of_int n ^ ")"
  | VString s -> "(String " ^ Ast.quote s ^ ")"
  | VList (typ, xs) ->
      "(List " ^ Kernel.type_to_canonical typ ^ " "
      ^ String.concat " " (List.map value_to_canonical xs) ^ ")"
  | VRecord fields ->
      "(Record "
      ^ String.concat " "
          (List.map (fun (n, v) -> "(" ^ n ^ " " ^ value_to_canonical v ^ ")") (sort_fields fields))
      ^ ")"
  | VVariant (typ, con, v) ->
      "(Variant " ^ Kernel.type_to_canonical typ ^ " " ^ con ^ " " ^ value_to_canonical v ^ ")"
  | VView view -> "(View " ^ view_to_canonical view ^ ")"
  | VClosure (typ, body, env) ->
      "(Closure " ^ Kernel.type_to_canonical typ ^ " " ^ Kernel.cterm_to_string body ^ " (env "
      ^ String.concat " " (List.map value_to_canonical env) ^ "))"
  | VBuiltinSucc -> "BuiltinSucc"
  | VProcessDone v -> "(Done " ^ value_to_canonical v ^ ")"
  | VProcessRequest s -> "(Suspended " ^ suspended_payload_to_canonical s ^ ")"

and view_to_canonical = function
  | VText s -> "(Text " ^ Ast.quote s ^ ")"
  | VImage (src, alt) -> "(Image " ^ Ast.quote src ^ " " ^ Ast.quote alt ^ ")"
  | VButton (label, msg) -> "(Button " ^ Ast.quote label ^ " " ^ value_to_canonical msg ^ ")"
  | VInput (value, _) -> "(Input " ^ Ast.quote value ^ " <handler>)"
  | VColumn children ->
      "(Column " ^ String.concat " " (List.map view_to_canonical children) ^ ")"
  | VRow children -> "(Row " ^ String.concat " " (List.map view_to_canonical children) ^ ")"

and continuation_to_canonical = function
  | KDone -> "KDone"
  | KBind (inner, body, env) ->
      "(KBind " ^ continuation_to_canonical inner ^ " " ^ Kernel.cterm_to_string body ^ " (env "
      ^ String.concat " " (List.map value_to_canonical env) ^ "))"

and suspended_payload_to_canonical s =
  let request = Kernel.req_to_canonical s.req in
  let cont = continuation_to_canonical s.cont in
  let cap_scope = String.concat " " (List.sort_uniq String.compare s.cap_scope) in
  "(suspended (request-id " ^ Kernel.hash_string ("request:" ^ request) ^ ") (request " ^ request
  ^ ") (continuation-id " ^ Kernel.hash_string ("cont:" ^ cont) ^ ") (cont " ^ cont
  ^ ") (cap-scope (" ^ cap_scope ^ ")))"

let serialize_suspended s = "(" ^ runtime_version ^ " " ^ suspended_payload_to_canonical s ^ ")"

let with_cap_scope cap_scope = function
  | VProcessRequest s -> VProcessRequest { s with cap_scope = List.sort_uniq String.compare cap_scope }
  | v -> v

let request_id s = Kernel.hash_string ("request:" ^ Kernel.req_to_canonical s.req)

let continuation_id s = Kernel.hash_string ("cont:" ^ continuation_to_canonical s.cont)

let rec value_of_canonical_sexp = function
  | Sexp.List [ Sexp.Atom "Unit" ] -> VUnit
  | Sexp.List [ Sexp.Atom "Bool"; Sexp.Atom "true" ] -> VBool true
  | Sexp.List [ Sexp.Atom "Bool"; Sexp.Atom "false" ] -> VBool false
  | Sexp.List [ Sexp.Atom "Nat"; Sexp.Atom n ] -> VNat (int_of_string n)
  | Sexp.List [ Sexp.Atom "String"; Sexp.Str s ] -> VString s
  | Sexp.List (Sexp.Atom "List" :: typ :: xs) ->
      VList (Kernel.type_of_canonical_sexp typ, List.map value_of_canonical_sexp xs)
  | Sexp.List (Sexp.Atom "Record" :: fields) ->
      VRecord
        (sort_fields
           (List.map
              (function
                | Sexp.List [ Sexp.Atom n; v ] -> (n, value_of_canonical_sexp v)
                | x -> fail ("invalid runtime record value: " ^ Sexp.to_string x))
              fields))
  | Sexp.List [ Sexp.Atom "Variant"; typ; Sexp.Atom con; v ] ->
      VVariant (Kernel.type_of_canonical_sexp typ, con, value_of_canonical_sexp v)
  | Sexp.List [ Sexp.Atom "View"; view ] -> VView (view_of_canonical_sexp view)
  | Sexp.List [ Sexp.Atom "Closure"; typ; body; Sexp.List (Sexp.Atom "env" :: env) ] ->
      VClosure
        ( Kernel.type_of_canonical_sexp typ,
          Kernel.cterm_of_canonical_sexp body,
          List.map value_of_canonical_sexp env )
  | Sexp.Atom "BuiltinSucc" -> VBuiltinSucc
  | Sexp.List [ Sexp.Atom "Done"; v ] -> VProcessDone (value_of_canonical_sexp v)
  | x -> fail ("invalid runtime value: " ^ Sexp.to_string x)

and view_of_canonical_sexp = function
  | Sexp.List [ Sexp.Atom "Text"; Sexp.Str s ] -> VText s
  | Sexp.List [ Sexp.Atom "Image"; Sexp.Str src; Sexp.Str alt ] -> VImage (src, alt)
  | Sexp.List [ Sexp.Atom "Button"; Sexp.Str label; msg ] ->
      VButton (label, value_of_canonical_sexp msg)
  | Sexp.List (Sexp.Atom "Column" :: children) ->
      VColumn (List.map view_of_canonical_sexp children)
  | Sexp.List (Sexp.Atom "Row" :: children) -> VRow (List.map view_of_canonical_sexp children)
  | x -> fail ("invalid runtime view: " ^ Sexp.to_string x)

let rec continuation_of_canonical_sexp = function
  | Sexp.Atom "KDone" -> KDone
  | Sexp.List [ Sexp.Atom "KBind"; inner; body; Sexp.List (Sexp.Atom "env" :: env) ] ->
      KBind
        ( continuation_of_canonical_sexp inner,
          Kernel.cterm_of_canonical_sexp body,
          List.map value_of_canonical_sexp env )
  | x -> fail ("invalid runtime continuation: " ^ Sexp.to_string x)

let suspended_of_payload = function
  | Sexp.List items -> (
      match items with
      | Sexp.Atom "suspended" :: fields ->
          let field name =
            List.find_map
              (function Sexp.List [ Sexp.Atom n; v ] when String.equal n name -> Some v | _ -> None)
              fields
          in
          let req =
            match field "request" with
            | Some req -> Kernel.req_of_canonical_sexp req
            | None -> fail "runtime suspension missing request"
          in
          let cont =
            match field "cont" with
            | Some cont -> continuation_of_canonical_sexp cont
            | None -> fail "runtime suspension missing continuation"
          in
          let cap_scope =
            match field "cap-scope" with
            | Some (Sexp.List caps) ->
                List.map
                  (function
                    | Sexp.Atom s -> s
                    | x -> fail ("invalid cap-scope atom: " ^ Sexp.to_string x))
                  caps
            | Some (Sexp.Atom cap) -> [ cap ]
            | Some _ -> fail "invalid cap-scope"
            | None -> []
          in
          let require_id field_name expected =
            match field field_name with
            | None -> ()
            | Some (Sexp.Atom actual) when String.equal actual expected -> ()
            | Some (Sexp.Atom actual) ->
                fail
                  (field_name ^ " mismatch: expected " ^ expected ^ ", got " ^ actual)
            | Some x -> fail ("invalid " ^ field_name ^ ": " ^ Sexp.to_string x)
          in
          require_id "request-id" (Kernel.hash_string ("request:" ^ Kernel.req_to_canonical req));
          require_id "continuation-id" (Kernel.hash_string ("cont:" ^ continuation_to_canonical cont));
          { req; cont; cap_scope = List.sort_uniq String.compare cap_scope }
      | _ -> fail ("invalid runtime suspension: " ^ Sexp.to_string (Sexp.List items)))
  | x -> fail ("invalid runtime suspension: " ^ Sexp.to_string x)

let parse_suspended input =
  match Kernel.single_sexp input with
  | Sexp.List [ Sexp.Atom version; payload ] when String.equal version runtime_version ->
      suspended_of_payload payload
  | Sexp.List [ Sexp.Atom "protoss-runtime-v1"; payload ] ->
      let s = suspended_of_payload payload in
      { s with cap_scope = [] }
  | x -> fail ("invalid runtime suspended serialization: " ^ Sexp.to_string x)

let response_value req response =
  let protocol_mismatch expected =
    fail ("protocol response type mismatch: expected " ^ expected ^ ", got " ^ response)
  in
  let parse_tag tag =
    strip_prefix (tag ^ ":") response
  in
  let has_known_tag =
    List.exists
      (fun tag -> match parse_tag tag with Some _ -> true | None -> false)
      [ "String"; "Nat"; "Bool"; "Unit" ]
  in
  match Kernel.req_result_type req with
  | TString -> (
      match parse_tag "String" with
      | Some s -> VString s
      | None ->
          if has_known_tag then protocol_mismatch "String";
          VString response)
  | TUnit when String.equal response "unit" || String.equal response "Unit:" -> VUnit
  | TUnit ->
      if has_known_tag then protocol_mismatch "Unit";
      fail ("response is not Unit: " ^ response)
  | TBool when String.equal response "true" || String.equal response "Bool:true" -> VBool true
  | TBool when String.equal response "false" || String.equal response "Bool:false" -> VBool false
  | TBool ->
      if has_known_tag then protocol_mismatch "Bool";
      fail ("response is not Bool: " ^ response)
  | TNat -> (
      let raw =
        match parse_tag "Nat" with
        | Some n -> n
        | None ->
            if has_known_tag then protocol_mismatch "Nat";
            response
      in
      try VNat (int_of_string raw) with Failure _ -> fail ("response is not a Nat: " ^ response))
  | typ -> fail ("CLI resume cannot parse response type " ^ Ast.string_of_typ typ)

let rec apply_cont st cont response =
  match cont with
  | KDone -> VProcessDone response
  | KBind (inner, body, env) -> (
      match apply_cont st inner response with
      | VProcessDone v -> eval_cterm st (v :: env) body
      | VProcessRequest s -> VProcessRequest { s with cont = KBind (s.cont, body, env) }
      | other -> fail ("invalid resumed process value: " ^ value_to_string other))

let resume checked suspended response =
  let st = state checked in
  apply_cont st suspended.cont response
