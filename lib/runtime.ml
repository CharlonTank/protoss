open Ast

let fail = Kernel.fail

let runtime_version = "protoss-runtime-v2"

let eval_cache_version = "protoss.eval.v1"

let process_eval_cache_version = "protoss.process.eval.v1"

let no_args_hash = Kernel.hash_string "protoss.eval.args.v1\n()"

let capability_scope_text caps =
  String.concat " " (List.sort_uniq String.compare caps)

let eval_key ~def_id ~args_hash ~runtime_policy =
  Kernel.hash_string
    (eval_cache_version ^ "\ndef-id=" ^ def_id ^ "\nargs-hash=" ^ args_hash
   ^ "\nruntime-policy=" ^ runtime_policy)

let process_eval_key ~def_id ~world_ref ~cap_scope ~runtime_policy =
  Kernel.hash_string
    (process_eval_cache_version ^ "\ndef-id=" ^ def_id ^ "\nworld-ref=" ^ world_ref
   ^ "\ncap-scope=" ^ capability_scope_text cap_scope ^ "\nruntime-policy=" ^ runtime_policy)

type value =
  | VUnit
  | VBool of bool
  | VNat of int
  | VString of string
  | VList of Ast.typ * value list
  | VRecord of (string * value) list
  | VVariant of typ * string * value
  | VView of view
  | VAttribute of vattr
  | VClosure of typ * Kernel.cterm * value list * string list
  | VBuiltinSucc
  | VProcessDone of value
  | VProcessRequest of suspended
  | VThunk of thunk

and thunk = {
  mutable thunk_value : value option;
  thunk_eval : unit -> value;
}

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
  | VNode of string * vattr list * view list

and vattr =
  | VAttr of string * string
  | VOn of string * value

and continuation =
  | KDone
  | KBind of continuation * Kernel.cterm * value list * string list

let rec force_value = function
  | VThunk thunk -> (
      match thunk.thunk_value with
      | Some value -> force_value value
      | None ->
          let value = thunk.thunk_eval () in
          thunk.thunk_value <- Some value;
          force_value value)
  | value -> value

type recur_frame = {
  (* Serializing the fold node and its full environment is expensive and only
     observable through cache keys (persistent cache / tracing), so it is
     deferred until a cache key is actually built. *)
  recur_key : string Lazy.t;
  recur_apply : value -> value;
}

type eval_state = {
  checked : Kernel.checked;
  def_cache : (string, value) Hashtbl.t;
  app_cache : (string, value) Hashtbl.t;
  cache_dir : string option;
  cache_scope : string;
  mutable trace : string list;
  trace_cache : bool;
  stdlib_fast_paths : bool;
  mutable recur_stack : recur_frame list;
  mutable cap_scope : string list;
}

let runtime_policy_text ~cap_scope ~cache_scope ~stdlib_fast_paths =
  "runtime=" ^ runtime_version ^ "\ncache-scope=" ^ cache_scope
  ^ "\nstdlib-fast-paths=" ^ string_of_bool stdlib_fast_paths
  ^ "\ncap-scope=" ^ capability_scope_text cap_scope

let runtime_policy_of_state st =
  runtime_policy_text ~cache_scope:st.cache_scope ~stdlib_fast_paths:st.stdlib_fast_paths
    ~cap_scope:st.cap_scope

let rec value_to_string = function
  | VThunk thunk -> value_to_string (force_value (VThunk thunk))
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
  | VAttribute a -> vattr_to_string a
  | VClosure (t, body, _, _) ->
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
  | VNode (tag, attrs, children) ->
      "(node " ^ Ast.quote tag ^ " ["
      ^ String.concat ", " (List.map vattr_to_string attrs)
      ^ "] ["
      ^ String.concat ", " (List.map view_to_string children)
      ^ "])"

and vattr_to_string = function
  | VAttr (name, value) -> "(attr " ^ Ast.quote name ^ " " ^ Ast.quote value ^ ")"
  | VOn (event, msg) -> "(on " ^ Ast.quote event ^ " " ^ value_to_string msg ^ ")"

let rec value_to_cache_key = function
  | VThunk thunk -> value_to_cache_key (force_value (VThunk thunk))
  | VUnit -> "(Unit)"
  | VBool b -> "(Bool " ^ string_of_bool b ^ ")"
  | VNat n -> "(Nat " ^ string_of_int n ^ ")"
  | VString s -> "(String " ^ Ast.quote s ^ ")"
  | VList (typ, xs) ->
      "(List " ^ Kernel.type_to_canonical typ ^ " "
      ^ String.concat " " (List.map value_to_cache_key xs) ^ ")"
  | VRecord fields ->
      "(Record "
      ^ String.concat " "
          (List.map
             (fun (n, v) -> "(" ^ n ^ " " ^ value_to_cache_key v ^ ")")
             (sort_fields fields))
      ^ ")"
  | VVariant (typ, con, value) ->
      "(Variant " ^ Kernel.type_to_canonical typ ^ " " ^ con ^ " "
      ^ value_to_cache_key value ^ ")"
  | VView view -> "(View " ^ view_to_cache_key view ^ ")"
  | VAttribute a -> "(Attribute " ^ vattr_to_cache_key a ^ ")"
  | VClosure (typ, body, env, cap_scope) ->
      "(Closure " ^ Kernel.type_to_canonical typ ^ " " ^ Kernel.cterm_to_string body
      ^ " (env " ^ String.concat " " (List.map value_to_cache_key env)
      ^ ") (cap-scope " ^ String.concat " " (List.sort_uniq String.compare cap_scope)
      ^ "))"
  | VBuiltinSucc -> "BuiltinSucc"
  | VProcessDone value -> "(Done " ^ value_to_cache_key value ^ ")"
  | VProcessRequest suspended -> "(Suspended " ^ suspended_to_cache_key suspended ^ ")"

and view_to_cache_key = function
  | VText s -> "(Text " ^ Ast.quote s ^ ")"
  | VImage (src, alt) -> "(Image " ^ Ast.quote src ^ " " ^ Ast.quote alt ^ ")"
  | VButton (label, msg) -> "(Button " ^ Ast.quote label ^ " " ^ value_to_cache_key msg ^ ")"
  | VInput (value, handler) ->
      "(Input " ^ Ast.quote value ^ " " ^ value_to_cache_key handler ^ ")"
  | VColumn children ->
      "(Column " ^ String.concat " " (List.map view_to_cache_key children) ^ ")"
  | VRow children -> "(Row " ^ String.concat " " (List.map view_to_cache_key children) ^ ")"
  | VNode (tag, attrs, children) ->
      "(Node " ^ Ast.quote tag ^ " ("
      ^ String.concat " " (List.map vattr_to_cache_key attrs)
      ^ ") ("
      ^ String.concat " " (List.map view_to_cache_key children)
      ^ "))"

and vattr_to_cache_key = function
  | VAttr (name, value) -> "(Attr " ^ Ast.quote name ^ " " ^ Ast.quote value ^ ")"
  | VOn (event, msg) -> "(On " ^ Ast.quote event ^ " " ^ value_to_cache_key msg ^ ")"

and continuation_to_cache_key = function
  | KDone -> "KDone"
  | KBind (inner, body, env, cap_scope) ->
      "(KBind " ^ continuation_to_cache_key inner ^ " " ^ Kernel.cterm_to_string body
      ^ " (env " ^ String.concat " " (List.map value_to_cache_key env)
      ^ ") (cap-scope " ^ String.concat " " (List.sort_uniq String.compare cap_scope)
      ^ "))"

and suspended_to_cache_key suspended =
  "(request " ^ Kernel.req_to_canonical suspended.req ^ ") (cont "
  ^ continuation_to_cache_key suspended.cont ^ ") (cap-scope "
  ^ String.concat " " (List.sort_uniq String.compare suspended.cap_scope) ^ ")"

let recur_stack_to_cache_key frames =
  match frames with
  | [] -> "recur:none"
  | _ ->
      "recur:"
      ^ String.concat ">"
          (List.map (fun frame -> Kernel.hash_string (Lazy.force frame.recur_key)) frames)

let app_cache_key_budget = 400

let consume_list consume budget xs =
  List.fold_left
    (fun acc x -> match acc with None -> None | Some budget -> consume budget x)
    (Some budget) xs

let rec consume_cterm budget term =
  if budget <= 0 then None
  else
    let budget = budget - 1 in
    match term with
    | Kernel.CUnit | CBool _ | CNat _ | CString _ | CVar _ | CGlobal _ | CInst _
    | CRequest _ | CNil _ ->
        Some budget
    | CLambda (_, body) | CField (body, _) | CVariant (_, _, body) | CRecur body
    | CText body | CColumn body | CRow body | CDone body ->
        consume_cterm budget body
    | CApp (a, b) | CLet (a, b) | CImage (a, b) | CButton (a, b) | CInput (a, b)
    | CListView (a, b) | CWhenView (a, b) | CAttr (a, b) | COn (a, b)
    | CBind (a, _, b) ->
        Option.bind (consume_cterm budget a) (fun budget -> consume_cterm budget b)
    | CRecord fields -> consume_list consume_cterm budget (List.map snd fields)
    | CCase (scrutinee, branches) ->
        Option.bind (consume_cterm budget scrutinee) (fun budget ->
            consume_list consume_cbranch budget branches)
    | CFoldNat (a, b, c) | CFoldList (a, b, c) | CCaseList (a, b, c)
    | CNode (a, b, c) ->
        Option.bind
          (Option.bind (consume_cterm budget a) (fun budget -> consume_cterm budget b))
          (fun budget -> consume_cterm budget c)
    | CFoldVariant (_, _, scrutinee, branches) ->
        Option.bind (consume_cterm budget scrutinee) (fun budget ->
            consume_list consume_cbranch budget branches)
    | CCons (_, head, tail) ->
        Option.bind (consume_cterm budget head) (fun budget -> consume_cterm budget tail)

and consume_cbranch budget = function
  | Kernel.CBBool (_, body) | CBVariant (_, body) -> consume_cterm budget body

let rec consume_cache_value budget value =
  if budget <= 0 then None
  else
    let budget = budget - 1 in
    match value with
    | VThunk thunk -> consume_cache_value budget (force_value (VThunk thunk))
    | VUnit | VBool _ | VNat _ | VBuiltinSucc -> Some budget
    | VString s -> if String.length s > 1024 then None else Some budget
    | VList (_, xs) -> consume_list consume_cache_value budget xs
    | VRecord fields -> consume_list consume_cache_value budget (List.map snd fields)
    | VVariant (_, _, value) | VProcessDone value -> consume_cache_value budget value
    | VView view -> consume_cache_view budget view
    | VAttribute attr -> consume_cache_attr budget attr
    | VClosure (_, body, env, _) ->
        Option.bind (consume_cterm budget body) (fun budget ->
            consume_list consume_cache_value budget env)
    | VProcessRequest suspended -> consume_cache_suspended budget suspended

and consume_cache_view budget = function
  | _ when budget <= 0 -> None
  | VText s -> if String.length s > 1024 then None else Some (budget - 1)
  | VImage (src, alt) ->
      if String.length src + String.length alt > 2048 then None else Some (budget - 1)
  | VButton (_, msg) | VInput (_, msg) -> consume_cache_value (budget - 1) msg
  | VColumn children | VRow children -> consume_list consume_cache_view (budget - 1) children
  | VNode (tag, attrs, children) ->
      if String.length tag > 256 then None
      else
        Option.bind (consume_list consume_cache_attr (budget - 1) attrs) (fun budget ->
            consume_list consume_cache_view budget children)

and consume_cache_attr budget = function
  | _ when budget <= 0 -> None
  | VAttr (name, value) ->
      if String.length name + String.length value > 2048 then None else Some (budget - 1)
  | VOn (_, msg) -> consume_cache_value (budget - 1) msg

and consume_cache_continuation budget = function
  | _ when budget <= 0 -> None
  | KDone -> Some (budget - 1)
  | KBind (inner, body, env, _) ->
      Option.bind
        (Option.bind (consume_cache_continuation (budget - 1) inner) (fun budget ->
             consume_cterm budget body))
        (fun budget -> consume_list consume_cache_value budget env)

and consume_cache_suspended budget suspended =
  if budget <= 0 then None else
  consume_cache_continuation (budget - 1) suspended.cont

let app_cache_key st fv av =
  match consume_cache_value app_cache_key_budget fv with
  | None -> None
  | Some budget -> (
      match consume_cache_value budget av with
      | None -> None
      | Some _ ->
          Some
            (Kernel.hash_string
               ("app-v5:" ^ runtime_policy_of_state st ^ "\nrecur="
              ^ recur_stack_to_cache_key st.recur_stack ^ "\nfunction="
              ^ value_to_cache_key fv ^ "\nargument=" ^ value_to_cache_key av)))

let trace st line = if st.trace_cache then st.trace <- line :: st.trace

let rec force_value_traced st = function
  | VThunk thunk -> (
      match thunk.thunk_value with
      | Some value -> force_value_traced st value
      | None ->
          trace st "force let";
          let value = thunk.thunk_eval () in
          thunk.thunk_value <- Some value;
          force_value_traced st value)
  | value -> value

let has_prefix prefix s =
  let lp = String.length prefix in
  String.length s >= lp && String.sub s 0 lp = prefix

let strip_prefix prefix s =
  if has_prefix prefix s then
    Some (String.sub s (String.length prefix) (String.length s - String.length prefix))
  else None

let rec cache_value_to_canonical = function
  | VThunk thunk -> cache_value_to_canonical (force_value (VThunk thunk))
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
  | VAttribute _ -> None
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
  Store.ensure_dir_cached dir;
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
      Store.ensure_dir_cached dir;
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

let persistent_cache_entries dir =
  if not (Sys.file_exists dir) then []
  else
    Sys.readdir dir |> Array.to_list
    |> List.filter (fun f -> Filename.check_suffix f ".cache")
    |> List.map (fun f -> String.sub f 0 (String.length f - String.length ".cache"))
    |> List.sort String.compare

(* Global references are resolved on every [CGlobal]/[CInst] evaluation, so a
   linear scan over all definitions dominates large programs. The index keeps
   the first definition per name/def_id, matching [List.find_opt] order. *)
let def_index_memo : (Kernel.checked * (string, Kernel.checked_def) Hashtbl.t) option ref =
  ref None

let def_index checked =
  match !def_index_memo with
  | Some (c, index) when c == checked -> index
  | _ ->
      let index = Hashtbl.create 1024 in
      List.iter
        (fun (d : Kernel.checked_def) ->
          if not (Hashtbl.mem index d.Kernel.def.name) then
            Hashtbl.add index d.Kernel.def.name d;
          if not (Hashtbl.mem index d.Kernel.def_id) then Hashtbl.add index d.Kernel.def_id d)
        checked.Kernel.defs;
      def_index_memo := Some (checked, index);
      index

let def_by_ref checked n = Hashtbl.find_opt (def_index checked) n

(* [CInst] re-parses the serialized canonical body on every evaluation of a
   polymorphic reference. The parsed form is immutable and keyed by the exact
   serialized text, so sharing it is observation-free. *)
let parsed_def_memo : (string, Kernel.canonical_def) Hashtbl.t = Hashtbl.create 512

let parse_serialized_def_memo canonical =
  match Hashtbl.find_opt parsed_def_memo canonical with
  | Some parsed -> parsed
  | None ->
      let parsed = Kernel.parse_serialized_def canonical in
      Hashtbl.add parsed_def_memo canonical parsed;
      parsed

let merge_caps a b = List.sort_uniq String.compare (a @ b)

(* Both lists are sorted+deduped everywhere scopes are produced (checker output
   and [merge_caps]); if that ever fails to hold the subset test only returns a
   false negative and the slow path keeps the exact semantics. *)
let rec subset_sorted a b =
  match (a, b) with
  | [], _ -> true
  | _, [] -> false
  | x :: xs, y :: ys ->
      let c = String.compare x y in
      if c = 0 then subset_sorted xs ys else if c > 0 then subset_sorted a ys else false

let eval_with_cap_scope st caps f =
  if caps == [] || subset_sorted caps st.cap_scope then
    (* The merge would leave the scope unchanged: no mutation, no protect. *)
    f ()
  else
    let previous = st.cap_scope in
    st.cap_scope <- merge_caps st.cap_scope caps;
    Fun.protect ~finally:(fun () -> st.cap_scope <- previous) f

let maybe_type typ = TVariant (sort_fields [ ("None", TUnit); ("Some", typ) ])

let pair_type key_typ value_typ = TNamed ("Pair", [ key_typ; value_typ ])

let primitive_poly_value name args =
  match (name, args) with
  | "List.map", [ _; out_typ ] -> Some (VClosure (out_typ, Kernel.CGlobal "prim.List.map", [], []))
  | "List.length", [ _ ] -> Some (VClosure (TUnit, Kernel.CGlobal "prim.List.length", [], []))
  | "List.append", [ item_typ ] ->
      Some (VClosure (item_typ, Kernel.CGlobal "prim.List.append", [], []))
  | "List.reverse", [ item_typ ] ->
      Some (VClosure (item_typ, Kernel.CGlobal "prim.List.reverse", [], []))
  | "List.any", [ _ ] -> Some (VClosure (TUnit, Kernel.CGlobal "prim.List.any", [], []))
  | "List.all", [ _ ] -> Some (VClosure (TUnit, Kernel.CGlobal "prim.List.all", [], []))
  | "List.member", [ _ ] -> Some (VClosure (TUnit, Kernel.CGlobal "prim.List.member", [], []))
  | "List.find", [ item_typ ] ->
      Some (VClosure (item_typ, Kernel.CGlobal "prim.List.find", [], []))
  | "Assoc.empty", [ key_typ; value_typ ] -> Some (VList (pair_type key_typ value_typ, []))
  | "Assoc.insert", [ key_typ; value_typ ] ->
      Some (VClosure (pair_type key_typ value_typ, Kernel.CGlobal "prim.Assoc.insert", [], []))
  | "Assoc.get", [ _; value_typ ] ->
      Some (VClosure (value_typ, Kernel.CGlobal "prim.Assoc.get", [], []))
  | "Assoc.contains", [ _; _ ] ->
      Some (VClosure (TUnit, Kernel.CGlobal "prim.Assoc.contains", [], []))
  | "Assoc.keys", [ key_typ; _ ] ->
      Some (VClosure (key_typ, Kernel.CGlobal "prim.Assoc.keys", [], []))
  | "Assoc.values", [ _; value_typ ] ->
      Some (VClosure (value_typ, Kernel.CGlobal "prim.Assoc.values", [], []))
  | _ -> None

let record_field field = function
  | VRecord fields -> (
      match Kernel.assoc_opt field fields with
      | Some value -> value
      | None -> fail ("runtime record missing field: " ^ field))
  | value -> fail ("expected record while reading field " ^ field ^ ", got " ^ value_to_string value)

let expect_bool context = function
  | VBool value -> value
  | value -> fail (context ^ " returned non-Bool runtime value: " ^ value_to_string value)

let rec nth_env st env i =
  match (env, i) with
  | v :: _, 0 -> force_value_traced st v
  | _ :: rest, n when n > 0 -> nth_env st rest (n - 1)
  | _ -> fail ("unbound canonical variable #" ^ string_of_int i)

let rec eval_cterm st env = function
  | Kernel.CUnit -> VUnit
  | Kernel.CBool b -> VBool b
  | Kernel.CNat n -> VNat n
  | Kernel.CString s -> VString s
  | Kernel.CVar i -> nth_env st env i
  | Kernel.CGlobal n -> (
      match n with
      | "succ" -> VBuiltinSucc
      | "prim.Nat.add" | "prim.Nat.mul" | "prim.Nat.pred" | "prim.Nat.sub" | "prim.Nat.eq"
      | "prim.Nat.lte" | "prim.Nat.lt" | "prim.Nat.gte" | "prim.Nat.gt" | "prim.Nat.toString"
      | "prim.String.concat" | "prim.String.eq" | "prim.String.length" | "prim.String.slice"
      | "prim.String.charAt" ->
          VClosure (TUnit, Kernel.CGlobal n, [], [])
      | _ -> eval_def st n)
  | Kernel.CInst (n, args) -> (
      match def_by_ref st.checked n with
      | None -> fail ("unknown polymorphic definition at runtime: " ^ n)
      | Some d -> (
          match
            if st.stdlib_fast_paths then primitive_poly_value d.def.name args else None
          with
          | Some value -> value
          | None ->
              let canonical = parse_serialized_def_memo d.canonical in
              eval_with_cap_scope st d.capabilities (fun () ->
                  eval_cterm st [] (Kernel.subst_type_in_cterm args canonical.cbody))))
  | Kernel.CLambda (t, body) -> VClosure (t, body, env, st.cap_scope)
  | Kernel.CApp (f, arg) ->
      let fv = eval_cterm st env f in
      let av = eval_cterm st env arg in
      eval_app st fv av
  | Kernel.CLet (e, body) ->
      trace st "thunk let";
      let recur_stack = st.recur_stack in
      let cap_scope = st.cap_scope in
      let thunk_eval () =
        let previous_recur_stack = st.recur_stack in
        let previous_cap_scope = st.cap_scope in
        st.recur_stack <- recur_stack;
        st.cap_scope <- cap_scope;
        Fun.protect
          ~finally:(fun () ->
            st.recur_stack <- previous_recur_stack;
            st.cap_scope <- previous_cap_scope)
          (fun () -> eval_cterm st env e)
      in
      let thunk = VThunk { thunk_value = None; thunk_eval } in
      eval_cterm st (thunk :: env) body
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
  | Kernel.CFoldVariant (target, result, scrut, branches) ->
      (* Captured eagerly: the lazy key must reflect the scope at fold entry,
         not whatever the mutable scope holds when a cache key forces it. *)
      let scope_at_entry = st.cap_scope in
      let recur_key =
        lazy
          ("foldVariant:"
          ^ Kernel.cterm_to_string (Kernel.CFoldVariant (target, result, scrut, branches))
          ^ " (env " ^ String.concat " " (List.map value_to_cache_key env)
          ^ ") (cap-scope " ^ String.concat " " (List.sort_uniq String.compare scope_at_entry) ^ ")")
      in
      let rec fold value =
        match value with
        | VVariant (_, con, payload) ->
            let body =
              branches
              |> List.find_map (function
                   | Kernel.CBVariant (con', body) when String.equal con con' -> Some body
                   | _ -> None)
            in
            let body = Kernel.option_or_fail ("missing foldVariant branch at runtime: " ^ con) body in
            let previous = st.recur_stack in
            st.recur_stack <- { recur_key; recur_apply = fold } :: previous;
            Fun.protect
              ~finally:(fun () -> st.recur_stack <- previous)
              (fun () -> eval_cterm st (payload :: env) body)
        | v -> fail ("foldVariant on non-Variant runtime value: " ^ value_to_string v)
      in
      fold (eval_cterm st env scrut)
  | Kernel.CRecur e -> (
      match st.recur_stack with
      | frame :: _ -> frame.recur_apply (eval_cterm st env e)
      | [] -> fail "recur outside foldVariant at runtime")
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
  | Kernel.CCaseList (xs, nil_body, cons_body) -> (
      match eval_cterm st env xs with
      | VList (_, []) -> eval_cterm st env nil_body
      | VList (item_ty, head :: tail) ->
          eval_cterm st (head :: VList (item_ty, tail) :: env) cons_body
      | v -> fail ("caseList on non-List runtime value: " ^ value_to_string v))
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
  | Kernel.CNode (tag, attrs, children) -> (
      match eval_cterm st env tag with
      | VString tag -> (
          match (eval_cterm st env attrs, eval_cterm st env children) with
          | VList (_, attr_items), VList (_, child_items) ->
              VView (VNode (tag, List.map expect_attr attr_items, List.map expect_view child_items))
          | VList _, v -> fail ("node children on non-List runtime value: " ^ value_to_string v)
          | v, _ -> fail ("node attributes on non-List runtime value: " ^ value_to_string v))
      | v -> fail ("node tag on non-String runtime value: " ^ value_to_string v))
  | Kernel.CAttr (name, value) -> (
      match (eval_cterm st env name, eval_cterm st env value) with
      | VString name, VString value -> VAttribute (VAttr (name, value))
      | VString _, v -> fail ("attr value on non-String runtime value: " ^ value_to_string v)
      | v, _ -> fail ("attr name on non-String runtime value: " ^ value_to_string v))
  | Kernel.COn (event, msg) -> (
      match eval_cterm st env event with
      | VString event -> VAttribute (VOn (event, eval_cterm st env msg))
      | v -> fail ("on event on non-String runtime value: " ^ value_to_string v))
  | Kernel.CDone e -> VProcessDone (eval_cterm st env e)
  | Kernel.CRequest req -> VProcessRequest { req; cont = KDone; cap_scope = st.cap_scope }
  | Kernel.CBind (p, _, body) -> (
      match eval_cterm st env p with
      | VProcessDone v -> eval_cterm st (v :: env) body
      | VProcessRequest s ->
          VProcessRequest { s with cont = KBind (s.cont, body, env, st.cap_scope) }
      | other -> fail ("bind on non-process runtime value: " ^ value_to_string other))

and expect_view = function
  | VView view -> view
  | v -> fail ("expected View runtime value, got " ^ value_to_string v)

and expect_attr = function
  | VAttribute a -> a
  | v -> fail ("expected Attr runtime value, got " ^ value_to_string v)

(* Hashing every application's function/argument values to build a cache key
   costs far more than re-evaluating most applications: the serialized closure
   environments grow with the program and dominate runtime (sha256 over MBs per
   call). Evaluation is pure and deterministic, so the cache can never change a
   result — it is skipped entirely unless the caller opted into the persistent
   cache or cache tracing, which are the only observers of cache keys. *)
and eval_app st fv av =
  if st.cache_dir = None && not st.trace_cache then apply_value st fv av
  else (
    match app_cache_key st fv av with
    | None ->
        trace st "cache skip";
        apply_value st fv av
    | Some key -> (
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
                let result = apply_value st fv av in
                Hashtbl.add st.app_cache key result;
                persistent_cache_put st key result;
                result)))

and apply_value st fv av =
  match (fv, av) with
  | VBuiltinSucc, _ -> (
      match av with
      | VNat n -> VNat (n + 1)
      | v -> fail ("builtin on " ^ value_to_string v))
  | VClosure (_, Kernel.CGlobal "prim.Nat.add", closure_env, _), _ -> (
      match (closure_env, av) with
      | [], VNat _ -> VClosure (TNat, Kernel.CGlobal "prim.Nat.add", [ av ], [])
      | [ VNat a ], VNat b -> VNat (a + b)
      | _ -> fail "prim.Nat.add expects Nat Nat")
  | VClosure (_, Kernel.CGlobal "prim.Nat.mul", closure_env, _), _ -> (
      match (closure_env, av) with
      | [], VNat _ -> VClosure (TNat, Kernel.CGlobal "prim.Nat.mul", [ av ], [])
      | [ VNat a ], VNat b -> VNat (a * b)
      | _ -> fail "prim.Nat.mul expects Nat Nat")
  | VClosure (_, Kernel.CGlobal "prim.Nat.pred", closure_env, _), _ -> (
      match (closure_env, av) with
      | [], VNat n -> VNat (max 0 (n - 1))
      | _ -> fail "prim.Nat.pred expects Nat")
  | VClosure (_, Kernel.CGlobal "prim.Nat.sub", closure_env, _), _ -> (
      match (closure_env, av) with
      | [], VNat _ -> VClosure (TNat, Kernel.CGlobal "prim.Nat.sub", [ av ], [])
      | [ VNat a ], VNat b -> VNat (max 0 (a - b))
      | _ -> fail "prim.Nat.sub expects Nat Nat")
  | VClosure (_, Kernel.CGlobal "prim.Nat.eq", closure_env, _), _ -> (
      match (closure_env, av) with
      | [], VNat _ -> VClosure (TNat, Kernel.CGlobal "prim.Nat.eq", [ av ], [])
      | [ VNat a ], VNat b -> VBool (a = b)
      | _ -> fail "prim.Nat.eq expects Nat Nat")
  | VClosure (_, Kernel.CGlobal "prim.Nat.lte", closure_env, _), _ -> (
      match (closure_env, av) with
      | [], VNat _ -> VClosure (TBool, Kernel.CGlobal "prim.Nat.lte", [ av ], [])
      | [ VNat a ], VNat b -> VBool (a <= b)
      | _ -> fail "prim.Nat.lte expects Nat Nat")
  | VClosure (_, Kernel.CGlobal "prim.Nat.lt", closure_env, _), _ -> (
      match (closure_env, av) with
      | [], VNat _ -> VClosure (TBool, Kernel.CGlobal "prim.Nat.lt", [ av ], [])
      | [ VNat a ], VNat b -> VBool (a < b)
      | _ -> fail "prim.Nat.lt expects Nat Nat")
  | VClosure (_, Kernel.CGlobal "prim.Nat.gte", closure_env, _), _ -> (
      match (closure_env, av) with
      | [], VNat _ -> VClosure (TBool, Kernel.CGlobal "prim.Nat.gte", [ av ], [])
      | [ VNat a ], VNat b -> VBool (a >= b)
      | _ -> fail "prim.Nat.gte expects Nat Nat")
  | VClosure (_, Kernel.CGlobal "prim.Nat.gt", closure_env, _), _ -> (
      match (closure_env, av) with
      | [], VNat _ -> VClosure (TBool, Kernel.CGlobal "prim.Nat.gt", [ av ], [])
      | [ VNat a ], VNat b -> VBool (a > b)
      | _ -> fail "prim.Nat.gt expects Nat Nat")
  | VClosure (_, Kernel.CGlobal "prim.Nat.toString", closure_env, _), _ -> (
      match (closure_env, av) with
      | [], VNat n -> VString (string_of_int n)
      | _ -> fail "prim.Nat.toString expects Nat")
  | VClosure (_, Kernel.CGlobal "prim.String.concat", closure_env, _), _ -> (
      match (closure_env, av) with
      | [], VString _ -> VClosure (TString, Kernel.CGlobal "prim.String.concat", [ av ], [])
      | [ VString a ], VString b -> VString (a ^ b)
      | _ -> fail "prim.String.concat expects String String")
  | VClosure (_, Kernel.CGlobal "prim.String.eq", closure_env, _), _ -> (
      match (closure_env, av) with
      | [], VString _ -> VClosure (TString, Kernel.CGlobal "prim.String.eq", [ av ], [])
      | [ VString a ], VString b -> VBool (String.equal a b)
      | _ -> fail "prim.String.eq expects String String")
  | VClosure (_, Kernel.CGlobal "prim.String.length", closure_env, _), _ -> (
      match (closure_env, av) with
      | [], VString s -> VNat (String_prim.length s)
      | _ -> fail "prim.String.length expects String")
  | VClosure (_, Kernel.CGlobal "prim.String.slice", closure_env, _), _ -> (
      match (closure_env, av) with
      | [], VString _ -> VClosure (TNat, Kernel.CGlobal "prim.String.slice", [ av ], [])
      | [ VString _ ], VNat _ ->
          VClosure (TNat, Kernel.CGlobal "prim.String.slice", closure_env @ [ av ], [])
      | [ VString s; VNat start ], VNat count -> VString (String_prim.slice s start count)
      | _ -> fail "prim.String.slice expects String Nat Nat")
  | VClosure (_, Kernel.CGlobal "prim.String.charAt", closure_env, _), _ -> (
      match (closure_env, av) with
      | [], VString _ -> VClosure (TNat, Kernel.CGlobal "prim.String.charAt", [ av ], [])
      | [ VString s ], VNat index ->
          if index < 0 || index >= String_prim.length s then
            VVariant (maybe_type TString, "None", VUnit)
          else VVariant (maybe_type TString, "Some", VString (String_prim.slice s index 1))
      | _ -> fail "prim.String.charAt expects String Nat")
  | VClosure (_, Kernel.CGlobal "prim.List.length", [], _), VList (_, xs) ->
      VNat (List.length xs)
  | VClosure (item_typ, Kernel.CGlobal "prim.List.append", [], _), VList (_, xs) ->
      VClosure (item_typ, Kernel.CGlobal "prim.List.append", [ VList (item_typ, xs) ], [])
  | VClosure (item_typ, Kernel.CGlobal "prim.List.append", [ VList (_, xs) ], _), VList (_, ys)
    ->
      VList (item_typ, xs @ ys)
  | VClosure (item_typ, Kernel.CGlobal "prim.List.reverse", [], _), VList (_, xs) ->
      VList (item_typ, List.rev xs)
  | VClosure (out_typ, Kernel.CGlobal "prim.List.map", [], _), VList (_, xs) ->
      VClosure (out_typ, Kernel.CGlobal "prim.List.map", [ VList (TUnit, xs) ], [])
  | VClosure (out_typ, Kernel.CGlobal "prim.List.map", [ VList (_, xs) ], _), fn ->
      VList (out_typ, List.map (fun item -> eval_app st fn item) xs)
  | VClosure (_, Kernel.CGlobal "prim.List.any", [], _), VList (_, xs) ->
      VClosure (TUnit, Kernel.CGlobal "prim.List.any", [ VList (TUnit, xs) ], [])
  | VClosure (_, Kernel.CGlobal "prim.List.any", [ VList (_, xs) ], _), pred ->
      VBool (List.exists (fun item -> expect_bool "List.any predicate" (eval_app st pred item)) xs)
  | VClosure (_, Kernel.CGlobal "prim.List.all", [], _), VList (_, xs) ->
      VClosure (TUnit, Kernel.CGlobal "prim.List.all", [ VList (TUnit, xs) ], [])
  | VClosure (_, Kernel.CGlobal "prim.List.all", [ VList (_, xs) ], _), pred ->
      VBool (List.for_all (fun item -> expect_bool "List.all predicate" (eval_app st pred item)) xs)
  | VClosure (_, Kernel.CGlobal "prim.List.member", [], _), eq ->
      VClosure (TUnit, Kernel.CGlobal "prim.List.member", [ eq ], [])
  | VClosure (_, Kernel.CGlobal "prim.List.member", [ eq ], _), value ->
      VClosure (TUnit, Kernel.CGlobal "prim.List.member", [ eq; value ], [])
  | VClosure (_, Kernel.CGlobal "prim.List.member", [ eq; value ], _), VList (_, xs) ->
      VBool
        (List.exists
           (fun item -> expect_bool "List.member equality" (eval_app st (eval_app st eq item) value))
           xs)
  | VClosure (item_typ, Kernel.CGlobal "prim.List.find", [], _), VList (_, xs) ->
      VClosure (item_typ, Kernel.CGlobal "prim.List.find", [ VList (item_typ, xs) ], [])
  | VClosure (item_typ, Kernel.CGlobal "prim.List.find", [ VList (_, xs) ], _), pred -> (
      match
        List.find_opt (fun item -> expect_bool "List.find predicate" (eval_app st pred item)) xs
      with
      | Some item -> VVariant (maybe_type item_typ, "Some", item)
      | None -> VVariant (maybe_type item_typ, "None", VUnit))
  | VClosure (pair_typ, Kernel.CGlobal "prim.Assoc.insert", [], _), key ->
      VClosure (pair_typ, Kernel.CGlobal "prim.Assoc.insert", [ key ], [])
  | VClosure (pair_typ, Kernel.CGlobal "prim.Assoc.insert", [ key ], _), value ->
      VClosure (pair_typ, Kernel.CGlobal "prim.Assoc.insert", [ key; value ], [])
  | VClosure (pair_typ, Kernel.CGlobal "prim.Assoc.insert", [ key; value ], _), VList (_, entries)
    ->
      VList (pair_typ, VRecord [ ("first", key); ("second", value) ] :: entries)
  | VClosure (value_typ, Kernel.CGlobal "prim.Assoc.get", [], _), eq ->
      VClosure (value_typ, Kernel.CGlobal "prim.Assoc.get", [ eq ], [])
  | VClosure (value_typ, Kernel.CGlobal "prim.Assoc.get", [ eq ], _), key ->
      VClosure (value_typ, Kernel.CGlobal "prim.Assoc.get", [ eq; key ], [])
  | VClosure (value_typ, Kernel.CGlobal "prim.Assoc.get", [ eq; key ], _), VList (_, entries)
    -> (
      match
        List.find_opt
          (fun entry ->
            let entry_key = record_field "first" entry in
            expect_bool "Assoc.get equality" (eval_app st (eval_app st eq entry_key) key))
          entries
      with
      | Some entry -> VVariant (maybe_type value_typ, "Some", record_field "second" entry)
      | None -> VVariant (maybe_type value_typ, "None", VUnit))
  | VClosure (_, Kernel.CGlobal "prim.Assoc.contains", [], _), eq ->
      VClosure (TUnit, Kernel.CGlobal "prim.Assoc.contains", [ eq ], [])
  | VClosure (_, Kernel.CGlobal "prim.Assoc.contains", [ eq ], _), key ->
      VClosure (TUnit, Kernel.CGlobal "prim.Assoc.contains", [ eq; key ], [])
  | VClosure (_, Kernel.CGlobal "prim.Assoc.contains", [ eq; key ], _), VList (_, entries) ->
      VBool
        (List.exists
           (fun entry ->
             let entry_key = record_field "first" entry in
             expect_bool "Assoc.contains equality" (eval_app st (eval_app st eq entry_key) key))
           entries)
  | VClosure (key_typ, Kernel.CGlobal "prim.Assoc.keys", [], _), VList (_, entries) ->
      VList (key_typ, List.map (record_field "first") entries)
  | VClosure (value_typ, Kernel.CGlobal "prim.Assoc.values", [], _), VList (_, entries) ->
      VList (value_typ, List.map (record_field "second") entries)
  | VClosure (_, body, closure_env, cap_scope), _ ->
      eval_with_cap_scope st cap_scope (fun () -> eval_cterm st (av :: closure_env) body)
  | v, _ -> fail ("application of non-function runtime value: " ^ value_to_string v)

and eval_key_for_checked_def st (d : Kernel.checked_def) =
  let cap_scope = merge_caps st.cap_scope d.capabilities in
  eval_key ~def_id:d.def_id ~args_hash:no_args_hash
    ~runtime_policy:
      (runtime_policy_text ~cache_scope:st.cache_scope ~stdlib_fast_paths:st.stdlib_fast_paths
         ~cap_scope)

and eval_def st n =
  match Hashtbl.find_opt st.def_cache n with
  | Some v -> v
  | None -> (
      match def_by_ref st.checked n with
      | None -> fail ("unknown definition at runtime: " ^ n)
      | Some d ->
          let key = eval_key_for_checked_def st d in
          (match persistent_cache_get st key with
          | Some v ->
              trace st ("cache hit eval " ^ key);
              Hashtbl.add st.def_cache n v;
              v
          | None ->
              trace st ("cache miss eval " ^ key);
          let canonical = parse_serialized_def_memo d.canonical in
          let v = eval_with_cap_scope st d.capabilities (fun () -> eval_cterm st [] canonical.cbody) in
          Hashtbl.add st.def_cache n v;
              persistent_cache_put st key v;
              v))

(* The default cache scope is a hash of the whole program. [eval_entry] /
   [normalize_def] are frequently called many times on the *same* checked
   program, and hashing every definition on each call dominates runtime. The
   hash is a pure function of [checked], so memoize it by physical identity of
   the program (single most-recent entry covers the common repeated-call case).
   The returned value is identical, so cache keys and determinism are unchanged. *)
let cache_scope_memo : (Kernel.checked * string) option ref = ref None

let program_cache_scope checked =
  match !cache_scope_memo with
  | Some (c, h) when c == checked -> h
  | _ ->
      let h = Kernel.hash_program checked in
      cache_scope_memo := Some (checked, h);
      h

let eval_runtime_policy ?(stdlib_fast_paths = false) ?cache_scope ?(cap_scope = []) checked =
  let scope =
    match cache_scope with Some scope -> scope | None -> program_cache_scope checked
  in
  runtime_policy_text ~cache_scope:scope ~stdlib_fast_paths ~cap_scope

let eval_key_for_def ?(stdlib_fast_paths = false) ?cache_scope ?cap_scope checked name =
  match def_by_ref checked name with
  | None -> fail ("unknown definition for eval key: " ^ name)
  | Some d ->
      let cap_scope =
        match cap_scope with Some caps -> caps | None -> d.capabilities
      in
      eval_key ~def_id:d.def_id ~args_hash:no_args_hash
        ~runtime_policy:(eval_runtime_policy ~stdlib_fast_paths ?cache_scope ~cap_scope checked)

let process_eval_key_for_def ?(stdlib_fast_paths = false) ?cache_scope ?cap_scope ~world_ref
    checked name =
  match def_by_ref checked name with
  | None -> fail ("unknown definition for process eval key: " ^ name)
  | Some d ->
      let cap_scope =
        match cap_scope with Some caps -> caps | None -> d.capabilities
      in
      process_eval_key ~def_id:d.def_id ~world_ref ~cap_scope
        ~runtime_policy:(eval_runtime_policy ~stdlib_fast_paths ?cache_scope ~cap_scope checked)

(* Normal forms are pure functions of the program, yet the default
   [eval_entry]/[normalize_def] path rebuilt empty caches on every call. Reuse
   caches across calls on the same checked program. [eval_app] skips caching
   structurally large function/argument pairs before building a textual key, so
   this shared app cache keeps cheap repeated calls without retaining the huge
   closure keys that dominate interpreted self-hosted tests. *)
let shared_caches_memo :
    (Kernel.checked * (string, value) Hashtbl.t * (string, value) Hashtbl.t) option ref =
  ref None

let shared_caches checked =
  match !shared_caches_memo with
  | Some (c, dc, ac) when c == checked -> (dc, ac)
  | _ ->
      let dc = Hashtbl.create 256 and ac = Hashtbl.create 512 in
      shared_caches_memo := Some (checked, dc, ac);
      (dc, ac)

let state ?(trace_cache = false) ?(stdlib_fast_paths = false) ?cache_dir ?cache_scope checked =
  let scope =
    match cache_scope with Some scope -> scope | None -> program_cache_scope checked
  in
  let def_cache, app_cache =
    if cache_dir = None && (not trace_cache) && cache_scope = None then shared_caches checked
    else (Hashtbl.create 32, Hashtbl.create 64)
  in
  {
    checked;
    def_cache;
    app_cache;
    cache_dir;
    cache_scope = scope;
    trace = [];
    trace_cache;
    stdlib_fast_paths;
    recur_stack = [];
    cap_scope = [];
  }

let eval_entry ?(trace_cache = false) ?(stdlib_fast_paths = false) ?cache_dir ?cache_scope checked
    entry =
  let st = state ~trace_cache ~stdlib_fast_paths ?cache_dir ?cache_scope checked in
  let value = eval_def st entry in
  (value, List.rev st.trace)

let apply ?(stdlib_fast_paths = false) checked f arg =
  let st = state ~stdlib_fast_paths checked in
  eval_app st f arg

let normalize_def ?(trace_cache = false) ?(stdlib_fast_paths = false) ?cache_dir ?cache_scope checked
    name =
  eval_entry ~trace_cache ~stdlib_fast_paths ?cache_dir ?cache_scope checked name

let normalize_all checked =
  List.map
    (fun d -> (d.Kernel.def.Ast.name, fst (normalize_def checked d.Kernel.def.name)))
    checked.Kernel.defs

let rec value_to_canonical = function
  | VThunk thunk -> value_to_canonical (force_value (VThunk thunk))
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
  | VAttribute a -> "(Attribute " ^ vattr_to_canonical a ^ ")"
  | VClosure (typ, body, env, cap_scope) ->
      "(Closure " ^ Kernel.type_to_canonical typ ^ " " ^ Kernel.cterm_to_string body ^ " (env "
      ^ String.concat " " (List.map value_to_canonical env) ^ ") (cap-scope "
      ^ String.concat " " (List.sort_uniq String.compare cap_scope) ^ "))"
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
  | VNode (tag, attrs, children) ->
      "(Node " ^ Ast.quote tag ^ " ("
      ^ String.concat " " (List.map vattr_to_canonical attrs)
      ^ ") ("
      ^ String.concat " " (List.map view_to_canonical children)
      ^ "))"

and vattr_to_canonical = function
  | VAttr (name, value) -> "(Attr " ^ Ast.quote name ^ " " ^ Ast.quote value ^ ")"
  | VOn (event, msg) -> "(On " ^ Ast.quote event ^ " " ^ value_to_canonical msg ^ ")"

and continuation_to_canonical = function
  | KDone -> "KDone"
  | KBind (inner, body, env, cap_scope) ->
      "(KBind " ^ continuation_to_canonical inner ^ " " ^ Kernel.cterm_to_string body ^ " (env "
      ^ String.concat " " (List.map value_to_canonical env) ^ ") (cap-scope "
      ^ String.concat " " (List.sort_uniq String.compare cap_scope) ^ "))"

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
  | Sexp.List [ Sexp.Atom "Attribute"; a ] -> VAttribute (vattr_of_canonical_sexp a)
  | Sexp.List [ Sexp.Atom "Closure"; typ; body; Sexp.List (Sexp.Atom "env" :: env) ] ->
      VClosure
        ( Kernel.type_of_canonical_sexp typ,
          Kernel.cterm_of_canonical_sexp body,
          List.map value_of_canonical_sexp env,
          [] )
  | Sexp.List
      [
        Sexp.Atom "Closure";
        typ;
        body;
        Sexp.List (Sexp.Atom "env" :: env);
        Sexp.List (Sexp.Atom "cap-scope" :: caps);
      ] ->
      VClosure
        ( Kernel.type_of_canonical_sexp typ,
          Kernel.cterm_of_canonical_sexp body,
          List.map value_of_canonical_sexp env,
          List.map
            (function
              | Sexp.Atom cap -> cap
              | x -> fail ("invalid closure cap-scope atom: " ^ Sexp.to_string x))
            caps )
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
  | Sexp.List [ Sexp.Atom "Node"; Sexp.Str tag; Sexp.List attrs; Sexp.List children ] ->
      VNode
        ( tag,
          List.map vattr_of_canonical_sexp attrs,
          List.map view_of_canonical_sexp children )
  | x -> fail ("invalid runtime view: " ^ Sexp.to_string x)

and vattr_of_canonical_sexp = function
  | Sexp.List [ Sexp.Atom "Attr"; Sexp.Str name; Sexp.Str value ] -> VAttr (name, value)
  | Sexp.List [ Sexp.Atom "On"; Sexp.Str event; msg ] -> VOn (event, value_of_canonical_sexp msg)
  | x -> fail ("invalid runtime attribute: " ^ Sexp.to_string x)

let rec continuation_of_canonical_sexp = function
  | Sexp.Atom "KDone" -> KDone
  | Sexp.List [ Sexp.Atom "KBind"; inner; body; Sexp.List (Sexp.Atom "env" :: env) ] ->
      KBind
        ( continuation_of_canonical_sexp inner,
          Kernel.cterm_of_canonical_sexp body,
          List.map value_of_canonical_sexp env,
          [] )
  | Sexp.List
      [
        Sexp.Atom "KBind";
        inner;
        body;
        Sexp.List (Sexp.Atom "env" :: env);
        Sexp.List (Sexp.Atom "cap-scope" :: caps);
      ] ->
      KBind
        ( continuation_of_canonical_sexp inner,
          Kernel.cterm_of_canonical_sexp body,
          List.map value_of_canonical_sexp env,
          List.map
            (function
              | Sexp.Atom cap -> cap
              | x -> fail ("invalid continuation cap-scope atom: " ^ Sexp.to_string x))
            caps )
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
  | KBind (inner, body, env, cap_scope) -> (
      match apply_cont st inner response with
      | VProcessDone v ->
          eval_with_cap_scope st cap_scope (fun () -> eval_cterm st (v :: env) body)
      | VProcessRequest s -> VProcessRequest { s with cont = KBind (s.cont, body, env, cap_scope) }
      | other -> fail ("invalid resumed process value: " ^ value_to_string other))

let resume checked suspended response =
  let st = state checked in
  apply_cont st suspended.cont response
