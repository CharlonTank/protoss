open Ast

exception Error of string

let fail msg = raise (Error msg)

let option_or_fail msg = function Some x -> x | None -> fail msg

type op = AddDef | ReplaceDef | DeleteDef | RenameDef

type t = {
  op : op;
  name : string;
  new_name : string option;
  def : def option;
  capabilities : string list;
  dependencies : string list;
}

type checked_change = {
  index : int;
  patch : t;
  target_name : string;
  previous_name : string option;
  changed_def : def option;
  changed_checked_def : Kernel.checked_def option;
}

type checked_patch = {
  patches : t list;
  program : program;
  checked : Kernel.checked;
  changes : checked_change list;
}

let read_file path =
  let ic = open_in path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () ->
      let len = in_channel_length ic in
      really_input_string ic len)

let op_to_string = function
  | AddDef -> "AddDef"
  | ReplaceDef -> "ReplaceDef"
  | DeleteDef -> "DeleteDef"
  | RenameDef -> "RenameDef"

let patch_context index patch =
  "patch op #" ^ string_of_int index ^ " " ^ op_to_string patch.op ^ " " ^ patch.name

let partial_context index ?op ?name () =
  "patch op #" ^ string_of_int index
  ^ (match op with Some op -> " " ^ op_to_string op | None -> "")
  ^ (match name with Some name -> " " ^ name | None -> "")

let fail_in context msg = fail (context ^ ": " ^ msg)

let with_context context f =
  try f () with Error msg -> fail_in context msg

let is_prefix prefix s =
  let lp = String.length prefix in
  String.length s >= lp && String.sub s 0 lp = prefix

let is_digit = function '0' .. '9' -> true | _ -> false

let split_line_col_prefix s =
  let len = String.length s in
  let rec digits i =
    if i < len && is_digit s.[i] then digits (i + 1) else i
  in
  let line_end = digits 0 in
  if line_end = 0 || line_end >= len || s.[line_end] <> ':' then None
  else
    let col_start = line_end + 1 in
    let col_end = digits col_start in
    if col_end = col_start || col_end >= len || s.[col_end] <> ':' then None
    else
      let rest_start =
        let i = col_end + 1 in
        if i < len && s.[i] = ' ' then i + 1 else i
      in
      Some (String.sub s 0 (col_end + 1), String.sub s rest_start (len - rest_start))

let has_line_col_prefix s =
  match split_line_col_prefix s with Some _ -> true | None -> false

let locate path msg =
  if has_line_col_prefix msg then path ^ ":" ^ msg else path ^ ": " ^ msg

let invalid_json_patch msg =
  match split_line_col_prefix msg with
  | Some (loc, rest) -> loc ^ " invalid JSON patch: " ^ rest
  | None -> "invalid JSON patch: " ^ msg

let take_until_colon s =
  match String.index_opt s ':' with
  | Some i -> Some (String.sub s 0 i)
  | None -> None

let extract_error_def_name msg =
  if is_prefix "definition " msg then
    let rest = String.sub msg 11 (String.length msg - 11) in
    take_until_colon rest
  else if is_prefix "duplicate definition: " msg then
    Some (String.sub msg 22 (String.length msg - 22))
  else if is_prefix "definition shadows builtin: " msg then
    Some (String.sub msg 28 (String.length msg - 28))
  else None

let json_field name obj =
  match Json.field name obj with Some v -> v | None -> fail ("patch missing field: " ^ name)

let json_string_field name obj =
  match Json.string (json_field name obj) with
  | Some s -> s
  | None -> fail ("patch field must be string: " ^ name)

let json_string_array_field name obj =
  match Json.array (json_field name obj) with
  | Some xs ->
      List.map
        (function Json.String s -> s | _ -> fail ("patch field must be string array: " ^ name))
        xs
  | None -> fail ("patch field must be array: " ^ name)

let rec type_sexp_of_json = function
  | Json.Object [ ("source", Json.String s) ] -> (
      match Kernel.single_sexp s with
      | sexp -> sexp)
  | Json.String s -> Sexp.Atom s
  | Json.Array (Json.String head :: args) ->
      Sexp.List (Sexp.Atom head :: List.map type_sexp_of_json args)
  | Json.Object fields -> (
      match fields with
      | [ ("Record", Json.Object fs) ] ->
          Sexp.List
            (Sexp.Atom "Record"
            :: List.map (fun (n, t) -> Sexp.List [ Sexp.Atom n; type_sexp_of_json t ]) fs)
      | [ ("Variant", Json.Object cs) ] ->
          Sexp.List
            (Sexp.Atom "Variant"
            :: List.map (fun (n, t) -> Sexp.List [ Sexp.Atom n; type_sexp_of_json t ]) cs)
      | _ -> fail "invalid structural type JSON")
  | _ -> fail "invalid structural type JSON"

let rec expr_sexp_of_json = function
  | Json.Object [ ("source", Json.String s) ] -> (
      match Kernel.single_sexp s with
      | sexp -> sexp)
  | Json.Num n -> Sexp.Atom (string_of_int n)
  | Json.Bool true -> Sexp.Atom "true"
  | Json.Bool false -> Sexp.Atom "false"
  | Json.Null -> Sexp.Atom "unit"
  | Json.String s -> Sexp.Atom s
  | Json.Array (Json.String head :: args) ->
      Sexp.List (Sexp.Atom head :: List.map expr_sexp_of_json args)
  | Json.Object [ ("string", Json.String s) ] -> Sexp.Str s
  | Json.Object [ ("lambda", Json.Array [ Json.String x; ty; body ]) ] ->
      Sexp.List
        [
          Sexp.Atom "lambda";
          Sexp.List [ Sexp.Atom x; type_sexp_of_json ty ];
          expr_sexp_of_json body;
        ]
  | Json.Object [ ("let", Json.Array [ Json.String x; e; body ]) ] ->
      Sexp.List
        [
          Sexp.Atom "let";
          Sexp.List [ Sexp.Atom x; expr_sexp_of_json e ];
          expr_sexp_of_json body;
        ]
  | Json.Object [ ("record", Json.Object fields) ] ->
      Sexp.List
        (Sexp.Atom "record"
        :: List.map (fun (n, e) -> Sexp.List [ Sexp.Atom n; expr_sexp_of_json e ]) fields)
  | Json.Object [ ("done", e) ] -> Sexp.List [ Sexp.Atom "done"; expr_sexp_of_json e ]
  | _ -> fail "invalid structural expr JSON"

let parse_one_json ?(index = 1) obj =
  let op =
    with_context (partial_context index ()) (fun () ->
        match json_string_field "op" obj with
        | "AddDef" -> AddDef
        | "ReplaceDef" -> ReplaceDef
        | "DeleteDef" -> DeleteDef
        | "RenameDef" -> RenameDef
        | s -> fail ("unknown patch op: " ^ s))
  in
  let name =
    with_context (partial_context index ~op ()) (fun () -> json_string_field "name" obj)
  in
  let context = partial_context index ~op ~name () in
  let new_name =
    match Json.field "newName" obj with
    | None -> None
    | Some v -> (
        match Json.string v with
        | Some s -> Some s
        | None -> fail_in (context ^ " field newName") "patch field must be string: newName")
  in
  let def =
    match op with
    | AddDef | ReplaceDef ->
        let typ =
          with_context (context ^ " field type") (fun () ->
              try Parser.parse_type (type_sexp_of_json (json_field "type" obj)) with
              | Parser.Error msg -> fail ("invalid patch type: " ^ msg))
        in
        let body =
          with_context (context ^ " field expr") (fun () ->
              try Parser.parse_expr (expr_sexp_of_json (json_field "expr" obj)) with
              | Parser.Error msg -> fail ("invalid patch expr: " ^ msg))
        in
        Some { name; type_params = []; typ; body }
    | DeleteDef | RenameDef -> None
  in
  let capabilities =
    with_context (context ^ " field capabilities") (fun () ->
        let caps =
          match Json.field "capabilities" obj with
          | None -> []
          | Some (Json.Array xs) ->
              List.map
                (function Json.String s -> s | _ -> fail "capabilities must be strings")
                xs
          | Some _ -> fail "capabilities must be an array"
        in
        (try Kernel.validate_capabilities caps with Kernel.Error msg -> fail msg);
        caps)
  in
  let dependencies =
    with_context (context ^ " field deps") (fun () -> json_string_array_field "deps" obj)
  in
  { op; name; new_name; def; capabilities; dependencies }

let parse_json input =
  let obj =
    try Json.parse input with Json.Error msg -> fail (invalid_json_patch msg)
  in
  parse_one_json obj

let parse_ops_json input =
  let value =
    try Json.parse input with Json.Error msg -> fail (invalid_json_patch msg)
  in
  match Json.field "ops" value with
  | None -> [ parse_one_json ~index:1 value ]
  | Some (Json.Array ops) ->
      ops |> List.mapi (fun i op -> parse_one_json ~index:(i + 1) op)
  | Some _ -> fail "patch ops must be an array"

let parse_file path =
  try parse_json (read_file path) with Error msg -> fail (locate path msg)

let parse_ops_file path =
  try parse_ops_json (read_file path) with Error msg -> fail (locate path msg)

let def_by_name (defs : def list) name =
  List.find_opt (fun (d : def) -> String.equal d.name name) defs

let parse_type_source s =
  try Parser.parse_type (Kernel.single_sexp s) with
  | Parser.Error msg | Kernel.Error msg -> fail ("invalid stored type: " ^ msg)

let marker_fields path =
  if not (Sys.file_exists path) then []
  else
    read_file path |> String.split_on_char '\n'
    |> List.filter_map (fun line ->
           match String.index_opt line '=' with
           | None -> None
           | Some i ->
               Some
                 ( String.sub line 0 i,
                   String.sub line (i + 1) (String.length line - i - 1) ))

let marker_field name fields =
  List.find_opt (fun (k, _) -> String.equal k name) fields |> Option.map snd

let rec contains_process_type = function
  | TProcess _ -> true
  | TFun (a, b) -> contains_process_type a || contains_process_type b
  | TRecord fields | TVariant fields -> List.exists (fun (_, t) -> contains_process_type t) fields
  | TList t | TView t -> contains_process_type t
  | TForall (_, t) -> contains_process_type t
  | TVar _ -> false
  | TNamed _ -> false
  | TUnit | TBool | TNat | TString -> false

let validate_web_patch store_root checked =
  let marker = Filename.concat store_root "web_app" in
  if Sys.file_exists marker then (
    let fields = marker_fields marker in
    let old_model =
      marker_field "model" fields
      |> Option.map parse_type_source
      |> option_or_fail "web_app marker missing model"
    in
    let contract =
      try Web.check_contract checked with Web.Error msg -> fail msg
    in
    if not (equal_typ old_model contract.Web.model_ty) then
      let migration =
        checked.Kernel.defs
        |> List.find_opt (fun (d : Kernel.checked_def) -> String.equal d.def.name "migrate_v1_v2")
      in
      match migration with
      | Some d
        when equal_typ d.def.typ (TFun (old_model, contract.model_ty))
             && not (contains_process_type d.def.typ) ->
          ()
      | Some d ->
          fail
            ("model migration has wrong type: expected "
            ^ string_of_typ (TFun (old_model, contract.model_ty))
            ^ ", got " ^ string_of_typ d.def.typ)
      | None -> fail "model shape changed without required migrate_v1_v2")

let required_def patch =
  option_or_fail "patch operation requires a definition body" patch.def

let dependents (defs : def list) name =
  defs
  |> List.filter (fun (d : def) ->
         not (String.equal d.name name)
         && List.exists (String.equal name) (Kernel.dependencies_of_defs defs d.name))
  |> List.map (fun (d : def) -> d.name)

let merge_defs (patch : t) (defs : def list) =
  let exists = def_by_name defs patch.name <> None in
  match patch.op with
  | AddDef ->
      let new_def = required_def patch in
      if exists then fail ("AddDef target already exists: " ^ patch.name);
      (defs @ [ new_def ], new_def.name, None, Some new_def)
  | ReplaceDef ->
      let new_def = required_def patch in
      if not exists then fail ("ReplaceDef target does not exist: " ^ patch.name);
      ( List.map (fun (d : def) -> if String.equal d.name patch.name then new_def else d) defs,
        new_def.name,
        None,
        Some new_def )
  | DeleteDef ->
      if not exists then fail ("DeleteDef target does not exist: " ^ patch.name);
      ( List.filter (fun (d : def) -> not (String.equal d.name patch.name)) defs,
        patch.name,
        Some patch.name,
        None )
  | RenameDef ->
      let new_name = option_or_fail "RenameDef requires newName" patch.new_name in
      let old_def = option_or_fail ("RenameDef target does not exist: " ^ patch.name) (def_by_name defs patch.name) in
      if def_by_name defs new_name <> None then fail ("RenameDef target already exists: " ^ new_name);
      let renamed = { old_def with name = new_name } in
      ( List.map (fun (d : def) -> if String.equal d.name patch.name then renamed else d) defs,
        new_name,
        Some patch.name,
        Some renamed )

let fail_for_patch patch_path index patch msg =
  fail_in (locate patch_path (patch_context index patch)) msg

let fail_kernel_with_patch_context patch_path changes msg =
  match extract_error_def_name msg with
  | Some name -> (
      match
        changes
        |> List.find_opt (fun change ->
               String.equal change.target_name name
               || Option.value ~default:"" change.previous_name = name)
      with
      | Some change -> fail_for_patch patch_path change.index change.patch msg
      | None -> fail msg)
  | None -> fail msg

let check store_root patch_path =
  let patches = parse_ops_file patch_path in
  let current : program =
    try Store.load_program store_root with
    | Store.Error msg -> fail msg
    | Parser.Error msg -> fail ("store contains invalid definition: " ^ msg)
  in
  let defs, changes, _ =
    List.fold_left
      (fun (defs, changes, index) patch ->
        let before_defs = defs in
        let defs, target_name, previous_name, changed_def =
          try merge_defs patch defs with Error msg -> fail_for_patch patch_path index patch msg
        in
        let actual_deps =
          let source_defs = match patch.op with DeleteDef -> before_defs | _ -> defs in
          Kernel.dependencies_of_defs source_defs target_name |> List.sort_uniq String.compare
        in
        let declared_deps = List.sort_uniq String.compare patch.dependencies in
        if actual_deps <> declared_deps then
          fail_for_patch patch_path index patch
            ("dependency mismatch for " ^ patch.name ^ ": declared ["
            ^ String.concat ", " declared_deps ^ "], actual ["
            ^ String.concat ", " actual_deps ^ "]");
        ( defs,
          { index; patch; target_name; previous_name; changed_def; changed_checked_def = None }
          :: changes,
          index + 1 ))
      (current.defs, [], 1) patches
  in
  let program =
    {
      imports = [];
      capabilities =
        List.sort_uniq String.compare
          (current.capabilities @ List.concat (List.map (fun p -> p.capabilities) patches));
      module_name = None;
      exports = None;
      type_aliases = current.type_aliases;
      defs;
    }
  in
  let checked =
    try Kernel.check_program program with
    | Kernel.Error msg -> fail_kernel_with_patch_context patch_path (List.rev changes) msg
  in
  let changes =
    List.rev changes
    |> List.map (fun change ->
           let changed_checked_def =
             match change.changed_def with
             | None -> None
             | Some d ->
                 checked.defs
                 |> List.find_opt (fun (cd : Kernel.checked_def) -> String.equal cd.def.name d.name)
           in
           { change with changed_checked_def })
  in
  let current_checked =
    try Kernel.check_program { current with defs = current.defs } with Kernel.Error msg -> fail msg
  in
  List.iter
    (fun change ->
      match change.patch.op with
      | RenameDef -> (
          let old_def_id =
            current_checked.defs
            |> List.find_opt (fun (d : Kernel.checked_def) -> String.equal d.def.name change.patch.name)
            |> option_or_fail "internal RenameDef checked source missing"
          in
          match change.changed_checked_def with
          | None -> ()
          | Some new_def_id ->
              if not (String.equal old_def_id.def_id new_def_id.def_id) then
                fail "RenameDef changed canonical body hash")
      | AddDef | ReplaceDef | DeleteDef -> ())
    changes;
  validate_web_patch store_root checked;
  { patches; program; checked; changes }

let describe_checked checked_patch =
  match checked_patch.changes with
  | [ { changed_checked_def = Some d; _ } ] -> d.hash
  | [ { changed_checked_def = None; _ } ] -> "no-object"
  | _ -> Kernel.hash_program checked_patch.checked

let patch_audits_dir store_root = Filename.concat store_root "patches"

let patch_latest_path store_root = Filename.concat (patch_audits_dir store_root) "latest"

let patch_audit_path store_root patch_ref =
  Filename.concat (patch_audits_dir store_root) (Store.sanitize_name patch_ref ^ ".patch")

let change_audit_line change =
  "op=" ^ string_of_int change.index ^ " kind=" ^ op_to_string change.patch.op ^ " name="
  ^ change.patch.name ^ " target=" ^ change.target_name
  ^ (match change.previous_name with Some name -> " previous=" ^ name | None -> "")
  ^
  match change.changed_checked_def with
  | Some d -> " result=" ^ d.hash
  | None -> " result=no-object"

let patch_audit_content patch_source checked_patch =
  let source_hash = Kernel.hash_string ("protoss-patch-source-v1\n" ^ patch_source) in
  let lines =
    [
      "protoss-patch-audit-v1";
      "source-hash=" ^ source_hash;
      "program-hash=" ^ Kernel.hash_program checked_patch.checked;
      "result=" ^ describe_checked checked_patch;
      "ops=" ^ string_of_int (List.length checked_patch.changes);
    ]
    @ List.map change_audit_line checked_patch.changes
    @ [ "source-bytes=" ^ string_of_int (String.length patch_source); "--source--"; patch_source ]
  in
  String.concat "\n" lines

let write_patch_audit store_root patch_path checked_patch =
  let patch_source = read_file patch_path in
  let content = patch_audit_content patch_source checked_patch in
  let patch_ref = Kernel.hash_string ("protoss-patch-audit-v1\n" ^ content) in
  let dir = patch_audits_dir store_root in
  Store.ensure_dir dir;
  Store.write_file_atomic (patch_audit_path store_root patch_ref) (content ^ "\n");
  Store.write_file_atomic (patch_latest_path store_root) (patch_ref ^ "\n");
  patch_ref

type audit = {
  audit_ref : string;
  content : string;
  source_hash : string;
  program_hash : string;
  result : string;
  ops : int;
}

let strip_one_final_newline s =
  let len = String.length s in
  if len > 0 && s.[len - 1] = '\n' then String.sub s 0 (len - 1) else s

let find_sub s needle =
  let ls = String.length s and ln = String.length needle in
  let rec loop i =
    if i + ln > ls then None
    else if String.sub s i ln = needle then Some i
    else loop (i + 1)
  in
  if ln = 0 then Some 0 else loop 0

let split_once_string needle s =
  match find_sub s needle with
  | None -> None
  | Some i ->
      Some
        ( String.sub s 0 i,
          String.sub s (i + String.length needle) (String.length s - i - String.length needle) )

let audit_field name fields =
  List.find_opt (fun (k, _) -> String.equal k name) fields |> Option.map snd

let parse_audit_fields header =
  match String.split_on_char '\n' header with
  | "protoss-patch-audit-v1" :: lines ->
      lines
      |> List.filter_map (fun line ->
             match String.index_opt line '=' with
             | None -> None
             | Some i -> Some (String.sub line 0 i, String.sub line (i + 1) (String.length line - i - 1)))
  | _ -> fail "patch audit has invalid header"

let required_audit_field fields name =
  match audit_field name fields with Some value -> value | None -> fail ("patch audit missing field: " ^ name)

let resolve_audit_ref store_root ref_arg =
  if String.equal ref_arg "latest" then
    let path = patch_latest_path store_root in
    if not (Sys.file_exists path) then fail "patch audit latest not found";
    String.trim (read_file path)
  else ref_arg

let verify_audit ?(ref = "latest") store_root =
  let audit_ref = resolve_audit_ref store_root ref in
  let path = patch_audit_path store_root audit_ref in
  if not (Sys.file_exists path) then fail ("patch audit not found: " ^ audit_ref);
  let content = read_file path |> strip_one_final_newline in
  let computed_ref = Kernel.hash_string ("protoss-patch-audit-v1\n" ^ content) in
  if not (String.equal computed_ref audit_ref) then
    fail ("patch audit hash mismatch: expected " ^ audit_ref ^ ", got " ^ computed_ref);
  let header, source =
    match split_once_string "\n--source--\n" content with
    | Some parts -> parts
    | None -> fail "patch audit missing source marker"
  in
  let fields = parse_audit_fields header in
  let source_hash = required_audit_field fields "source-hash" in
  let computed_source_hash = Kernel.hash_string ("protoss-patch-source-v1\n" ^ source) in
  if not (String.equal source_hash computed_source_hash) then
    fail ("patch audit source hash mismatch: expected " ^ source_hash ^ ", got " ^ computed_source_hash);
  let source_bytes = required_audit_field fields "source-bytes" in
  if not (String.equal source_bytes (string_of_int (String.length source))) then
    fail
      ("patch audit source size mismatch: expected " ^ source_bytes ^ ", got "
      ^ string_of_int (String.length source));
  let ops =
    try int_of_string (required_audit_field fields "ops") with Failure _ -> fail "patch audit ops is not an int"
  in
  {
    audit_ref;
    content;
    source_hash;
    program_hash = required_audit_field fields "program-hash";
    result = required_audit_field fields "result";
    ops;
  }

let current_store_program_hash store_root =
  let program =
    try Store.load_program store_root with
    | Store.Error msg -> fail msg
    | Parser.Error msg -> fail ("store contains invalid definition: " ^ msg)
  in
  let checked =
    try Kernel.check_program program with
    | Kernel.Error msg -> fail ("store program invalid: " ^ msg)
  in
  Kernel.hash_program checked

let verify_latest_matches_store store_root =
  let audit = verify_audit store_root in
  let current_hash = current_store_program_hash store_root in
  if not (String.equal audit.program_hash current_hash) then
    fail
      ("patch audit program hash mismatch: expected " ^ audit.program_hash ^ ", got "
      ^ current_hash);
  audit

let inspect_audit ?(ref = "latest") store_root =
  let audit =
    if String.equal ref "latest" then verify_latest_matches_store store_root
    else verify_audit ~ref store_root
  in
  "Patch audit OK " ^ audit.audit_ref ^ "\n" ^ audit.content ^ "\n"

let write_program_metadata store_root checked =
  let canonical = Kernel.serialize_checked_program checked in
  Store.write_file_atomic (Filename.concat store_root "capabilities")
    (String.concat "\n" checked.Kernel.program.capabilities ^ "\n");
  Store.write_type_aliases store_root checked.Kernel.program.type_aliases;
  Store.write_file_atomic (Filename.concat store_root "program.canon") (canonical ^ "\n");
  Store.write_file_atomic (Filename.concat store_root "program.graph.json")
    (Kernel.checked_to_graph_json checked)

let capability_scopes_dir store_root = Filename.concat store_root "capability-scopes"

let capability_scope_path store_root name =
  Filename.concat (capability_scopes_dir store_root) (Store.sanitize_name name ^ ".capabilities")

let delete_capability_scope store_root name =
  let path = capability_scope_path store_root name in
  if Sys.file_exists path then Sys.remove path

let write_capability_scope store_root (cd : Kernel.checked_def) =
  Store.ensure_dir (capability_scopes_dir store_root);
  Store.write_file_atomic (capability_scope_path store_root cd.def.name)
    (String.concat "\n" cd.capabilities ^ "\n")

let apply store_root patch_path =
  let checked_patch = check store_root patch_path in
  let current =
    try Store.load_program store_root with
    | Store.Error msg -> fail msg
    | Parser.Error msg -> fail ("store contains invalid definition: " ^ msg)
  in
  let final_names = List.map (fun (d : def) -> d.name) checked_patch.program.defs in
  List.iter
    (fun (d : def) ->
      if not (List.exists (String.equal d.name) final_names) then (
        Store.delete_def store_root d.name;
        delete_capability_scope store_root d.name))
    current.defs;
  List.iter
    (fun change ->
      match change.previous_name with
      | Some name when not (List.exists (String.equal name) final_names) ->
          Store.delete_def store_root name;
          delete_capability_scope store_root name
      | _ -> ())
    checked_patch.changes;
  List.iter
    (fun (cd : Kernel.checked_def) ->
      let normal, _ = Runtime.normalize_def checked_patch.checked cd.def.name in
      ignore (Store.write_def store_root cd.def cd.canonical (Runtime.value_to_string normal));
      write_capability_scope store_root cd)
    checked_patch.checked.defs;
  write_program_metadata store_root checked_patch.checked;
  (if Sys.file_exists (Filename.concat store_root "web_app") then
     let contract = Web.check_contract checked_patch.checked in
     Web.write_web_marker store_root contract);
  write_patch_audit store_root patch_path checked_patch
