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

let parse_one_json obj =
  let op =
    match json_string_field "op" obj with
    | "AddDef" -> AddDef
    | "ReplaceDef" -> ReplaceDef
    | "DeleteDef" -> DeleteDef
    | "RenameDef" -> RenameDef
    | s -> fail ("unknown patch op: " ^ s)
  in
  let name = json_string_field "name" obj in
  let new_name = match Json.field "newName" obj with Some v -> Json.string v | None -> None in
  let def =
    match op with
    | AddDef | ReplaceDef ->
        let typ =
          try Parser.parse_type (type_sexp_of_json (json_field "type" obj)) with
          | Parser.Error msg -> fail ("invalid patch type: " ^ msg)
        in
        let body =
          try Parser.parse_expr (expr_sexp_of_json (json_field "expr" obj)) with
          | Parser.Error msg -> fail ("invalid patch expr: " ^ msg)
        in
        Some { name; type_params = []; typ; body }
    | DeleteDef | RenameDef -> None
  in
  let capabilities =
    match Json.field "capabilities" obj with
    | None -> []
    | Some (Json.Array xs) ->
        List.map
          (function Json.String s -> s | _ -> fail "capabilities must be strings")
          xs
    | Some _ -> fail "capabilities must be an array"
  in
  let dependencies = json_string_array_field "deps" obj in
  { op; name; new_name; def; capabilities; dependencies }

let parse_json input =
  let obj =
    try Json.parse input with Json.Error msg -> fail ("invalid JSON patch: " ^ msg)
  in
  parse_one_json obj

let parse_ops_json input =
  let value =
    try Json.parse input with Json.Error msg -> fail ("invalid JSON patch: " ^ msg)
  in
  match Json.field "ops" value with
  | None -> [ parse_one_json value ]
  | Some (Json.Array ops) ->
      List.map parse_one_json ops
  | Some _ -> fail "patch ops must be an array"

let parse_file path = parse_json (read_file path)

let parse_ops_file path = parse_ops_json (read_file path)

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

let check store_root patch_path =
  let patches = parse_ops_file patch_path in
  let current : program =
    try Store.load_program store_root with
    | Store.Error msg -> fail msg
    | Parser.Error msg -> fail ("store contains invalid definition: " ^ msg)
  in
  let defs, changes =
    List.fold_left
      (fun (defs, changes) patch ->
        let before_defs = defs in
        let defs, target_name, previous_name, changed_def = merge_defs patch defs in
        let actual_deps =
          let source_defs = match patch.op with DeleteDef -> before_defs | _ -> defs in
          Kernel.dependencies_of_defs source_defs target_name |> List.sort_uniq String.compare
        in
        let declared_deps = List.sort_uniq String.compare patch.dependencies in
        if actual_deps <> declared_deps then
          fail
            ("dependency mismatch for " ^ patch.name ^ ": declared ["
            ^ String.concat ", " declared_deps ^ "], actual ["
            ^ String.concat ", " actual_deps ^ "]");
        ( defs,
          { patch; target_name; previous_name; changed_def; changed_checked_def = None }
          :: changes ))
      (current.defs, []) patches
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
    try Kernel.check_program program with Kernel.Error msg -> fail msg
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

let write_program_metadata store_root checked =
  let canonical = Kernel.serialize_checked_program checked in
  Store.write_file_atomic (Filename.concat store_root "capabilities")
    (String.concat "\n" checked.Kernel.program.capabilities ^ "\n");
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
  describe_checked checked_patch
