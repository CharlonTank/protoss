open Ast

exception Error of string

let fail msg = raise (Error msg)

let has_prefix prefix s =
  let n = String.length prefix in
  String.length s >= n && String.sub s 0 n = prefix

let has_suffix suffix s =
  let ls = String.length s and lf = String.length suffix in
  ls >= lf && String.sub s (ls - lf) lf = suffix

let trim = String.trim

let split_once ch s =
  match String.index_opt s ch with
  | None -> None
  | Some i ->
      Some
        ( String.sub s 0 i,
          String.sub s (i + 1) (String.length s - i - 1) )

let realpath_or path =
  try Unix.realpath path with Unix.Unix_error _ -> path

let normalize_path base path =
  let raw = if Filename.is_relative path then Filename.concat base path else path in
  realpath_or raw

let path_hash path = Kernel.hash_string ("path:" ^ path)

let sanitize_id s =
  let b = Buffer.create (String.length s) in
  String.iter
    (function
      | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '.' | '-' | '_' as c -> Buffer.add_char b c
      | _ -> Buffer.add_char b '_')
    s;
  Buffer.contents b

let ensure_dir = Store.ensure_dir

let read_file path =
  try Store.read_file path with Store.Error msg -> fail msg | Sys_error msg -> fail msg

let write_file path content = Store.write_file_atomic path content

let remove_file path = if Sys.file_exists path then Sys.remove path

let rec remove_dir path =
  if Sys.file_exists path then
    if Sys.is_directory path then (
      Sys.readdir path |> Array.iter (fun f -> remove_dir (Filename.concat path f));
      Unix.rmdir path)
    else Sys.remove path

type manifest = {
  root : string;
  name : string;
  version : string;
  entrypoints : string list;
  stdlib : string option;
  source_dirs : string list;
  store_dir : string;
  cache_dir : string;
  capabilities : string list;
  package_imports : (string * string) list;
  package_interfaces : (string * string) list;
}

let manifest_name = "protoss.toml"

let manifest_path root = Filename.concat root manifest_name

let unquote s =
  let s = trim s in
  let len = String.length s in
  if len >= 2 && s.[0] = '"' && s.[len - 1] = '"' then
    String.sub s 1 (len - 2)
  else s

let parse_array s =
  let s = trim s in
  let len = String.length s in
  if len < 2 || s.[0] <> '[' || s.[len - 1] <> ']' then
    fail ("manifest array expected: " ^ s);
  let body = String.sub s 1 (len - 2) in
  let rec loop acc i =
    let rec skip j =
      if j < String.length body && (body.[j] = ' ' || body.[j] = '\t' || body.[j] = ',') then
        skip (j + 1)
      else j
    in
    let i = skip i in
    if i >= String.length body then List.rev acc
    else if body.[i] <> '"' then fail ("manifest string array expected: " ^ s)
    else
      let b = Buffer.create 16 in
      let rec string j =
        if j >= String.length body then fail ("unterminated manifest string: " ^ s)
        else
          match body.[j] with
          | '"' -> (Buffer.contents b, j + 1)
          | '\\' when j + 1 < String.length body ->
              Buffer.add_char b body.[j + 1];
              string (j + 2)
          | c ->
              Buffer.add_char b c;
              string (j + 1)
      in
      let item, j = string (i + 1) in
      loop (item :: acc) j
  in
  loop [] 0

let parse_manifest_pair kind s =
  match split_once '=' s with
  | Some (name, hash) when trim name <> "" && trim hash <> "" -> (trim name, trim hash)
  | _ -> fail (kind ^ " expected name=value: " ^ s)

let parse_package_import s = parse_manifest_pair "package import" s

let parse_package_interface s = parse_manifest_pair "package interface constraint" s

let parse_manifest root =
  let root = realpath_or root in
  let path = manifest_path root in
  if not (Sys.file_exists path) then fail ("missing protoss.toml: " ^ path);
  let fields =
    read_file path |> String.split_on_char '\n'
    |> List.fold_left
         (fun acc line ->
           let line =
             match String.index_opt line '#' with
             | Some i -> String.sub line 0 i
             | None -> line
             |> trim
           in
           if line = "" then acc
           else
             match split_once '=' line with
             | Some (k, v) -> (trim k, trim v) :: acc
             | None -> fail ("invalid manifest line: " ^ line))
         []
  in
  let field name =
    List.find_opt (fun (k, _) -> String.equal k name) fields |> Option.map snd
  in
  let string_field name default =
    match field name with Some v -> unquote v | None -> default
  in
  let array_field name default =
    match field name with Some v -> parse_array v | None -> default
  in
  {
    root;
    name = string_field "name" (Filename.basename root);
    version = string_field "version" "0.1.0";
    entrypoints = array_field "entrypoints" [ "src/main.protoss" ];
    stdlib =
      (match field "stdlib" with
      | None -> None
      | Some v ->
          let v = unquote v in
          if v = "" || v = "none" then None else Some v);
    source_dirs = array_field "source_dirs" [ "src" ];
    store_dir = string_field "store_dir" ".protoss/store";
    cache_dir = string_field "cache_dir" ".protoss/cache";
    capabilities = array_field "capabilities" [];
    package_imports = array_field "package_imports" [] |> List.map parse_package_import;
    package_interfaces =
      array_field "package_interfaces" [] |> List.map parse_package_interface;
  }

let path_in_project manifest path = normalize_path manifest.root path

let store_root manifest = path_in_project manifest manifest.store_dir

let cache_root manifest = path_in_project manifest manifest.cache_dir

let project_root path =
  let path = if path = "" then "." else path in
  let path = realpath_or path in
  if Sys.file_exists path && not (Sys.is_directory path) then Filename.dirname path else path

let init ?(force = false) root =
  let root = project_root root in
  ensure_dir root;
  let src = Filename.concat root "src" in
  ensure_dir src;
  let dot = Filename.concat root ".protoss" in
  ensure_dir dot;
  ensure_dir (Filename.concat dot "store");
  ensure_dir (Filename.concat dot "cache");
  let manifest = manifest_path root in
  if Sys.file_exists manifest && not force then fail ("manifest already exists: " ^ manifest);
  write_file manifest
    "name = \"protoss-app\"\n\
     version = \"0.1.0\"\n\
     entrypoints = [\"src/main.protoss\"]\n\
     stdlib = \"none\"\n\
     source_dirs = [\"src\"]\n\
     store_dir = \".protoss/store\"\n\
     cache_dir = \".protoss/cache\"\n\
     capabilities = []\n\
     package_imports = []\n\
     package_interfaces = []\n";
  let main = Filename.concat src "main.protoss" in
  if not (Sys.file_exists main) then write_file main "(def main Nat 0)\n";
  manifest

let rec collect_protoss_files dir =
  if not (Sys.file_exists dir) then []
  else if Sys.is_directory dir then
    Sys.readdir dir |> Array.to_list |> List.sort String.compare
    |> List.concat_map (fun f -> collect_protoss_files (Filename.concat dir f))
  else if has_suffix ".protoss" dir then [ realpath_or dir ]
  else []

let sort_uniq xs = xs |> List.sort_uniq String.compare

let split_lines s =
  s |> String.split_on_char '\n' |> List.map trim |> List.filter (fun line -> line <> "")

let unit_dir store = Filename.concat store "units"

let unit_defs_dir store = Filename.concat store "unit-defs"

let types_dir store = Filename.concat store "types"

let deps_dir store = Filename.concat store "deps"

let normal_dir store = Filename.concat store "normal"

let defids_dir store = Filename.concat store "defids"

let builds_dir store = Filename.concat store "builds"

let meta_dir store = Filename.concat store "meta"

let project_store_dirs store =
  Store.ensure_store store;
  List.iter ensure_dir
    [
      unit_dir store;
      unit_defs_dir store;
      types_dir store;
      deps_dir store;
      normal_dir store;
      defids_dir store;
      builds_dir store;
      meta_dir store;
    ]

let unit_key path =
  path_hash path |> sanitize_id

let unit_meta_path store path = Filename.concat (unit_dir store) (unit_key path ^ ".unit")

let unit_defs_path store path = Filename.concat (unit_defs_dir store) (unit_key path ^ ".protoss")

let meta_assoc path =
  if not (Sys.file_exists path) then []
  else
    read_file path |> String.split_on_char '\n'
    |> List.filter_map (fun line ->
           match split_once '=' line with
           | Some (k, v) -> Some (k, v)
           | None -> None)

let meta_field name fields =
  List.find_opt (fun (k, _) -> String.equal k name) fields |> Option.map snd

let split_words s =
  if trim s = "" then []
  else String.split_on_char ' ' s |> List.filter (fun x -> x <> "")

type unit_load = {
  path : string;
  source_hash : string;
  imports : string list;
  capabilities : string list;
  module_name : string option;
  exports : string list option;
  type_aliases : type_alias list;
  defs : def list;
  public_symbols : string list;
  all_symbols : string list;
  parsed_from_source : bool;
}

type build_stats = {
  mutable parsed : int;
  mutable reused : int;
  mutable typechecked : int;
  mutable normalized : int;
  mutable cache_hits : int;
}

let empty_stats () = { parsed = 0; reused = 0; typechecked = 0; normalized = 0; cache_hits = 0 }

let source_hash source = Kernel.hash_string ("source:" ^ source)

let local_symbols_of_program (program : program) =
  List.map (fun (d : def) -> d.name) program.defs
  @ List.map (fun (a : type_alias) -> a.type_name) program.type_aliases

let public_symbols_of_program (program : program) =
  match program.exports with Some names -> names | None -> local_symbols_of_program program

let rec type_refs = function
  | TUnit | TBool | TNat | TString -> []
  | TFun (a, b) -> type_refs a @ type_refs b
  | TRecord fields | TVariant fields -> List.concat_map (fun (_, t) -> type_refs t) fields
  | TList t | TView t | TProcess t -> type_refs t
  | TVar _ -> []
  | TForall (_, t) -> type_refs t
  | TNamed (n, args) -> n :: List.concat_map type_refs args

let rec expr_type_refs = function
  | EUnit | EBool _ | ENat _ | EString _ | EName _ | ERequest _ | ENilInfer -> []
  | ELambda (_, t, body) -> type_refs t @ expr_type_refs body
  | ELambdaInfer (_, body) -> expr_type_refs body
  | EApp (f, x) -> expr_type_refs f @ expr_type_refs x
  | ELet (_, e, body) -> expr_type_refs e @ expr_type_refs body
  | ELetAnnot (_, t, e, body) -> type_refs t @ expr_type_refs e @ expr_type_refs body
  | ELetRecord (record, _, body) -> expr_type_refs record @ expr_type_refs body
  | ERecord fields -> List.concat_map (fun (_, e) -> expr_type_refs e) fields
  | EField (e, _) -> expr_type_refs e
  | EVariant (t, _, e) -> type_refs t @ expr_type_refs e
  | EVariantInferred (_, e) -> expr_type_refs e
  | EInst (_, args) -> List.concat_map type_refs args
  | ECase (e, branches) -> expr_type_refs e @ List.concat_map branch_type_refs branches
  | EFoldNat (n, z, step) -> expr_type_refs n @ expr_type_refs z @ expr_type_refs step
  | EFoldVariant (target, result, scrut, branches) ->
      type_refs target @ type_refs result @ expr_type_refs scrut
      @ List.concat_map branch_type_refs branches
  | ERecur e -> expr_type_refs e
  | ENil t -> type_refs t
  | ECons (t, head, tail) -> type_refs t @ expr_type_refs head @ expr_type_refs tail
  | EConsInfer (head, tail) -> expr_type_refs head @ expr_type_refs tail
  | EFoldList (xs, z, step) -> expr_type_refs xs @ expr_type_refs z @ expr_type_refs step
  | ECaseList (xs, nil_body, _, _, cons_body) ->
      expr_type_refs xs @ expr_type_refs nil_body @ expr_type_refs cons_body
  | EText e | EColumn e | ERow e | EDone e -> expr_type_refs e
  | EImage (a, b) | EButton (a, b) | EInput (a, b) | EListView (a, b)
  | EWhenView (a, b) ->
      expr_type_refs a @ expr_type_refs b
  | EBind (p, _, t, body) -> expr_type_refs p @ type_refs t @ expr_type_refs body
  | EBindInfer (p, _, body) -> expr_type_refs p @ expr_type_refs body

and branch_type_refs = function
  | BBool (_, e) -> expr_type_refs e
  | BVariant (_, _, e) -> expr_type_refs e
  | BVariantUnit (_, e) -> expr_type_refs e

let unit_type_refs (unit : unit_load) =
  let alias_refs =
    unit.type_aliases
    |> List.concat_map (fun a ->
           type_refs a.type_body
           |> List.filter (fun n -> not (List.exists (String.equal n) a.type_params)))
  in
  let def_refs =
    unit.defs
    |> List.concat_map (fun (d : def) ->
           type_refs d.typ @ expr_type_refs d.body
           |> List.filter (fun n -> not (List.exists (String.equal n) d.type_params)))
  in
  sort_uniq (alias_refs @ def_refs)

let parse_cached_unit store path hash =
  let fields = meta_assoc (unit_meta_path store path) in
  match (meta_field "source_hash" fields, Sys.file_exists (unit_defs_path store path)) with
  | Some old_hash, true when String.equal old_hash hash ->
      let parsed = Parser.parse_string (read_file (unit_defs_path store path)) in
      let local_symbols = local_symbols_of_program parsed in
      Some
        {
          path;
          source_hash = hash;
          imports = meta_field "imports" fields |> Option.value ~default:"" |> split_words;
          capabilities = meta_field "capabilities" fields |> Option.value ~default:"" |> split_words;
          module_name = meta_field "module" fields;
          exports =
            (match meta_field "exports" fields with
            | None -> parsed.exports
            | Some s -> Some (split_words s));
          type_aliases = parsed.type_aliases;
          defs = parsed.defs;
          public_symbols =
            meta_field "public_symbols" fields |> Option.map split_words
            |> Option.value ~default:(public_symbols_of_program parsed);
          all_symbols =
            meta_field "all_symbols" fields |> Option.map split_words
            |> Option.value ~default:local_symbols;
          parsed_from_source = false;
        }
  | _ -> None

let parse_source_unit path source hash =
  let parsed = Parser.parse_string source in
  {
    path;
    source_hash = hash;
    imports = parsed.imports;
    capabilities = parsed.capabilities;
    module_name = parsed.module_name;
    exports = parsed.exports;
    type_aliases = parsed.type_aliases;
    defs = parsed.defs;
    public_symbols = public_symbols_of_program parsed;
    all_symbols = local_symbols_of_program parsed;
    parsed_from_source = true;
  }

let write_unit store unit =
  let forms = List.map string_of_type_alias unit.type_aliases @ List.map string_of_def unit.defs in
  let defs_source = String.concat "\n" forms ^ if forms = [] then "" else "\n" in
  write_file (unit_defs_path store unit.path) defs_source;
	  write_file (unit_meta_path store unit.path)
	    ("path=" ^ unit.path ^ "\nsource_hash=" ^ unit.source_hash ^ "\nimports="
	   ^ String.concat " " unit.imports ^ "\ncapabilities="
	    ^ String.concat " " unit.capabilities ^ "\nmodule="
	    ^ Option.value unit.module_name ~default:"" ^ "\nexports="
	    ^ Option.value (Option.map (String.concat " ") unit.exports) ~default:"" ^ "\npublic_symbols="
	    ^ String.concat " " unit.public_symbols ^ "\nall_symbols="
	    ^ String.concat " " unit.all_symbols ^ "\ndefs="
	   ^ String.concat " " (List.map (fun (d : def) -> d.name) unit.defs)
	   ^ "\n")

let manifest_roots manifest =
  let stdlib =
    match manifest.stdlib with
    | None -> []
    | Some path -> [ path_in_project manifest path ]
  in
  let entrypoints = List.map (path_in_project manifest) manifest.entrypoints in
  let source_files =
    manifest.source_dirs
    |> List.map (path_in_project manifest)
    |> List.concat_map collect_protoss_files
  in
  sort_uniq (stdlib @ entrypoints @ source_files)

let load_units manifest store stats =
  project_store_dirs store;
  let seen = Hashtbl.create 32 in
  let units = ref [] in
  let rec load stack path =
    let path = realpath_or path in
    if List.exists (String.equal path) stack then
      fail ("import cycle: " ^ String.concat " -> " (List.rev (path :: stack)));
    if not (Hashtbl.mem seen path) then (
      Hashtbl.add seen path ();
      let source = read_file path in
      let hash = source_hash source in
      let unit =
        match parse_cached_unit store path hash with
        | Some unit ->
            stats.reused <- stats.reused + 1;
            unit
        | None ->
            let unit =
              try parse_source_unit path source hash with
              | Parser.Error msg -> fail (path ^ ":1:1: " ^ msg)
            in
            stats.parsed <- stats.parsed + 1;
            unit
      in
      units := unit :: !units;
      let base = Filename.dirname path in
      List.iter
        (fun import_path ->
          let target = normalize_path base import_path in
          if Sys.file_exists target then load (path :: stack) target
          else fail (path ^ ":1:1: missing import: " ^ import_path))
        unit.imports)
  in
	  List.iter (load []) (manifest_roots manifest);
	  !units |> List.sort (fun a b -> String.compare a.path b.path)

let unit_by_path (units : unit_load list) path =
  let path = realpath_or path in
  List.find_opt (fun u -> String.equal u.path path) units

let direct_import_units (units : unit_load list) (unit : unit_load) =
  unit.imports
  |> List.filter_map (fun import_path ->
         let target = normalize_path (Filename.dirname unit.path) import_path in
         unit_by_path units target)

let rec reachable_symbols (units : unit_load list) seen (unit : unit_load) =
  if List.exists (String.equal unit.path) seen then []
  else
    let seen = unit.path :: seen in
    unit.all_symbols
    @ (direct_import_units units unit
      |> List.concat_map (reachable_symbols units seen))

let manifest_stdlib_unit (manifest : manifest) (units : unit_load list) =
  match manifest.stdlib with
  | None -> None
  | Some path -> unit_by_path units (path_in_project manifest path)

let check_unit_access (manifest : manifest) (units : unit_load list) =
  let all_defs = units |> List.concat_map (fun (u : unit_load) -> u.defs) in
  let project_symbols =
    units |> List.concat_map (fun (u : unit_load) -> u.all_symbols) |> sort_uniq
  in
  let stdlib_public, stdlib_all =
    match manifest_stdlib_unit manifest units with
    | None -> ([], [])
    | Some u -> (u.public_symbols, reachable_symbols units [] u |> sort_uniq)
  in
  List.iter
    (fun (unit : unit_load) ->
      let direct_imports = direct_import_units units unit in
      let import_public =
        direct_imports |> List.concat_map (fun u -> u.public_symbols) |> sort_uniq
      in
      let import_all =
        direct_imports |> List.concat_map (fun u -> reachable_symbols units [] u) |> sort_uniq
      in
      let allowed = sort_uniq (unit.all_symbols @ import_public @ stdlib_public) in
      let known = sort_uniq (project_symbols @ import_all @ stdlib_all) in
      let check_ref kind name =
        if List.exists (String.equal name) allowed then ()
        else if List.exists (String.equal name) known then
          fail
            (unit.path ^ ":1:1: " ^ kind
           ^ " is not imported or exported for this unit: " ^ name)
      in
      List.iter
        (fun (d : def) ->
          Kernel.dependencies_of_defs all_defs d.name |> List.iter (check_ref "definition"))
        unit.defs;
      unit_type_refs unit |> List.iter (check_ref "type"))
    units

let checked_def_by_name checked name =
  checked.Kernel.defs
  |> List.find_opt (fun (d : Kernel.checked_def) -> String.equal d.def.name name)

let read_trim path = if Sys.file_exists path then Some (trim (read_file path)) else None

let normal_path store name = Filename.concat (normal_dir store) (Store.sanitize_name name ^ ".nf")

let type_path store name = Filename.concat (types_dir store) (Store.sanitize_name name ^ ".type")

let deps_path store name = Filename.concat (deps_dir store) (Store.sanitize_name name ^ ".deps")

let defid_path store name = Filename.concat (defids_dir store) (Store.sanitize_name name ^ ".defid")

let capability_scopes_dir store = Filename.concat store "capability-scopes"

let capability_scope_path store name =
  Filename.concat (capability_scopes_dir store) (Store.sanitize_name name ^ ".capabilities")

let write_project_def store cache_dir checked stats program_hash cd =
  let name = cd.Kernel.def.name in
  let deps =
    Kernel.dependencies_of_defs checked.Kernel.program.defs name |> List.sort_uniq String.compare
  in
  let old_defid = read_trim (defid_path store name) in
  let old_normal = read_trim (normal_path store name) in
  let normal =
    match (old_defid, old_normal) with
    | Some old, Some nf when String.equal old cd.def_id ->
        stats.cache_hits <- stats.cache_hits + 1;
        nf
    | _ ->
        let value, trace =
          Runtime.normalize_def ~trace_cache:true ~cache_dir ~cache_scope:program_hash checked name
        in
        stats.normalized <- stats.normalized + 1;
        stats.cache_hits <-
          stats.cache_hits
          + List.fold_left
              (fun acc line -> if has_prefix "cache hit" line then acc + 1 else acc)
              0 trace;
        Runtime.value_to_string value
  in
  ignore (Store.write_def store cd.def cd.canonical normal);
  write_file (type_path store name) (string_of_typ cd.def.typ ^ "\n");
  write_file (deps_path store name) (String.concat "\n" deps ^ "\n");
  Store.ensure_dir (capability_scopes_dir store);
  write_file (capability_scope_path store name) (String.concat "\n" cd.capabilities ^ "\n");
  write_file (normal_path store name) (normal ^ "\n");
  write_file (defid_path store name) (cd.def_id ^ "\n")

let cleanup_removed_defs store final_names =
  let existing =
    try (Store.load_program store).defs with _ -> []
  in
  List.iter
    (fun (d : def) ->
      if not (List.exists (String.equal d.name) final_names) then (
        Store.delete_def store d.name;
        remove_file (type_path store d.name);
        remove_file (deps_path store d.name);
        remove_file (normal_path store d.name);
        remove_file (defid_path store d.name)))
    existing

type build_result = {
  manifest : manifest;
  checked : Kernel.checked;
  stats : build_stats;
  build_id : string;
  store : string;
}

type package_result = {
  package_ref : string;
  package_path : string;
  lock_hash : string;
  build_id : string;
  store : string;
}

type prepared_build = {
  units : unit_load list;
  checked : Kernel.checked;
  stats : build_stats;
  build_id : string;
  program_canonical : string;
  program_graph : string;
}

let prepare_build manifest =
  let stats = empty_stats () in
  let store = store_root manifest in
  let units = load_units manifest store stats in
  check_unit_access manifest units;
  let program =
    {
      imports = [];
      capabilities =
        List.sort_uniq String.compare
          (manifest.capabilities @ List.concat (List.map (fun u -> u.capabilities) units));
      module_name = None;
      exports = None;
      type_aliases = List.concat (List.map (fun u -> u.type_aliases) units);
      defs = List.concat (List.map (fun u -> u.defs) units);
    }
  in
  stats.typechecked <-
    List.fold_left
      (fun acc u -> if u.parsed_from_source then acc + List.length u.defs else acc)
      0 units;
  let checked =
    try Kernel.check_program program with Kernel.Error msg -> fail msg
  in
  let program_canonical = Kernel.serialize_checked_program checked in
  let program_graph = Kernel.checked_to_graph_json checked in
  let build_id = Kernel.hash_string program_canonical in
  { units; checked; stats; build_id; program_canonical; program_graph }

let build ?(write = true) ?lock_hash manifest =
  let prepared = prepare_build manifest in
  let store = store_root manifest in
  if write then (
    project_store_dirs store;
    List.iter (write_unit store) prepared.units;
    write_file (Filename.concat store "capabilities")
      (String.concat "\n" prepared.checked.program.capabilities ^ "\n");
    Store.write_type_aliases store prepared.checked.program.type_aliases;
    write_file (Filename.concat store "program.canon") (prepared.program_canonical ^ "\n");
    write_file (Filename.concat store "program.graph.json") prepared.program_graph;
    cleanup_removed_defs store (List.map (fun d -> d.Kernel.def.name) prepared.checked.defs);
    List.iter
      (write_project_def store (cache_root manifest) prepared.checked prepared.stats prepared.build_id)
      prepared.checked.defs;
    write_file (Filename.concat (builds_dir store) (sanitize_id prepared.build_id ^ ".build"))
      ("id=" ^ prepared.build_id ^ "\npackage=" ^ manifest.name ^ "\nversion="
     ^ manifest.version ^ "\nprogram_hash=" ^ prepared.build_id ^ "\ndefs="
      ^ String.concat " " (List.map (fun d -> d.Kernel.def.name) prepared.checked.defs)
      ^ "\nlock_hash=" ^ Option.value lock_hash ~default:""
      ^ "\n");
    write_file (Filename.concat store "current") (prepared.build_id ^ "\n");
    write_file (Filename.concat store "roots")
      ("package=" ^ manifest.name ^ "\nversion=" ^ manifest.version ^ "\nentrypoints="
     ^ String.concat " " manifest.entrypoints ^ "\nroots="
      ^ String.concat " "
          (prepared.units
          |> List.filter (fun u ->
                 List.exists
                   (fun entry -> String.equal u.path (path_in_project manifest entry))
                   manifest.entrypoints)
          |> List.concat_map (fun u -> List.map (fun (d : def) -> d.name) u.defs))
      ^ "\n");
    write_file (Filename.concat store "world_refs") "");
  { manifest; checked = prepared.checked; stats = prepared.stats; build_id = prepared.build_id; store }

let lock_path manifest = Filename.concat (Filename.concat manifest.root ".protoss") "lock"

let package_current_path manifest = Filename.concat (Filename.concat manifest.root ".protoss") "package"

let packages_dir manifest = Filename.concat (Filename.concat manifest.root ".protoss") "packages"

let package_path_for_ref manifest package_ref =
  Filename.concat (packages_dir manifest) (sanitize_id package_ref ^ ".package")

let package_items content =
  match Sexp.parse content with
  | [ Sexp.List (Sexp.Atom "protoss-package-v1" :: items) ] -> items
  | [ form ] -> fail ("invalid package descriptor: " ^ Sexp.to_string form)
  | [] -> fail "empty package descriptor"
  | _ -> fail "package descriptor must contain one form"

let package_atom_field name items =
  match
    List.find_opt
      (function Sexp.List (Sexp.Atom n :: _) when String.equal n name -> true | _ -> false)
      items
  with
  | Some (Sexp.List [ Sexp.Atom _; Sexp.Atom value ]) -> value
  | Some item -> fail ("invalid package field " ^ name ^ ": " ^ Sexp.to_string item)
  | None -> fail ("package missing field: " ^ name)

let package_string_field name items =
  match
    List.find_opt
      (function Sexp.List (Sexp.Atom n :: _) when String.equal n name -> true | _ -> false)
      items
  with
  | Some (Sexp.List [ Sexp.Atom _; Sexp.Str value ]) -> value
  | Some item -> fail ("invalid package field " ^ name ^ ": " ^ Sexp.to_string item)
  | None -> fail ("package missing field: " ^ name)

let relative_to_root manifest path =
  let root = realpath_or manifest.root in
  let path = realpath_or path in
  let root_prefix = if has_suffix "/" root then root else root ^ "/" in
  if has_prefix root_prefix path then
    String.sub path (String.length root_prefix) (String.length path - String.length root_prefix)
  else path

let lock_item name values =
  "(" ^ name
  ^ (match values with [] -> "" | _ -> " " ^ String.concat " " values)
  ^ ")"

let lock_string s = Ast.quote s

let lock_string_list name values =
  lock_item name (List.map lock_string (List.sort String.compare values))

let lock_pair_list name values =
  values
  |> List.map (fun (k, v) -> k ^ "=" ^ v)
  |> lock_string_list name

let package_type_item (alias : type_alias) =
  lock_item "type"
    [
      lock_item "name" [ lock_string alias.type_name ];
      lock_string_list "params" alias.type_params;
      lock_item "hash" [ Kernel.hash_string (string_of_type_alias alias) ];
    ]

let package_def_item (d : Kernel.checked_def) =
  lock_item "def"
    [
      lock_item "name" [ lock_string d.def.name ];
      lock_item "def-id" [ d.def_id ];
      lock_item "hash" [ Kernel.hash_string d.canonical ];
      lock_item "type-hash" [ Kernel.hash_string (Kernel.type_to_canonical d.def.typ) ];
      lock_string_list "capability-scope" d.capabilities;
    ]

let package_public_symbols manifest prepared =
  let stdlib_path = Option.map (path_in_project manifest) manifest.stdlib in
  prepared.units
  |> List.filter (fun u ->
         match stdlib_path with Some path -> not (String.equal u.path path) | None -> true)
  |> List.concat_map (fun u -> u.public_symbols)
  |> sort_uniq

let package_interface_item checked symbol =
  match checked_def_by_name checked symbol with
  | Some d ->
      lock_item "def"
        [
          lock_item "name" [ lock_string d.def.name ];
          lock_item "type-hash" [ Kernel.hash_string (Kernel.type_to_canonical d.def.typ) ];
          lock_string_list "capability-scope" d.capabilities;
        ]
  | None -> (
      match
        List.find_opt
          (fun (a : type_alias) -> String.equal a.type_name symbol)
          checked.Kernel.program.type_aliases
      with
      | Some alias ->
          lock_item "type"
            [
              lock_item "name" [ lock_string alias.type_name ];
              lock_string_list "params" alias.type_params;
              lock_item "type-hash" [ Kernel.hash_string (Kernel.type_to_canonical alias.type_body) ];
            ]
      | None -> fail ("package public symbol is not a definition or type: " ^ symbol))

let package_interface_items manifest prepared =
  package_public_symbols manifest prepared
  |> List.map (package_interface_item prepared.checked)

let package_interface_hash manifest prepared =
  Kernel.hash_string (lock_item "interface" (package_interface_items manifest prepared))

type package_import_info = {
  import_name : string;
  import_ref : string;
  import_lock_hash : string;
  import_interface_hash : string;
}

let package_import_manifest manifest (name, path) =
  let root = project_root (normalize_path manifest.root path) in
  let imported = parse_manifest root in
  if not (String.equal imported.name name) then
    fail
      ("package import name mismatch: expected " ^ name ^ ", got " ^ imported.name ^ " at "
     ^ root);
  imported

let read_package_import manifest import =
  let imported = package_import_manifest manifest import in
  let pointer = package_current_path imported in
  if not (Sys.file_exists pointer) then fail ("missing imported package pointer: " ^ pointer);
  let package_ref = trim (read_file pointer) in
  if String.equal package_ref "" then fail ("empty imported package pointer: " ^ pointer);
  let package_path = package_path_for_ref imported package_ref in
  if not (Sys.file_exists package_path) then
    fail ("missing imported package descriptor: " ^ package_path);
  let content = read_file package_path in
  let actual_ref = Kernel.hash_string content in
  if not (String.equal actual_ref package_ref) then
    fail
      ("imported package hash mismatch for " ^ imported.name ^ ": pointer " ^ package_ref
     ^ ", content " ^ actual_ref);
  let items = package_items content in
  let imported_lock_path = lock_path imported in
  if not (Sys.file_exists imported_lock_path) then
    fail ("missing imported lockfile: " ^ imported_lock_path);
  let current_lock_hash = Kernel.hash_string (read_file imported_lock_path) in
  let prepared = prepare_build imported in
  let current_interface_hash = package_interface_hash imported prepared in
  let expect_string field expected =
    let actual = package_string_field field items in
    if not (String.equal actual expected) then
      fail
        ("imported package " ^ imported.name ^ " " ^ field ^ " mismatch: expected "
       ^ expected ^ ", got " ^ actual)
  in
  let expect_atom field expected =
    let actual = package_atom_field field items in
    if not (String.equal actual expected) then
      fail
        ("imported package " ^ imported.name ^ " " ^ field ^ " mismatch: expected "
       ^ expected ^ ", got " ^ actual)
  in
  expect_string "package" imported.name;
  expect_string "version" imported.version;
  expect_string "canonical-version" Kernel.canonical_version;
  expect_string "canonical-graph-version" Kernel.canonical_graph_version;
  expect_string "canonical-node-graph-version" Kernel.canonical_node_graph_version;
  expect_string "hash-algorithm" Kernel.hash_algorithm;
  expect_string "hash-prefix" Kernel.hash_prefix;
  expect_atom "lock-hash" current_lock_hash;
  expect_atom "program-hash" prepared.build_id;
  expect_atom "program-canonical-hash" (Kernel.hash_string prepared.program_canonical);
  expect_atom "program-graph-hash" (Kernel.hash_string prepared.program_graph);
  expect_atom "interface-hash" current_interface_hash;
  {
    import_name = imported.name;
    import_ref = package_ref;
    import_lock_hash = current_lock_hash;
    import_interface_hash = current_interface_hash;
  }

let package_imports manifest =
  manifest.package_imports
  |> List.map (read_package_import manifest)
  |> List.sort (fun a b -> String.compare a.import_name b.import_name)

let unit_lock manifest (unit : unit_load) =
  let rel_import import =
    normalize_path (Filename.dirname unit.path) import |> relative_to_root manifest
  in
  lock_item "unit"
    [
      lock_item "path" [ lock_string (relative_to_root manifest unit.path) ];
      lock_item "source-hash" [ unit.source_hash ];
      lock_string_list "imports" (List.map rel_import unit.imports);
      lock_string_list "capabilities" unit.capabilities;
      lock_string_list "public-symbols" unit.public_symbols;
      lock_string_list "all-symbols" unit.all_symbols;
    ]

let lock_content manifest prepared =
  let units = List.sort (fun a b -> String.compare a.path b.path) prepared.units in
  let imports = package_imports manifest in
  String.concat "\n"
    [
      "(protoss-lock-v1";
      "  " ^ lock_item "package" [ lock_string manifest.name ];
      "  " ^ lock_item "version" [ lock_string manifest.version ];
      "  " ^ lock_item "canonical-version" [ lock_string Kernel.canonical_version ];
      "  " ^ lock_item "canonical-graph-version" [ lock_string Kernel.canonical_graph_version ];
      "  " ^ lock_item "hash-algorithm" [ lock_string Kernel.hash_algorithm ];
      "  " ^ lock_item "hash-prefix" [ lock_string Kernel.hash_prefix ];
      "  " ^ lock_item "program-hash" [ prepared.build_id ];
      "  " ^ lock_item "program-canonical-hash" [ Kernel.hash_string prepared.program_canonical ];
      "  " ^ lock_item "program-graph-hash" [ Kernel.hash_string prepared.program_graph ];
      "  " ^ lock_string_list "entrypoints" manifest.entrypoints;
      "  " ^ lock_string_list "source-dirs" manifest.source_dirs;
      "  " ^ lock_string_list "capabilities" prepared.checked.program.capabilities;
      "  " ^ lock_pair_list "package-imports" (List.map (fun i -> (i.import_name, i.import_ref)) imports);
      "  "
      ^ lock_pair_list "package-import-locks"
          (List.map (fun i -> (i.import_name, i.import_lock_hash)) imports);
      "  "
      ^ lock_pair_list "package-import-interfaces"
          (List.map (fun i -> (i.import_name, i.import_interface_hash)) imports);
      "  " ^ lock_pair_list "package-interfaces" manifest.package_interfaces;
      "  "
      ^ lock_item "defs"
          (prepared.checked.defs
          |> List.map (fun (d : Kernel.checked_def) ->
                 lock_item "def"
                   [
                     lock_item "name" [ lock_string d.def.name ];
                     lock_item "def-id" [ d.def_id ];
                     lock_item "hash" [ Kernel.hash_string d.canonical ];
                   ]));
      "  " ^ lock_item "units" (List.map (unit_lock manifest) units);
      ")";
      "";
    ]

let write_lock manifest =
  let prepared = prepare_build manifest in
  let path = lock_path manifest in
  ensure_dir (Filename.dirname path);
  let content = lock_content manifest prepared in
  write_file path content;
  (path, Kernel.hash_string content)

let write_lock_prepared manifest prepared =
  let path = lock_path manifest in
  ensure_dir (Filename.dirname path);
  let content = lock_content manifest prepared in
  write_file path content;
  (path, Kernel.hash_string content)

let check_lock manifest =
  let path = lock_path manifest in
  if not (Sys.file_exists path) then fail ("missing lockfile: " ^ path);
  let expected = lock_content manifest (prepare_build manifest) in
  let actual = read_file path in
  if not (String.equal actual expected) then fail ("lockfile out of date: " ^ path);
  Kernel.hash_string expected

let check_lock_prepared manifest prepared =
  let path = lock_path manifest in
  if not (Sys.file_exists path) then fail ("missing lockfile: " ^ path);
  let expected = lock_content manifest prepared in
  let actual = read_file path in
  if not (String.equal actual expected) then fail ("lockfile out of date: " ^ path);
  Kernel.hash_string expected

let build_locked ?(write = true) manifest =
  let lock_hash = check_lock manifest in
  build ~write ~lock_hash manifest

let check_project manifest =
  ignore (build ~write:false manifest)

let validate_package_interface_constraints manifest interface_hash =
  let available =
    (manifest.name, interface_hash)
    :: (package_imports manifest |> List.map (fun i -> (i.import_name, i.import_interface_hash)))
  in
  List.iter
    (fun (name, expected) ->
      match List.find_opt (fun (n, _) -> String.equal n name) available with
      | Some (_, actual) ->
          if not (String.equal expected actual) then
            fail
              ("package interface mismatch for " ^ name ^ ": expected " ^ expected ^ ", got "
             ^ actual)
      | None -> fail ("unknown package interface constraint: " ^ name))
    manifest.package_interfaces

let package_content manifest prepared lock_hash =
  let units = List.sort (fun a b -> String.compare a.path b.path) prepared.units in
  let interface_items = package_interface_items manifest prepared in
  let imports = package_imports manifest in
  String.concat "\n"
    [
      "(protoss-package-v1";
      "  " ^ lock_item "package" [ lock_string manifest.name ];
      "  " ^ lock_item "version" [ lock_string manifest.version ];
      "  " ^ lock_item "canonical-version" [ lock_string Kernel.canonical_version ];
      "  " ^ lock_item "canonical-graph-version" [ lock_string Kernel.canonical_graph_version ];
      "  " ^ lock_item "canonical-node-graph-version" [ lock_string Kernel.canonical_node_graph_version ];
      "  " ^ lock_item "hash-algorithm" [ lock_string Kernel.hash_algorithm ];
      "  " ^ lock_item "hash-prefix" [ lock_string Kernel.hash_prefix ];
      "  " ^ lock_item "lock-hash" [ lock_hash ];
      "  " ^ lock_item "program-hash" [ prepared.build_id ];
      "  " ^ lock_item "program-canonical-hash" [ Kernel.hash_string prepared.program_canonical ];
      "  " ^ lock_item "program-graph-hash" [ Kernel.hash_string prepared.program_graph ];
      "  " ^ lock_item "interface-hash" [ package_interface_hash manifest prepared ];
      "  " ^ lock_string_list "entrypoints" manifest.entrypoints;
      "  " ^ lock_string_list "source-dirs" manifest.source_dirs;
      "  " ^ lock_string_list "capabilities" prepared.checked.program.capabilities;
      "  " ^ lock_pair_list "package-imports" (List.map (fun i -> (i.import_name, i.import_ref)) imports);
      "  "
      ^ lock_pair_list "package-import-locks"
          (List.map (fun i -> (i.import_name, i.import_lock_hash)) imports);
      "  "
      ^ lock_pair_list "package-import-interfaces"
          (List.map (fun i -> (i.import_name, i.import_interface_hash)) imports);
      "  " ^ lock_pair_list "package-interfaces" manifest.package_interfaces;
      "  " ^ lock_item "interface" interface_items;
      "  " ^ lock_item "types" (List.map package_type_item prepared.checked.program.type_aliases);
      "  " ^ lock_item "defs" (List.map package_def_item prepared.checked.defs);
      "  " ^ lock_item "units" (List.map (unit_lock manifest) units);
      ")";
      "";
    ]

let write_package ?(locked = false) manifest =
  let prepared = prepare_build manifest in
  let interface_hash = package_interface_hash manifest prepared in
  validate_package_interface_constraints manifest interface_hash;
  let lock_hash =
    if locked then check_lock_prepared manifest prepared
    else snd (write_lock_prepared manifest prepared)
  in
  let build_result =
    if locked then build_locked manifest else build ~lock_hash manifest
  in
  let content = package_content manifest prepared lock_hash in
  let package_ref = Kernel.hash_string content in
  let dir = packages_dir manifest in
  ensure_dir dir;
  let package_path = package_path_for_ref manifest package_ref in
  write_file package_path content;
  write_file (package_current_path manifest) (package_ref ^ "\n");
  {
    package_ref;
    package_path;
    lock_hash;
    build_id = build_result.build_id;
    store = build_result.store;
  }

let check_package manifest =
  let current_path = package_current_path manifest in
  if not (Sys.file_exists current_path) then fail ("missing package pointer: " ^ current_path);
  let package_ref = trim (read_file current_path) in
  if String.equal package_ref "" then fail ("empty package pointer: " ^ current_path);
  let package_path = package_path_for_ref manifest package_ref in
  if not (Sys.file_exists package_path) then fail ("missing package descriptor: " ^ package_path);
  let content = read_file package_path in
  let actual_ref = Kernel.hash_string content in
  if not (String.equal package_ref actual_ref) then
    fail
      ("package hash mismatch: pointer " ^ package_ref ^ ", content " ^ actual_ref);
  let items = package_items content in
  let prepared = prepare_build manifest in
  let lock_hash = check_lock_prepared manifest prepared in
  let expected_content = package_content manifest prepared lock_hash in
  if not (String.equal content expected_content) then fail "package descriptor out of date";
  let expect_atom name expected =
    let actual = package_atom_field name items in
    if not (String.equal actual expected) then
      fail
        ("package " ^ name ^ " mismatch: expected " ^ expected ^ ", got " ^ actual)
  in
  let expect_string name expected =
    let actual = package_string_field name items in
    if not (String.equal actual expected) then
      fail
        ("package " ^ name ^ " mismatch: expected " ^ expected ^ ", got " ^ actual)
  in
  expect_string "package" manifest.name;
  expect_string "version" manifest.version;
  expect_string "canonical-version" Kernel.canonical_version;
  expect_string "canonical-graph-version" Kernel.canonical_graph_version;
  expect_string "canonical-node-graph-version" Kernel.canonical_node_graph_version;
  expect_string "hash-algorithm" Kernel.hash_algorithm;
  expect_string "hash-prefix" Kernel.hash_prefix;
  expect_atom "lock-hash" lock_hash;
  expect_atom "program-hash" prepared.build_id;
  expect_atom "program-canonical-hash" (Kernel.hash_string prepared.program_canonical);
  expect_atom "program-graph-hash" (Kernel.hash_string prepared.program_graph);
  let interface_hash = package_interface_hash manifest prepared in
  expect_atom "interface-hash" interface_hash;
  validate_package_interface_constraints manifest interface_hash;
  { package_ref; package_path; lock_hash; build_id = prepared.build_id; store = store_root manifest }

let stats_to_string stats =
  "parsed=" ^ string_of_int stats.parsed ^ "\nreused=" ^ string_of_int stats.reused
  ^ "\ntypechecked=" ^ string_of_int stats.typechecked ^ "\nnormalized="
  ^ string_of_int stats.normalized ^ "\ncache_hits=" ^ string_of_int stats.cache_hits
  ^ "\n"

let store_of_arg path =
  let root = project_root path in
  if Sys.file_exists (manifest_path root) then store_root (parse_manifest root)
  else realpath_or path

let project_store_of_cwd () =
  store_root (parse_manifest (Sys.getcwd ()))

let canonical_files store =
  let dir = Store.canonical_dir store in
  if not (Sys.file_exists dir) then []
  else
    Sys.readdir dir |> Array.to_list
    |> List.filter (fun f -> has_suffix ".canon" f)
    |> List.sort String.compare
    |> List.map (Filename.concat dir)

type stored_def = {
  s_name : string;
  s_def_id : string;
  s_typ : typ;
  s_canonical : string;
  s_hash : string;
  s_deps : string list;
}

let read_deps store name =
  let path = deps_path store name in
  if not (Sys.file_exists path) then []
  else read_file path |> String.split_on_char '\n' |> List.map trim |> List.filter (( <> ) "")

let stored_defs store =
  canonical_files store
  |> List.map (fun path ->
         let canonical = trim (read_file path) in
         let d = Kernel.parse_serialized_def canonical in
         {
           s_name = d.cname;
           s_def_id = d.cdef_id;
           s_typ = d.ctyp;
           s_canonical = canonical;
           s_hash = Kernel.hash_string canonical;
           s_deps = read_deps store d.cname;
         })
  |> List.sort (fun a b -> String.compare a.s_name b.s_name)

let list_store store =
  stored_defs store
  |> List.map (fun d ->
         d.s_name ^ " " ^ d.s_def_id ^ " " ^ string_of_typ d.s_typ ^ " deps=["
         ^ String.concat "," d.s_deps ^ "]")
  |> String.concat "\n"
  |> fun s -> if s = "" then "" else s ^ "\n"

let find_stored_def store id =
  stored_defs store
  |> List.find_opt (fun d -> String.equal d.s_name id || String.equal d.s_def_id id || String.equal d.s_hash id)

let get_store store id =
  if Sys.file_exists (Store.object_path store id) then Store.get_object store id
  else
    match find_stored_def store id with
    | None -> fail ("store id not found: " ^ id)
    | Some d ->
        "name=" ^ d.s_name ^ "\ndef_id=" ^ d.s_def_id ^ "\nhash=" ^ d.s_hash ^ "\ntype="
        ^ string_of_typ d.s_typ ^ "\ndeps=" ^ String.concat "," d.s_deps ^ "\ncanonical="
        ^ d.s_canonical ^ "\nnormal="
        ^ Option.value (read_trim (normal_path store d.s_name)) ~default:""
        ^ "\n"

let roots_store store =
  let path = Filename.concat store "roots" in
  if Sys.file_exists path then read_file path else ""

type diff_kind = Added | Removed | Modified

type diff_item = {
  kind : diff_kind;
  name : string;
  before : stored_def option;
  after : stored_def option;
  impacted : string list;
}

let assoc_def name defs = List.find_opt (fun d -> String.equal d.s_name name) defs

let dependents defs name =
  defs
  |> List.filter (fun d -> List.exists (String.equal name) d.s_deps)
  |> List.map (fun d -> d.s_name)
  |> sort_uniq

let diff store_a store_b =
  let a = stored_defs store_a and b = stored_defs store_b in
  let names =
    sort_uniq (List.map (fun d -> d.s_name) a @ List.map (fun d -> d.s_name) b)
  in
  names
  |> List.filter_map (fun name ->
         match (assoc_def name a, assoc_def name b) with
         | None, Some after ->
             Some
               {
                 kind = Added;
                 name;
                 before = None;
                 after = Some after;
                 impacted = dependents b name;
               }
         | Some before, None ->
             Some
               {
                 kind = Removed;
                 name;
                 before = Some before;
                 after = None;
                 impacted = dependents a name;
               }
         | Some before, Some after ->
             if String.equal before.s_def_id after.s_def_id
                && String.equal before.s_canonical after.s_canonical
             then None
             else
               Some
                 {
                   kind = Modified;
                   name;
                   before = Some before;
                   after = Some after;
                   impacted = sort_uniq (dependents a name @ dependents b name);
                 }
         | None, None -> None)

let kind_to_string = function Added -> "added" | Removed -> "removed" | Modified -> "modified"

let diff_item_to_text item =
  let before_id =
    match item.before with None -> "-" | Some d -> d.s_def_id
  in
  let after_id =
    match item.after with None -> "-" | Some d -> d.s_def_id
  in
  let type_change =
    match (item.before, item.after) with
    | Some a, Some b when not (equal_typ a.s_typ b.s_typ) ->
        " type=" ^ string_of_typ a.s_typ ^ "->" ^ string_of_typ b.s_typ
    | _ -> ""
  in
  kind_to_string item.kind ^ " " ^ item.name ^ " " ^ before_id ^ " -> " ^ after_id
  ^ type_change ^ " impacted=[" ^ String.concat "," item.impacted ^ "]"

let diff_to_text items =
  match items with
  | [] -> "No semantic changes\n"
  | xs -> String.concat "\n" (List.map diff_item_to_text xs) ^ "\n"

let json_string s = Ast.quote s

let json_array xs = "[" ^ String.concat ", " (List.map json_string xs) ^ "]"

let diff_item_to_json item =
  let field_def prefix = function
    | None -> [ "\"" ^ prefix ^ "\": null" ]
    | Some d ->
        [
          "\"" ^ prefix ^ "\": { \"defId\": " ^ json_string d.s_def_id ^ ", \"hash\": "
          ^ json_string d.s_hash ^ ", \"type\": " ^ json_string (string_of_typ d.s_typ) ^ " }";
        ]
  in
  "{ \"kind\": " ^ json_string (kind_to_string item.kind) ^ ", \"name\": "
  ^ json_string item.name ^ ", "
  ^ String.concat ", " (field_def "before" item.before @ field_def "after" item.after)
  ^ ", \"impacted\": " ^ json_array item.impacted ^ " }"

let diff_to_json items =
  "{ \"changes\": [" ^ String.concat ", " (List.map diff_item_to_json items) ^ "] }\n"

let load_store_program_with_caps store =
  let p = Store.load_program store in
  let caps =
    let path = Filename.concat store "capabilities" in
    if Sys.file_exists path then
      read_file path |> String.split_on_char '\n' |> List.map trim |> List.filter (( <> ) "")
    else []
  in
  { p with capabilities = caps }

let def_by_name defs name =
  List.find_opt (fun (d : def) -> String.equal d.name name) defs

let patch_json_for_def op deps (d : def) =
  "{ \"op\": " ^ json_string op ^ ", \"name\": " ^ json_string d.name ^ ", \"deps\": "
  ^ json_array deps ^ ", \"type\": { \"source\": " ^ json_string (string_of_typ d.typ)
  ^ " }, \"expr\": { \"source\": " ^ json_string (string_of_expr d.body) ^ " } }"

let patch_from_diff store_a store_b =
  let items = diff store_a store_b in
  let prog_a = load_store_program_with_caps store_a in
  let prog_b = load_store_program_with_caps store_b in
  let ops =
    items
    |> List.filter_map (fun item ->
           match item.kind with
           | Added ->
               let d = def_by_name prog_b.defs item.name |> Option.get in
               let deps = Kernel.dependencies_of_defs prog_b.defs item.name in
               Some (patch_json_for_def "AddDef" deps d)
           | Modified ->
               let d = def_by_name prog_b.defs item.name |> Option.get in
               let deps = Kernel.dependencies_of_defs prog_b.defs item.name in
               Some (patch_json_for_def "ReplaceDef" deps d)
           | Removed ->
               let deps = Kernel.dependencies_of_defs prog_a.defs item.name in
               Some
                 ("{ \"op\": \"DeleteDef\", \"name\": " ^ json_string item.name ^ ", \"deps\": "
                ^ json_array deps ^ " }"))
  in
  "{ \"ops\": [\n  " ^ String.concat ",\n  " ops ^ "\n] }\n"

let audit_program_graph store checked =
  let path = Filename.concat store "program.graph.json" in
  if not (Sys.file_exists path) then fail "missing canonical graph: program.graph.json";
  let stored = trim (read_file path) in
  let graph_caps, graph_defs =
    try Canonical_ir.parse_graph stored with Kernel.Error msg -> fail ("invalid canonical graph: " ^ msg)
  in
  let graph_canonical = Kernel.serialize_program graph_caps graph_defs in
  let expected_canonical = Kernel.serialize_checked_program checked in
  if not (String.equal graph_canonical expected_canonical) then
    fail "canonical graph program mismatch: program.graph.json";
  let expected = trim (Kernel.checked_to_graph_json checked) in
  if not (String.equal stored expected) then fail "canonical graph mismatch: program.graph.json"

let audit_program_canonical store checked =
  let path = Filename.concat store "program.canon" in
  if not (Sys.file_exists path) then fail "missing canonical program: program.canon";
  let stored = trim (read_file path) in
  ignore (Kernel.parse_serialized_program stored);
  let expected = Kernel.serialize_checked_program checked in
  if not (String.equal stored expected) then fail "canonical program mismatch: program.canon"

let audit manifest =
  let store = store_root manifest in
  if not (Sys.file_exists store) then fail ("store not found: " ^ store);
  let program = load_store_program_with_caps store in
  List.iter
    (fun cap ->
      if not (List.exists (String.equal cap) program.capabilities) then
        fail ("manifest capability missing from store: " ^ cap))
    manifest.capabilities;
  let checked =
    try Kernel.check_program program with Kernel.Error msg -> fail ("store program invalid: " ^ msg)
  in
  audit_program_canonical store checked;
  audit_program_graph store checked;
  List.iter
    (fun (cd : Kernel.checked_def) ->
      let canonical_path = Store.canonical_path store cd.def.name in
      if not (Sys.file_exists canonical_path) then fail ("missing canonical: " ^ cd.def.name);
      let stored = trim (read_file canonical_path) in
      ignore (Kernel.parse_serialized_def stored);
      if not (String.equal stored cd.canonical) then fail ("canonical mismatch: " ^ cd.def.name);
      let deps = read_deps store cd.def.name |> sort_uniq in
      let actual = Kernel.dependencies_of_defs program.defs cd.def.name |> sort_uniq in
      if deps <> actual then
        fail
          ("dependency mismatch in store for " ^ cd.def.name ^ ": stored ["
          ^ String.concat "," deps ^ "], actual [" ^ String.concat "," actual ^ "]");
      let capability_path = capability_scope_path store cd.def.name in
      if not (Sys.file_exists capability_path) then
        fail ("missing capability scope: " ^ cd.def.name);
      let stored_caps = read_file capability_path |> split_lines |> sort_uniq in
      let actual_caps = cd.capabilities |> sort_uniq in
      if stored_caps <> actual_caps then
        fail
          ("capability scope mismatch in store for " ^ cd.def.name ^ ": stored ["
          ^ String.concat "," stored_caps ^ "], actual [" ^ String.concat "," actual_caps ^ "]"))
    checked.defs;
  let cache = cache_root manifest in
  let _ = Runtime.persistent_cache_stats cache in
  if Sys.file_exists cache then
    Sys.readdir cache |> Array.iter (fun file ->
        if has_suffix ".cache" file then
          ignore (Runtime.cache_value_of_canonical (read_file (Filename.concat cache file))));
  let ledger = Filename.concat manifest.root ".protoss/ledger" in
  let events = Filename.concat ledger "events" in
  if Sys.file_exists events then
    Sys.readdir events |> Array.iter (fun event -> ignore (Ledger.inspect_event ledger event));
  if Sys.file_exists (package_current_path manifest) then ignore (check_package manifest);
  "Audit OK\n"
