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
     capabilities = []\n";
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
  | EUnit | EBool _ | ENat _ | EString _ | EName _ | ERequest _ -> []
  | ELambda (_, t, body) -> type_refs t @ expr_type_refs body
  | ELambdaInfer (_, body) -> expr_type_refs body
  | EApp (f, x) -> expr_type_refs f @ expr_type_refs x
  | ELet (_, e, body) -> expr_type_refs e @ expr_type_refs body
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
  | EFoldList (xs, z, step) -> expr_type_refs xs @ expr_type_refs z @ expr_type_refs step
  | EText e | EColumn e | ERow e | EDone e -> expr_type_refs e
  | EImage (a, b) | EButton (a, b) | EInput (a, b) | EListView (a, b)
  | EWhenView (a, b) ->
      expr_type_refs a @ expr_type_refs b
  | EBind (p, _, t, body) -> expr_type_refs p @ type_refs t @ expr_type_refs body
  | EBindInfer (p, _, body) -> expr_type_refs p @ expr_type_refs body

and branch_type_refs = function
  | BBool (_, e) -> expr_type_refs e
  | BVariant (_, _, e) -> expr_type_refs e

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

let build ?(write = true) manifest =
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
  if write then (
    project_store_dirs store;
    List.iter (write_unit store) units;
    write_file (Filename.concat store "capabilities")
      (String.concat "\n" checked.program.capabilities ^ "\n");
    write_file (Filename.concat store "program.canon") (program_canonical ^ "\n");
    write_file (Filename.concat store "program.graph.json") program_graph;
    cleanup_removed_defs store (List.map (fun d -> d.Kernel.def.name) checked.defs);
    List.iter (write_project_def store (cache_root manifest) checked stats build_id) checked.defs;
    write_file (Filename.concat (builds_dir store) (sanitize_id build_id ^ ".build"))
      ("id=" ^ build_id ^ "\npackage=" ^ manifest.name ^ "\nversion=" ^ manifest.version
     ^ "\nprogram_hash=" ^ build_id ^ "\ndefs="
      ^ String.concat " " (List.map (fun d -> d.Kernel.def.name) checked.defs)
      ^ "\n");
    write_file (Filename.concat store "current") (build_id ^ "\n");
    write_file (Filename.concat store "roots")
      ("package=" ^ manifest.name ^ "\nversion=" ^ manifest.version ^ "\nentrypoints="
     ^ String.concat " " manifest.entrypoints ^ "\nroots="
      ^ String.concat " "
          (units
          |> List.filter (fun u ->
                 List.exists
                   (fun entry -> String.equal u.path (path_in_project manifest entry))
                   manifest.entrypoints)
          |> List.concat_map (fun u -> List.map (fun (d : def) -> d.name) u.defs))
      ^ "\n");
    write_file (Filename.concat store "world_refs") "");
  { manifest; checked; stats; build_id; store }

let check_project manifest =
  ignore (build ~write:false manifest)

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
  "Audit OK\n"
