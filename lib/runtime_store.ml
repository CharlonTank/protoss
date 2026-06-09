(* Runtime Store Foundation.

   A durable, content-addressed runtime directory ([.protoss/runtime/]) for a web
   project. This increment only lays the foundation: a deterministic genesis
   WorldRef derived from the project's app graph / DefIds / lock / host contract,
   plus inspect/status/audit/reset. No live persistence, sync, or migration yet.

   Determinism is sacred: every persisted file is canonical JSON ([Json.to_string]
   sorts object keys), world objects are content-addressed (filename = hash of the
   file bytes), and writes are atomic (temp file + rename via
   [Store.write_file_atomic]). Audit never repairs; it only reports. *)

exception Error of string

let fail msg = raise (Error msg)

let schema_version = "runtime-store-v1"

(* -- layout ----------------------------------------------------------------- *)

let subdirs = [ "worlds"; "events"; "requests"; "responses"; "snapshots" ]

let manifest_of project =
  Workspace.parse_manifest (Workspace.project_root project)

let runtime_dir manifest =
  Filename.concat (Filename.concat manifest.Workspace.root ".protoss") "runtime"

let runtime_path project = runtime_dir (manifest_of project)

let runtime_json_path rt = Filename.concat rt "runtime.json"

let latest_world_path rt = Filename.concat rt "latest-world"

let worlds_dir rt = Filename.concat rt "worlds"

(* World refs are content hashes ([p2:...]); we use them verbatim as filenames,
   matching the existing ledger convention. *)
let world_path rt ref_ = Filename.concat (worlds_dir rt) ref_

(* -- genesis derivation ----------------------------------------------------- *)

type genesis = {
  project_name : string;
  app_graph_hash : string;
  host_contract_hash : string;
  lock_hash : string;
  init_def_id : string;
  update_def_id : string;
  view_def_id : string;
}

let def_id_of (checked : Kernel.checked) name =
  match
    List.find_opt
      (fun (d : Kernel.checked_def) -> String.equal d.def.name name)
      checked.defs
  with
  | Some d -> d.def_id
  | None -> ""

let compute_genesis manifest =
  let build = Workspace.build ~write:false manifest in
  let checked = build.Workspace.checked in
  let app_graph_hash = Kernel.checked_to_graph_content_hash checked in
  let graph_json = Kernel.checked_to_graph_json checked in
  let host_contract_hash =
    Workspace.host_contract_hash (Canonical_ir.graph_host_contract graph_json)
  in
  let lock_hash =
    let p = Workspace.lock_path manifest in
    if Sys.file_exists p then Kernel.hash_string (Store.read_file p) else ""
  in
  {
    project_name = manifest.Workspace.name;
    app_graph_hash;
    host_contract_hash;
    lock_hash;
    init_def_id = def_id_of checked "init";
    update_def_id = def_id_of checked "update";
    view_def_id = def_id_of checked "view";
  }

(* Canonical world object for the genesis world. The bytes are content-addressed:
   the WorldRef is the hash of exactly this string, so it never contains its own
   ref. Determinism comes from [Json.to_string] (sorted keys). *)
let genesis_world_json g =
  Json.to_string
    (Json.Object
       [
         ("kind", Json.String "genesis");
         ("canonicalVersion", Json.String Kernel.canonical_version);
         ("appGraphHash", Json.String g.app_graph_hash);
         ("hostContractHash", Json.String g.host_contract_hash);
         ("lockHash", Json.String g.lock_hash);
         ( "defIds",
           Json.Object
             [
               ("init", Json.String g.init_def_id);
               ("update", Json.String g.update_def_id);
               ("view", Json.String g.view_def_id);
             ] );
         ("previous", Json.Null);
         ("event", Json.Null);
       ])

let genesis_world_ref g = Hashcons.hash (genesis_world_json g)

let runtime_json_content g genesis_ref =
  Json.to_string
    (Json.Object
       [
         ("schemaVersion", Json.String schema_version);
         ("canonicalVersion", Json.String Kernel.canonical_version);
         ("project", Json.String g.project_name);
         ("appGraphHash", Json.String g.app_graph_hash);
         ("hostContractHash", Json.String g.host_contract_hash);
         ("lockHash", Json.String g.lock_hash);
         ( "defIds",
           Json.Object
             [
               ("init", Json.String g.init_def_id);
               ("update", Json.String g.update_def_id);
               ("view", Json.String g.view_def_id);
             ] );
         ("genesisWorld", Json.String genesis_ref);
       ])

(* -- filesystem helpers ----------------------------------------------------- *)

let ensure_layout rt =
  Store.ensure_dir rt;
  List.iter (fun d -> Store.ensure_dir (Filename.concat rt d)) subdirs

let rec rm_rf path =
  if Sys.file_exists path then
    if Sys.is_directory path then (
      Sys.readdir path |> Array.iter (fun e -> rm_rf (Filename.concat path e));
      Unix.rmdir path)
    else Sys.remove path

let count_files dir =
  if Sys.file_exists dir && Sys.is_directory dir then
    Sys.readdir dir |> Array.to_list
    |> List.filter (fun f -> not (Sys.is_directory (Filename.concat dir f)))
    |> List.length
  else 0

(* -- init ------------------------------------------------------------------- *)

let init project =
  let manifest = manifest_of project in
  let rt = runtime_dir manifest in
  let g = compute_genesis manifest in
  let world_json = genesis_world_json g in
  let genesis_ref = Hashcons.hash world_json in
  ensure_layout rt;
  (* World objects are immutable and content-addressed: only write if absent. *)
  let wpath = world_path rt genesis_ref in
  if not (Sys.file_exists wpath) then Store.write_file_atomic wpath world_json;
  Store.write_file_atomic (runtime_json_path rt) (runtime_json_content g genesis_ref);
  (* Never overwrite an advanced latest-world; only seed it when absent, and
     never before the target world object exists. *)
  if not (Sys.file_exists (latest_world_path rt)) then
    Store.write_file_atomic (latest_world_path rt) genesis_ref;
  Printf.sprintf "Runtime init %s\nPath %s\nGenesis %s\n" g.project_name rt genesis_ref

(* -- reading current state -------------------------------------------------- *)

let require_runtime rt =
  if not (Sys.file_exists (runtime_json_path rt)) then
    fail ("runtime not initialized (no runtime.json): " ^ rt)

let read_runtime_json rt =
  require_runtime rt;
  let content = Store.read_file (runtime_json_path rt) in
  match (try Json.parse content with Json.Error msg -> fail ("invalid runtime.json: " ^ msg)) with
  | Json.Object _ as obj -> obj
  | _ -> fail "runtime.json must be a JSON object"

let json_string_field obj name =
  match Json.field name obj with
  | Some (Json.String s) -> s
  | _ -> fail ("runtime.json missing string field: " ^ name)

let read_latest_world rt =
  let p = latest_world_path rt in
  if not (Sys.file_exists p) then fail ("missing latest-world: " ^ p);
  String.trim (Store.read_file p)

(* -- status ----------------------------------------------------------------- *)

let status project =
  let rt = runtime_path project in
  let obj = read_runtime_json rt in
  let latest = read_latest_world rt in
  Printf.sprintf
    "Path %s\n\
     SchemaVersion %s\n\
     Project %s\n\
     GenesisWorld %s\n\
     LatestWorld %s\n\
     AppGraphHash %s\n\
     HostContractHash %s\n\
     LockHash %s\n\
     Events %d\n\
     Requests %d\n\
     Responses %d\n\
     Snapshots %d\n\
     Worlds %d\n"
    rt (json_string_field obj "schemaVersion") (json_string_field obj "project")
    (json_string_field obj "genesisWorld") latest
    (json_string_field obj "appGraphHash") (json_string_field obj "hostContractHash")
    (let h = json_string_field obj "lockHash" in if h = "" then "-" else h)
    (count_files (Filename.concat rt "events"))
    (count_files (Filename.concat rt "requests"))
    (count_files (Filename.concat rt "responses"))
    (count_files (Filename.concat rt "snapshots"))
    (count_files (worlds_dir rt))

(* -- inspect ---------------------------------------------------------------- *)

let inspect project =
  let rt = runtime_path project in
  let obj = read_runtime_json rt in
  let latest = read_latest_world rt in
  let count d = Json.Num (count_files (Filename.concat rt d)) in
  Json.to_string
    (Json.Object
       [
         ("schemaVersion", Json.field "schemaVersion" obj |> Option.value ~default:Json.Null);
         ("project", Json.field "project" obj |> Option.value ~default:Json.Null);
         ("genesisWorld", Json.field "genesisWorld" obj |> Option.value ~default:Json.Null);
         ("latestWorld", Json.String latest);
         ("appGraphHash", Json.field "appGraphHash" obj |> Option.value ~default:Json.Null);
         ("hostContractHash", Json.field "hostContractHash" obj |> Option.value ~default:Json.Null);
         ("lockHash", Json.field "lockHash" obj |> Option.value ~default:Json.Null);
         ("defIds", Json.field "defIds" obj |> Option.value ~default:Json.Null);
         ( "counts",
           Json.Object
             [
               ("worlds", count "worlds");
               ("events", count "events");
               ("requests", count "requests");
               ("responses", count "responses");
               ("snapshots", count "snapshots");
             ] );
       ])
  ^ "\n"

(* -- world ------------------------------------------------------------------ *)

let world project =
  let rt = runtime_path project in
  ignore (read_runtime_json rt);
  let latest = read_latest_world rt in
  let wpath = world_path rt latest in
  if not (Sys.file_exists wpath) then
    fail ("latest-world points to unknown world object: " ^ latest);
  Printf.sprintf "WorldRef %s\n%s\n" latest (Store.read_file wpath)

(* -- audit ------------------------------------------------------------------ *)

let audit project =
  let rt = runtime_path project in
  if not (Sys.file_exists rt) then fail ("runtime not initialized: " ^ rt);
  let obj = read_runtime_json rt in
  (* schema version sanity *)
  let sv = json_string_field obj "schemaVersion" in
  if not (String.equal sv schema_version) then
    fail ("unsupported runtime schema version: " ^ sv);
  (* every world object must be content-addressed correctly *)
  let wdir = worlds_dir rt in
  if Sys.file_exists wdir then
    Sys.readdir wdir |> Array.to_list |> List.sort String.compare
    |> List.iter (fun name ->
           let path = Filename.concat wdir name in
           if not (Sys.is_directory path) then
             let actual = Hashcons.hash (Store.read_file path) in
             if not (String.equal actual name) then
               fail
                 ("world object hash mismatch: file " ^ name ^ " hashes to " ^ actual));
  (* every directory must contain only well-formed JSON payloads *)
  List.iter
    (fun sub ->
      let dir = Filename.concat rt sub in
      if Sys.file_exists dir then
        Sys.readdir dir |> Array.to_list
        |> List.iter (fun name ->
               let path = Filename.concat dir name in
               if not (Sys.is_directory path) then
                 try ignore (Json.parse (Store.read_file path))
                 with Json.Error msg ->
                   fail ("malformed JSON under " ^ sub ^ "/" ^ name ^ ": " ^ msg)))
    subdirs;
  (* latest-world must exist and point to a present world object *)
  let latest = read_latest_world rt in
  if not (Sys.file_exists (world_path rt latest)) then
    fail ("latest-world points to unknown world object: " ^ latest);
  (* genesis WorldRef must be consistent with the recorded metadata *)
  let genesis_ref = json_string_field obj "genesisWorld" in
  let def_ids = match Json.field "defIds" obj with Some o -> o | None -> Json.Null in
  let def_id name = match Json.field name def_ids with Some (Json.String s) -> s | _ -> "" in
  let g =
    {
      project_name = json_string_field obj "project";
      app_graph_hash = json_string_field obj "appGraphHash";
      host_contract_hash = json_string_field obj "hostContractHash";
      lock_hash = json_string_field obj "lockHash";
      init_def_id = def_id "init";
      update_def_id = def_id "update";
      view_def_id = def_id "view";
    }
  in
  let recomputed = genesis_world_ref g in
  if not (String.equal recomputed genesis_ref) then
    fail
      ("genesis WorldRef inconsistent with runtime metadata: recorded " ^ genesis_ref
     ^ ", derived " ^ recomputed);
  if not (Sys.file_exists (world_path rt genesis_ref)) then
    fail ("genesis world object missing: " ^ genesis_ref);
  Printf.sprintf "Runtime audit OK\nPath %s\nGenesis %s\nLatest %s\n" rt genesis_ref latest

(* -- reset ------------------------------------------------------------------ *)

let reset ~confirm project =
  if not confirm then fail "runtime reset requires --yes";
  let rt = runtime_path project in
  rm_rf rt;
  ignore (init project);
  Printf.sprintf "Runtime reset\nPath %s\n" rt
