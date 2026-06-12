open Ast

exception Error of string

let fail msg = raise (Error msg)

let rec ensure_dir path =
  if path <> "" && not (Sys.file_exists path) then (
    let parent = Filename.dirname path in
    if parent <> path then ensure_dir parent;
    Unix.mkdir path 0o755)

(* Store writes are dominated by tiny files; re-stating the same directory
   chain on every write was a measurable share of syscall time. Remember the
   directories this process already ensured. If a cached directory is removed
   behind our back, the write below fails and the caller retries once after a
   real ensure_dir (see write_file_atomic). *)
let ensured_dirs : (string, unit) Hashtbl.t = Hashtbl.create 256

let ensure_dir_cached path =
  if not (Hashtbl.mem ensured_dirs path) then (
    ensure_dir path;
    Hashtbl.replace ensured_dirs path ())

let read_file path =
  let ic = open_in path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () ->
      let len = in_channel_length ic in
      really_input_string ic len)

let write_file path content =
  let oc = open_out path in
  Fun.protect ~finally:(fun () -> close_out_noerr oc) (fun () -> output_string oc content)

(* Skip the write (and its tmp-file + rename + journal traffic) when the file
   already holds exactly [content]. Store artifacts are content-addressed or
   deterministic, so rebuilding into an existing store leaves most files
   byte-identical — reading is much cheaper than rewriting. *)
let file_holds path content =
  match open_in path with
  | exception Sys_error _ -> false
  | ic ->
      Fun.protect
        ~finally:(fun () -> close_in_noerr ic)
        (fun () ->
          let len = in_channel_length ic in
          len = String.length content && String.equal (really_input_string ic len) content)

let write_file_atomic path content =
  let dir = Filename.dirname path in
  ensure_dir_cached dir;
  let tmp = path ^ ".tmp." ^ string_of_int (Unix.getpid ()) in
  let attempt () =
    write_file tmp content;
    Sys.rename tmp path
  in
  try attempt () with
  | Sys_error _ when not (Sys.file_exists dir) ->
      (* The cached directory vanished (e.g. a test pruned the tree):
         re-create it for real and retry once. *)
      Hashtbl.remove ensured_dirs dir;
      ensure_dir_cached dir;
      (try attempt ()
       with exn ->
         if Sys.file_exists tmp then Sys.remove tmp;
         raise exn)
  | exn ->
      if Sys.file_exists tmp then Sys.remove tmp;
      raise exn

let write_file_atomic_if_changed path content =
  if not (file_holds path content) then write_file_atomic path content

(* Copy-on-write clone of a file or whole tree (APFS clonefile). Returns false
   when unsupported (non-macOS, cross-volume, destination exists); callers must
   fall back to a regular copy. *)
external try_clone : string -> string -> bool = "protoss_clonefile"

let sanitize_name name =
  if String.exists (fun c -> c = '/' || c = '\\' || c = '\000') name then
    fail ("invalid definition name for store: " ^ name);
  name

let objects_dir root = Filename.concat root "objects"

let global_store_root () =
  match Sys.getenv_opt "PROTOSS_GLOBAL_STORE" with
  | Some "" -> None
  | Some path -> Some path
  | None -> (
      match Sys.getenv_opt "HOME" with
      | Some home when home <> "" -> Some (Filename.concat home ".protoss/global-store")
      | _ -> None)

let graphs_dir root = Filename.concat root "graphs"

let defs_dir root = Filename.concat root "defs"

let canonical_dir root = Filename.concat root "canonical"

let type_aliases_path root = Filename.concat (defs_dir root) "__types.protoss"

let ensure_store root =
  ensure_dir_cached root;
  ensure_dir_cached (objects_dir root);
  ensure_dir_cached (graphs_dir root);
  ensure_dir_cached (defs_dir root);
  ensure_dir_cached (canonical_dir root)

let object_path root hash = Filename.concat (objects_dir root) hash

let graph_path root graph_hash =
  Filename.concat (graphs_dir root) (sanitize_name graph_hash ^ ".graph.json")

(* ------------------------------------------------------------------------
   Storage backend interface (docs/backend-architecture.md, brick 3).

   The content-addressed object/graph layer of the project store is the only
   genuinely *swappable* surface: every datum is keyed by its own content hash
   (objects) or by the hash of its canonical bytes (graphs), so the physical
   arrangement — files today, a SQLite table tomorrow — is an implementation
   detail of which adapter is mounted. The interface is deliberately narrow:
   it covers exactly what the store performs (idempotent put-if-absent, read,
   exists, list of objects; idempotent put of a canonical graph), NOT generic
   filesystem I/O. Source reading, lockfiles, reports, the ledger event log
   and the runtime world store all go through the plain [read_file] /
   [write_file_atomic] helpers above and are out of this layer's scope.

   The public [put_object]/[get_object]/[list_objects]/[write_graph] functions
   keep their exact historical signatures and delegate to the mounted backend,
   so every call site (and the on-disk byte layout the FS adapter produces) is
   unchanged. [object_path]/[graph_path]/[graphs_dir] remain top-level: they
   describe the FS-layout contract that direct readers across the codebase
   depend on, and the FS adapter is bound to honour exactly that layout. *)
module type BACKEND = sig
  val name : string

  (* Store [payload] under content key [hash] if absent. Idempotent: a present
     object is never rewritten (content-addressing makes the bytes immutable). *)
  val put_object_payload : root:string -> hash:string -> payload:string -> unit

  (* Read the bytes stored under content key [hash]. Raises [Error] if absent. *)
  val read_object : root:string -> hash:string -> string

  (* Whether an object with content key [hash] is present. *)
  val object_exists : root:string -> hash:string -> bool

  (* All object content keys present, sorted. *)
  val list_objects : root:string -> string list

  (* Store a canonical graph JSON under its content key [graph_hash] if its
     bytes differ from what is already stored. Deterministic JSON ⇒ identical
     rewrites are skipped. *)
  val put_graph : root:string -> graph_hash:string -> json:string -> unit
end

(* The filesystem adapter: the historical behaviour verbatim. Objects live one
   file per hash under [objects/]; graphs one file per hash under [graphs/].
   [put_object_payload] interns through the user-level global store and
   hardlinks the project object to the shared payload when possible (a
   cross-project, FS-specific dedup that an alternative backend would not
   share). *)
module Fs_backend : BACKEND = struct
  let name = "fs"

  let same_file_path a b =
    try
      let a = Unix.realpath a and b = Unix.realpath b in
      String.equal a b
    with Unix.Unix_error _ -> String.equal a b

  let write_global_object hash payload =
    match global_store_root () with
    | None -> None
    | Some root ->
        ensure_store root;
        let path = object_path root hash in
        if not (Sys.file_exists path) then write_file_atomic path payload;
        Some path

  let put_object_payload ~root ~hash ~payload =
    ensure_store root;
    let path = object_path root hash in
    if not (Sys.file_exists path) then
      match write_global_object hash payload with
      | Some global_path when not (same_file_path global_path path) -> (
          try Unix.link global_path path with Unix.Unix_error _ -> write_file_atomic path payload)
      | _ -> write_file_atomic path payload

  let read_object ~root ~hash =
    let path = object_path root hash in
    if not (Sys.file_exists path) then fail ("object not found: " ^ hash);
    read_file path

  let object_exists ~root ~hash = Sys.file_exists (object_path root hash)

  let list_objects ~root =
    let dir = objects_dir root in
    if not (Sys.file_exists dir) then []
    else Sys.readdir dir |> Array.to_list |> List.sort String.compare

  let put_graph ~root ~graph_hash ~json =
    ensure_store root;
    write_file_atomic_if_changed (graph_path root graph_hash) json
end

(* Mounted backend. Selected once per process from [PROTOSS_STORE_BACKEND]
   (and, where a manifest is in play, from its [store_backend] field — see
   [Workspace.store_backend], which validates and exports the same value). Only
   "fs" is implemented today; any other value fails loudly rather than silently
   falling back, so a misconfigured backend can never write a divergent store.
   A future "sqlite" adapter is mounted here (see docs/backend-architecture.md
   for the recommended path). *)
let backend_of_name = function
  | "" | "fs" -> (module Fs_backend : BACKEND)
  | other ->
      fail
        ("unsupported store backend: " ^ other
       ^ " (supported: fs). See docs/backend-architecture.md for the SQLite path.")

(* Resolved lazily on first store use, NOT at module-load time: an invalid
   [PROTOSS_STORE_BACKEND] must surface as a normal [Error] from the operation
   that needs the store (caught by the CLI's store-error handler → STORE001),
   never crash unrelated commands like [protoss --help] at startup. Memoized
   so the env is read once per process. *)
let active_backend_lazy =
  lazy
    (backend_of_name
       (match Sys.getenv_opt "PROTOSS_STORE_BACKEND" with
       | Some v -> String.trim v
       | None -> ""))

let active_backend () = Lazy.force active_backend_lazy

let backend_name () =
  let (module B : BACKEND) = active_backend () in
  B.name

let write_graph root graph_hash graph_json =
  let (module B : BACKEND) = active_backend () in
  B.put_graph ~root ~graph_hash ~json:graph_json

let put_object root kind content =
  let payload = "kind=" ^ kind ^ "\n" ^ content in
  let hash = Hashcons.hash payload in
  let (module B : BACKEND) = active_backend () in
  B.put_object_payload ~root ~hash ~payload;
  hash

let def_path root name = Filename.concat (defs_dir root) (sanitize_name name ^ ".protoss")

let canonical_path root name = Filename.concat (canonical_dir root) (sanitize_name name ^ ".canon")

let normal_dir root = Filename.concat root "normal"

let normal_path root name = Filename.concat (normal_dir root) (sanitize_name name ^ ".nf")

let remove_if_exists path = if Sys.file_exists path then Sys.remove path

let trim_file path = String.trim (read_file path)

let def_object_content d canonical normal =
  "name=" ^ d.name ^ "\ntype=" ^ string_of_typ d.typ ^ "\ncanonical=" ^ canonical
  ^ "\nnormal=" ^ normal ^ "\n"

let def_object_hash d canonical normal =
  Hashcons.hash ("kind=def\n" ^ def_object_content d canonical normal)

let write_def root d canonical normal =
  ensure_store root;
  let source = string_of_def d ^ "\n" in
  let object_content = def_object_content d canonical normal in
  let hash = put_object root "def" object_content in
  write_file_atomic_if_changed (canonical_path root d.name) (canonical ^ "\n");
  write_file_atomic_if_changed (def_path root d.name) source;
  hash

let delete_def root name =
  ensure_store root;
  remove_if_exists (def_path root name);
  remove_if_exists (canonical_path root name)

let write_type_aliases root aliases =
  ensure_store root;
  let path = type_aliases_path root in
  match aliases with
  | [] -> remove_if_exists path
  | _ ->
      let source = String.concat "\n" (List.map string_of_type_alias aliases) ^ "\n" in
      write_file_atomic_if_changed path source

let load_program root =
  let defs_path = defs_dir root in
  let capabilities =
    let path = Filename.concat root "capabilities" in
    if Sys.file_exists path then
      read_file path |> String.split_on_char '\n' |> List.map String.trim
      |> List.filter (fun s -> s <> "")
    else []
  in
  if not (Sys.file_exists defs_path) then
    { imports = []; capabilities; module_name = None; exports = None; type_aliases = []; defs = [] }
  else
    let files =
      Sys.readdir defs_path |> Array.to_list
      |> List.filter (fun f -> Filename.check_suffix f ".protoss")
      |> List.sort String.compare
    in
    let type_aliases, defs =
      List.fold_left
        (fun (aliases, defs) file ->
          let p = Parser.parse_string (read_file (Filename.concat defs_path file)) in
          (aliases @ p.type_aliases, defs @ p.defs))
        ([], []) files
    in
    { imports = []; capabilities; module_name = None; exports = None; type_aliases; defs }

let list_objects root =
  let (module B : BACKEND) = active_backend () in
  B.list_objects ~root

type gc_result = {
  objects : int;
  reachable : string list;
  unreachable : string list;
  deleted : string list;
}

let reachable_objects root =
  let program = load_program root in
  program.defs
  |> List.filter_map (fun d ->
         let canonical_file = canonical_path root d.name in
         let normal_file = normal_path root d.name in
         if Sys.file_exists canonical_file && Sys.file_exists normal_file then
           Some (def_object_hash d (trim_file canonical_file) (trim_file normal_file))
         else None)
  |> List.sort_uniq String.compare

let gc ?(delete = false) root =
  let objects = list_objects root in
  let reachable = reachable_objects root in
  let is_reachable object_hash = List.exists (String.equal object_hash) reachable in
  let unreachable = objects |> List.filter (fun object_hash -> not (is_reachable object_hash)) in
  let deleted =
    if delete then (
      List.iter (fun object_hash -> remove_if_exists (object_path root object_hash)) unreachable;
      unreachable)
    else []
  in
  { objects = List.length objects; reachable; unreachable; deleted }

let gc_report result =
  let lines =
    [
      "Store GC";
      "objects=" ^ string_of_int result.objects;
      "reachable=" ^ string_of_int (List.length result.reachable);
      "unreachable=" ^ string_of_int (List.length result.unreachable);
      "deleted=" ^ string_of_int (List.length result.deleted);
    ]
  in
  let unreachable =
    match result.unreachable with
    | [] -> []
    | xs -> "unreachable_objects:" :: xs
  in
  String.concat "\n" (lines @ unreachable) ^ "\n"

let get_object root hash =
  let (module B : BACKEND) = active_backend () in
  B.read_object ~root ~hash

let count_files dir suffix =
  if not (Sys.file_exists dir) then 0
  else
    Sys.readdir dir |> Array.to_list
    |> List.filter (fun f -> suffix = "" || Filename.check_suffix f suffix)
    |> List.length

let stats root =
  let objects = count_files (objects_dir root) "" in
  let defs = count_files (defs_dir root) ".protoss" in
  let canonical = count_files (canonical_dir root) ".canon" in
  (objects, defs, canonical)
