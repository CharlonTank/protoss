open Ast

exception Error of string

let fail msg = raise (Error msg)

let rec ensure_dir path =
  if path <> "" && not (Sys.file_exists path) then (
    let parent = Filename.dirname path in
    if parent <> path then ensure_dir parent;
    Unix.mkdir path 0o755)

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

let write_file_atomic path content =
  let dir = Filename.dirname path in
  ensure_dir dir;
  let tmp = path ^ ".tmp." ^ string_of_int (Unix.getpid ()) in
  try
    write_file tmp content;
    Sys.rename tmp path
  with exn ->
    if Sys.file_exists tmp then Sys.remove tmp;
    raise exn

let sanitize_name name =
  if String.exists (fun c -> c = '/' || c = '\\' || c = '\000') name then
    fail ("invalid definition name for store: " ^ name);
  name

let objects_dir root = Filename.concat root "objects"

let defs_dir root = Filename.concat root "defs"

let canonical_dir root = Filename.concat root "canonical"

let ensure_store root =
  ensure_dir root;
  ensure_dir (objects_dir root);
  ensure_dir (defs_dir root);
  ensure_dir (canonical_dir root)

let object_path root hash = Filename.concat (objects_dir root) hash

let put_object root kind content =
  ensure_store root;
  let payload = "kind=" ^ kind ^ "\n" ^ content in
  let hash = "p1:" ^ Hashcons.digest payload in
  let path = object_path root hash in
  if not (Sys.file_exists path) then write_file_atomic path payload;
  hash

let def_path root name = Filename.concat (defs_dir root) (sanitize_name name ^ ".protoss")

let canonical_path root name = Filename.concat (canonical_dir root) (sanitize_name name ^ ".canon")

let remove_if_exists path = if Sys.file_exists path then Sys.remove path

let write_def root d canonical normal =
  ensure_store root;
  let source = string_of_def d ^ "\n" in
  let object_content =
    "name=" ^ d.name ^ "\ntype=" ^ string_of_typ d.typ ^ "\ncanonical=" ^ canonical
    ^ "\nnormal=" ^ normal ^ "\n"
  in
  let hash = put_object root "def" object_content in
  write_file_atomic (canonical_path root d.name) (canonical ^ "\n");
  write_file_atomic (def_path root d.name) source;
  hash

let delete_def root name =
  ensure_store root;
  remove_if_exists (def_path root name);
  remove_if_exists (canonical_path root name)

let load_program root =
  let defs_path = defs_dir root in
  let capabilities =
    let path = Filename.concat root "capabilities" in
    if Sys.file_exists path then
      read_file path |> String.split_on_char '\n' |> List.map String.trim
      |> List.filter (fun s -> s <> "")
    else []
  in
  if not (Sys.file_exists defs_path) then { imports = []; capabilities; type_aliases = []; defs = [] }
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
    { imports = []; capabilities; type_aliases; defs }

let list_objects root =
  let dir = objects_dir root in
  if not (Sys.file_exists dir) then []
  else Sys.readdir dir |> Array.to_list |> List.sort String.compare

let get_object root hash =
  let path = object_path root hash in
  if not (Sys.file_exists path) then fail ("object not found: " ^ hash);
  read_file path

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
