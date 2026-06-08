open Ast

exception Error of string

let fail msg = raise (Error msg)

let locate path msg = path ^ ":1:1: " ^ msg

let read_file path =
  let ic = open_in path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () ->
      let len = in_channel_length ic in
      really_input_string ic len)

let is_space = function ' ' | '\t' | '\r' | '\n' -> true | _ -> false

let is_delim c = is_space c || c = '(' || c = ')' || c = ';' || c = '"'

let line_col source offset =
  let line = ref 1 and col = ref 1 in
  for i = 0 to max 0 (offset - 1) do
    if source.[i] = '\n' then (
      incr line;
      col := 1)
    else incr col
  done;
  (!line, !col)

let find_def_offset source name =
  let len = String.length source in
  let name_len = String.length name in
  let rec loop i =
    if i + 4 + name_len > len then None
    else if String.sub source i 4 = "(def" then
      let j = ref (i + 4) in
      while !j < len && is_space source.[!j] do
        incr j
      done;
      if !j + name_len <= len
         && String.sub source !j name_len = name
         && (!j + name_len = len || is_delim source.[!j + name_len])
      then Some i
      else loop (i + 1)
    else loop (i + 1)
  in
  loop 0

let def_locations path source defs =
  List.map
    (fun d ->
      let line, col =
        match find_def_offset source d.name with
        | Some offset -> line_col source offset
        | None -> (1, 1)
      in
      (d.name, path ^ ":" ^ string_of_int line ^ ":" ^ string_of_int col))
    defs

let has_prefix prefix s =
  let lp = String.length prefix in
  String.length s >= lp && String.sub s 0 lp = prefix

let take_until_colon s =
  match String.index_opt s ':' with
  | Some i -> Some (String.sub s 0 i)
  | None -> None

let extract_error_def_name msg =
  if has_prefix "definition " msg then
    let rest = String.sub msg 11 (String.length msg - 11) in
    take_until_colon rest
  else if has_prefix "duplicate definition: " msg then
    Some (String.sub msg 22 (String.length msg - 22))
  else if has_prefix "definition shadows builtin: " msg then
    Some (String.sub msg 28 (String.length msg - 28))
  else None

let locate_kernel_error locations fallback_path msg =
  match extract_error_def_name msg with
  | Some name -> (
      match List.find_opt (fun (n, _) -> String.equal n name) locations with
      | Some (_, loc) -> loc ^ ": " ^ msg
      | None -> locate fallback_path msg)
  | None -> locate fallback_path msg

let normalize_path base path =
  let raw = if Filename.is_relative path then Filename.concat base path else path in
  try Unix.realpath raw with Unix.Unix_error _ -> raw

let dirname path = Filename.dirname path

type loaded = {
  program : program;
  locations : (string * string) list;
}

let empty_loaded =
  { program = { imports = []; capabilities = []; type_aliases = []; defs = [] }; locations = [] }

let merge_loaded acc loaded import_path =
  {
    program =
      {
        imports = acc.program.imports @ loaded.program.imports @ [ import_path ];
        capabilities = acc.program.capabilities @ loaded.program.capabilities;
        type_aliases = acc.program.type_aliases @ loaded.program.type_aliases;
        defs = acc.program.defs @ loaded.program.defs;
      };
    locations = acc.locations @ loaded.locations;
  }

let rec load_file_with_locations ?(stack = []) path =
  let path = normalize_path (Sys.getcwd ()) path in
  if List.exists (String.equal path) stack then
    fail
      ("import cycle: "
      ^ String.concat " -> " (List.rev (path :: stack)));
  let source = try read_file path with Sys_error msg -> fail (locate path msg) in
  let parsed =
    try Parser.parse_string source with
    | Parser.Error msg -> fail (locate path msg)
  in
  let base = dirname path in
  let imported =
    List.fold_left
      (fun acc import_path ->
        let target = normalize_path base import_path in
        let imported = load_file_with_locations ~stack:(path :: stack) target in
        merge_loaded acc imported import_path)
      empty_loaded
      parsed.imports
  in
  let program =
    {
      imports = imported.program.imports @ parsed.imports;
      capabilities = List.sort_uniq String.compare (imported.program.capabilities @ parsed.capabilities);
      type_aliases = imported.program.type_aliases @ parsed.type_aliases;
      defs = imported.program.defs @ parsed.defs;
    }
  in
  { program; locations = imported.locations @ def_locations path source parsed.defs }

let load_file ?stack path = (load_file_with_locations ?stack path).program

let parse_file path = load_file path

let check_file path =
  let loaded = load_file_with_locations path in
  try Kernel.check_program loaded.program with
  | Kernel.Error msg -> fail (locate_kernel_error loaded.locations path msg)
