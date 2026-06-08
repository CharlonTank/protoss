exception Error of string

let fail msg = raise (Error msg)

let read_file path =
  try Store.read_file path with Store.Error msg -> fail msg | Sys_error msg -> fail msg

let patch_audits_dir store_root = Filename.concat store_root "patches"

let patch_latest_path store_root = Filename.concat (patch_audits_dir store_root) "latest"

let patch_audit_path store_root patch_ref =
  try Filename.concat (patch_audits_dir store_root) (Store.sanitize_name patch_ref ^ ".patch")
  with Store.Error msg -> fail msg

let audit_ref_of_content content =
  Kernel.hash_string ("protoss-patch-audit-v1\n" ^ content)

let source_hash_of_source source =
  Kernel.hash_string ("protoss-patch-source-v1\n" ^ source)

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
  let computed_ref = audit_ref_of_content content in
  if not (String.equal computed_ref audit_ref) then
    fail ("patch audit hash mismatch: expected " ^ audit_ref ^ ", got " ^ computed_ref);
  let header, source =
    match split_once_string "\n--source--\n" content with
    | Some parts -> parts
    | None -> fail "patch audit missing source marker"
  in
  let fields = parse_audit_fields header in
  let source_hash = required_audit_field fields "source-hash" in
  let computed_source_hash = source_hash_of_source source in
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
