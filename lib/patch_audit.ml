exception Error of string

let fail msg = raise (Error msg)

let read_file path =
  try Store.read_file path with Store.Error msg -> fail msg | Sys_error msg -> fail msg

let patch_audits_dir store_root = Filename.concat store_root "patches"

let patch_latest_path store_root = Filename.concat (patch_audits_dir store_root) "latest"

let patch_audit_path store_root patch_ref =
  try Filename.concat (patch_audits_dir store_root) (Store.sanitize_name patch_ref ^ ".patch")
  with Store.Error msg -> fail msg

let provenance_dir store_root = Filename.concat store_root "provenance"

let root_states_dir store_root = Filename.concat (provenance_dir store_root) "roots"

let patch_provenances_dir store_root = Filename.concat (provenance_dir store_root) "patches"

let latest_root_path store_root = Filename.concat (provenance_dir store_root) "latest-root"

let latest_patch_provenance_path store_root =
  Filename.concat (provenance_dir store_root) "latest-patch"

let root_state_path store_root root_ref =
  try Filename.concat (root_states_dir store_root) (Store.sanitize_name root_ref ^ ".root")
  with Store.Error msg -> fail msg

let patch_provenance_path store_root provenance_ref =
  try
    Filename.concat (patch_provenances_dir store_root)
      (Store.sanitize_name provenance_ref ^ ".provenance")
  with Store.Error msg -> fail msg

let audit_ref_of_content content =
  Kernel.hash_string ("protoss-patch-audit-v1\n" ^ content)

let root_ref_of_content content = Kernel.hash_string content

let patch_provenance_ref_of_content content = Kernel.hash_string content

let source_hash_of_source source =
  Kernel.hash_string ("protoss-patch-source-v1\n" ^ source)

type audit = {
  audit_ref : string;
  content : string;
  previous_ref : string option;
  previous_root : string option;
  root_ref : string;
  source_hash : string;
  program_hash : string;
  result : string;
  ops : int;
}

type root_state = {
  root_ref : string;
  root_content : string;
  root_program_hash : string;
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

let option_field name fields =
  match audit_field name fields with
  | None | Some "none" | Some "" -> None
  | Some value -> Some value

let previous_latest_ref store_root =
  let path = patch_latest_path store_root in
  if Sys.file_exists path then
    let value = String.trim (read_file path) in
    if value = "" then None else Some value
  else None

let previous_root_ref store_root =
  let current_root =
    let path = latest_root_path store_root in
    if Sys.file_exists path then
      let value = String.trim (read_file path) in
      if value = "" then None else Some value
    else None
  in
  match current_root with
  | Some _ -> current_root
  | None ->
      let universe_root = Filename.concat store_root "universe.root" in
      if Sys.file_exists universe_root then
        let value = String.trim (read_file universe_root) in
        if value = "" then None else Some value
      else None

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
  let previous_ref =
    option_field "previous-ref" fields
  in
  {
    audit_ref;
    content;
    previous_ref;
    previous_root = option_field "previous-root" fields;
    root_ref = required_audit_field fields "root-ref";
    source_hash;
    program_hash = required_audit_field fields "program-hash";
    result = required_audit_field fields "result";
    ops;
  }

let verify_chain ?(ref = "latest") store_root =
  let rec loop seen ref_arg =
    let audit = verify_audit ~ref:ref_arg store_root in
    if List.exists (String.equal audit.audit_ref) seen then
      fail ("patch audit cycle: " ^ audit.audit_ref);
    match audit.previous_ref with
    | None -> audit
    | Some previous ->
        ignore (loop (audit.audit_ref :: seen) previous);
        audit
  in
  loop [] ref

let checked_store_program store_root =
  let program =
    try Store.load_program store_root with
    | Store.Error msg -> fail msg
    | Parser.Error msg -> fail ("store contains invalid definition: " ^ msg)
  in
  let checked =
    try Kernel.check_program program with
    | Kernel.Error msg -> fail ("store program invalid: " ^ msg)
  in
  checked

let root_state_of_checked checked =
  let program_canonical = Kernel.serialize_checked_program checked in
  let program_hash = Kernel.hash_program checked in
  let graph_json = Kernel.checked_to_graph_json checked in
  let host_contract_hash = Kernel.hash_string (Canonical_ir.graph_host_contract graph_json) in
  let def_lines =
    checked.Kernel.defs
    |> List.map (fun (d : Kernel.checked_def) ->
           String.concat "|"
             [
               d.def.name;
               d.def_id;
               Kernel.hash_string d.canonical;
               Kernel.hash_string (Kernel.type_to_canonical d.def.typ);
               String.concat "," d.capabilities;
             ])
    |> List.sort String.compare
  in
  let type_lines =
    checked.Kernel.program.type_aliases
    |> List.map (fun alias -> Kernel.hash_string (Ast.string_of_type_alias alias))
    |> List.sort String.compare
  in
  let content =
    String.concat "\n"
      [
        "protoss-root-state-v1";
        "program-hash=" ^ program_hash;
        "program-canonical-hash=" ^ Kernel.hash_string program_canonical;
        "program-graph-hash=" ^ Kernel.checked_to_graph_content_hash checked;
        "host-contract-content-hash=" ^ host_contract_hash;
        "capabilities=" ^ String.concat "," checked.Kernel.program.capabilities;
        "defs=" ^ String.concat " " def_lines;
        "types=" ^ String.concat " " type_lines;
      ]
  in
  { root_ref = root_ref_of_content content; root_content = content; root_program_hash = program_hash }

let current_store_root_state store_root = checked_store_program store_root |> root_state_of_checked

let current_store_program_hash store_root =
  (current_store_root_state store_root).root_program_hash

let write_root_state store_root root_state =
  Store.ensure_dir_cached (root_states_dir store_root);
  Store.ensure_dir_cached (provenance_dir store_root);
  Store.write_file_atomic (root_state_path store_root root_state.root_ref)
    (root_state.root_content ^ "\n");
  Store.write_file_atomic (latest_root_path store_root) (root_state.root_ref ^ "\n")

let patch_provenance_content ~patch_ref ~previous_ref ~previous_root ~root_ref ~program_hash =
  String.concat "\n"
    [
      "protoss-root-provenance-v1";
      "patch-ref=" ^ patch_ref;
      "previous-ref=" ^ Option.value previous_ref ~default:"none";
      "previous-root=" ^ Option.value previous_root ~default:"none";
      "root-ref=" ^ root_ref;
      "program-hash=" ^ program_hash;
    ]

let write_patch_provenance store_root ~patch_ref ~previous_ref ~previous_root ~root_ref
    ~program_hash =
  let content =
    patch_provenance_content ~patch_ref ~previous_ref ~previous_root ~root_ref ~program_hash
  in
  let provenance_ref = patch_provenance_ref_of_content content in
  Store.ensure_dir_cached (patch_provenances_dir store_root);
  Store.ensure_dir_cached (provenance_dir store_root);
  Store.write_file_atomic (patch_provenance_path store_root provenance_ref) (content ^ "\n");
  Store.write_file_atomic (latest_patch_provenance_path store_root) (provenance_ref ^ "\n");
  provenance_ref

let verify_root_state store_root root_ref =
  let path = root_state_path store_root root_ref in
  if not (Sys.file_exists path) then fail ("patch root state not found: " ^ root_ref);
  let content = read_file path |> strip_one_final_newline in
  let computed = root_ref_of_content content in
  if not (String.equal computed root_ref) then
    fail ("patch root state hash mismatch: expected " ^ root_ref ^ ", got " ^ computed);
  content

let parse_key_values content =
  content |> String.split_on_char '\n'
  |> List.filter_map (fun line ->
         match String.index_opt line '=' with
         | None -> None
         | Some i -> Some (String.sub line 0 i, String.sub line (i + 1) (String.length line - i - 1)))

let required_field fields name =
  match audit_field name fields with Some value -> value | None -> fail ("provenance missing field: " ^ name)

let verify_latest_patch_provenance store_root audit =
  let latest_path = latest_patch_provenance_path store_root in
  if not (Sys.file_exists latest_path) then fail "patch provenance latest not found";
  let provenance_ref = String.trim (read_file latest_path) in
  let path = patch_provenance_path store_root provenance_ref in
  if not (Sys.file_exists path) then fail ("patch provenance not found: " ^ provenance_ref);
  let content = read_file path |> strip_one_final_newline in
  let computed = patch_provenance_ref_of_content content in
  if not (String.equal computed provenance_ref) then
    fail
      ("patch provenance hash mismatch: expected " ^ provenance_ref ^ ", got " ^ computed);
  let fields = parse_key_values content in
  if not (String.equal (required_field fields "patch-ref") audit.audit_ref) then
    fail "patch provenance patch ref mismatch";
  if not (String.equal (required_field fields "root-ref") audit.root_ref) then
    fail "patch provenance root ref mismatch";
  (match (option_field "previous-root" fields, audit.previous_root) with
  | None, None -> ()
  | Some a, Some b when String.equal a b -> ()
  | _ -> fail "patch provenance previous root mismatch");
  provenance_ref

let verify_latest_root_pointer store_root root_ref =
  let path = latest_root_path store_root in
  if not (Sys.file_exists path) then fail "patch latest root not found";
  let latest = String.trim (read_file path) in
  if not (String.equal latest root_ref) then
    fail ("patch latest root mismatch: expected " ^ root_ref ^ ", got " ^ latest)

let verify_latest_matches_store store_root =
  let audit = verify_chain store_root in
  let current_hash = current_store_program_hash store_root in
  if not (String.equal audit.program_hash current_hash) then
    fail
      ("patch audit program hash mismatch: expected " ^ audit.program_hash ^ ", got "
      ^ current_hash);
  ignore (verify_root_state store_root audit.root_ref);
  let current_root = current_store_root_state store_root in
  if not (String.equal audit.root_ref current_root.root_ref) then
    fail
      ("patch audit root mismatch: expected " ^ audit.root_ref ^ ", got "
     ^ current_root.root_ref);
  verify_latest_root_pointer store_root audit.root_ref;
  ignore (verify_latest_patch_provenance store_root audit);
  audit

let inspect_audit ?(ref = "latest") store_root =
  let audit =
    if String.equal ref "latest" then verify_latest_matches_store store_root
    else verify_chain ~ref store_root
  in
  "Patch audit OK " ^ audit.audit_ref ^ "\n" ^ audit.content ^ "\n"
