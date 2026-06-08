type file_result = {
  source : string;
  defs : int;
  program_hash : string;
  graph_hash : string;
  normalized : int;
}

type alpha_result = {
  left : string;
  right : string;
  alpha_hash : string;
  alpha_graph_hash : string;
}

type process_result = {
  source : string;
  entry : string;
  program_hash : string;
  request_id : string;
  continuation_id : string;
  result : string;
}

type ledger_result = {
  source : string;
  entry : string;
  ledger : string;
  program_hash : string;
  request_event : string;
  resume_event : string;
  request_id : string;
  continuation_id : string;
  capability : string;
  request_tag : string;
  response_type : string;
  result : string;
}

let fail = Kernel.fail

let validate_checked source checked =
  let canonical = Kernel.serialize_checked_program checked in
  let program_hash = Kernel.hash_program checked in
  let graph = Canonical_ir.serialize_graph checked in
  if not (String.equal graph (Canonical_ir.serialize_graph checked)) then
    fail ("canonical graph is not deterministic: " ^ source);
  let graph_program = Canonical_ir.graph_to_program graph in
  if not (String.equal canonical graph_program) then
    fail ("canonical graph round-trip mismatch: " ^ source);
  let graph_checked = Canonical_ir.checked_of_graph graph in
  if not (String.equal program_hash (Kernel.hash_program graph_checked)) then
    fail ("canonical graph checked hash mismatch: " ^ source);
  if
    not
      (String.equal canonical (Kernel.serialize_checked_program graph_checked))
  then fail ("canonical graph checked serialization mismatch: " ^ source);
  let normalized = Runtime.normalize_all checked in
  {
    source;
    defs = List.length checked.Kernel.defs;
    program_hash;
    graph_hash = Kernel.hash_string graph;
    normalized = List.length normalized;
  }

let check_file file =
  validate_checked file (Loader.check_file file)

let check_graph file =
  let graph = Store.read_file file in
  let checked = Canonical_ir.checked_of_graph graph in
  let canonical = Kernel.serialize_checked_program checked in
  let graph_program = Canonical_ir.graph_to_program graph in
  if not (String.equal canonical graph_program) then
    fail ("canonical graph source mismatch: " ^ file);
  validate_checked file checked

let check_alpha left right =
  let left_checked = Loader.check_file left in
  let right_checked = Loader.check_file right in
  let left_hash = Kernel.hash_program left_checked in
  let right_hash = Kernel.hash_program right_checked in
  if not (String.equal left_hash right_hash) then
    fail ("alpha hash mismatch: " ^ left ^ " vs " ^ right);
  let left_graph = Canonical_ir.serialize_graph left_checked in
  let right_graph = Canonical_ir.serialize_graph right_checked in
  if not (String.equal left_graph right_graph) then
    fail ("alpha canonical graph mismatch: " ^ left ^ " vs " ^ right);
  {
    left;
    right;
    alpha_hash = left_hash;
    alpha_graph_hash = Kernel.hash_string left_graph;
  }

let check_process_checked source checked entry response =
  match fst (Runtime.eval_entry checked entry) with
  | Runtime.VProcessRequest suspended ->
      let serialized = Runtime.serialize_suspended suspended in
      let parsed = Runtime.parse_suspended serialized in
      let response_value = Runtime.response_value parsed.Runtime.req response in
      let result = Runtime.resume checked parsed response_value in
      {
        source;
        entry;
        program_hash = Kernel.hash_program checked;
        request_id = Runtime.request_id parsed;
        continuation_id = Runtime.continuation_id parsed;
        result = Runtime.value_to_string result;
      }
  | Runtime.VProcessDone value ->
      fail
        ("process entry did not suspend: " ^ entry ^ " finished with "
       ^ Runtime.value_to_string value)
  | value ->
      fail
        ("entry is not a Process suspension: " ^ entry ^ " = "
       ^ Runtime.value_to_string value)

let check_process file entry response =
  check_process_checked file (Loader.check_file file) entry response

let check_graph_process file entry response =
  check_process_checked file (Canonical_ir.checked_of_graph (Store.read_file file)) entry response

let default_ledger_root source entry =
  Filename.concat (Filename.get_temp_dir_name ())
    ("protoss-invariants-ledger-" ^ string_of_int (Unix.getpid ()) ^ "-"
    ^ Kernel.hash_string (source ^ ":" ^ entry))

let check_ledger_process_checked ?ledger source checked entry response =
  match fst (Runtime.eval_entry checked entry) with
  | Runtime.VProcessRequest suspended ->
      let ledger = Option.value ledger ~default:(default_ledger_root source entry) in
      let world = Ledger.init ledger in
      let serialized = Runtime.serialize_suspended suspended in
      let request_event, next_world =
        Ledger.record_request ledger world suspended.Runtime.req serialized
          (Runtime.request_id suspended) (Runtime.continuation_id suspended)
          suspended.Runtime.cap_scope
      in
      let request_fields = Ledger.event_fields ledger request_event in
      let field name =
        match Ledger.field name request_fields with
        | Some value -> value
        | None -> fail ("ledger invariant missing request field: " ^ name)
      in
      let parsed = Runtime.parse_suspended serialized in
      let response_value = Runtime.response_value parsed.Runtime.req response in
      let result = Runtime.resume checked parsed response_value in
      let result_text = Runtime.value_to_string result in
      let resume_event, _resume_world =
        Ledger.record_resume ledger next_world request_event response result_text
      in
      ignore (Ledger.event_fields ledger resume_event);
      {
        source;
        entry;
        ledger;
        program_hash = Kernel.hash_program checked;
        request_event;
        resume_event;
        request_id = Runtime.request_id suspended;
        continuation_id = Runtime.continuation_id suspended;
        capability = field "capability";
        request_tag = field "request-tag";
        response_type = field "response-type";
        result = result_text;
      }
  | Runtime.VProcessDone value ->
      fail
        ("process entry did not suspend for ledger invariant: " ^ entry ^ " finished with "
       ^ Runtime.value_to_string value)
  | value ->
      fail
        ("entry is not a Process suspension for ledger invariant: " ^ entry ^ " = "
       ^ Runtime.value_to_string value)

let check_ledger_process ?ledger file entry response =
  check_ledger_process_checked ?ledger file (Loader.check_file file) entry response

let check_graph_ledger_process ?ledger file entry response =
  check_ledger_process_checked ?ledger file
    (Canonical_ir.checked_of_graph (Store.read_file file))
    entry response

let describe_file (result : file_result) =
  "Invariants OK\nkind=file\nsource=" ^ result.source ^ "\ndefs="
  ^ string_of_int result.defs ^ "\nprogram_hash=" ^ result.program_hash
  ^ "\ngraph_hash=" ^ result.graph_hash ^ "\nnormalized="
  ^ string_of_int result.normalized ^ "\n"

let describe_alpha (result : alpha_result) =
  "Invariants OK\nkind=alpha\nleft=" ^ result.left ^ "\nright=" ^ result.right
  ^ "\nprogram_hash=" ^ result.alpha_hash ^ "\ngraph_hash="
  ^ result.alpha_graph_hash ^ "\n"

let describe_process (result : process_result) =
  "Invariants OK\nkind=process\nsource=" ^ result.source ^ "\nentry="
  ^ result.entry ^ "\nprogram_hash=" ^ result.program_hash ^ "\nrequest_id="
  ^ result.request_id ^ "\ncontinuation_id=" ^ result.continuation_id
  ^ "\nresult=" ^ result.result ^ "\n"

let describe_ledger (result : ledger_result) =
  "Invariants OK\nkind=ledger\nsource=" ^ result.source ^ "\nentry="
  ^ result.entry ^ "\nledger=" ^ result.ledger ^ "\nprogram_hash="
  ^ result.program_hash ^ "\nrequest_event=" ^ result.request_event
  ^ "\nresume_event=" ^ result.resume_event ^ "\nrequest_id="
  ^ result.request_id ^ "\ncontinuation_id=" ^ result.continuation_id
  ^ "\ncapability=" ^ result.capability ^ "\nrequest_tag="
  ^ result.request_tag ^ "\nresponse_type=" ^ result.response_type
  ^ "\nresult=" ^ result.result ^ "\n"
