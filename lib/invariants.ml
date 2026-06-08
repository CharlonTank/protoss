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
