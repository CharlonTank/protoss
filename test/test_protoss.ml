open Protoss

let fail msg = raise (Failure msg)

let assert_true msg b = if not b then fail msg

let assert_equal msg a b = if a <> b then fail (msg ^ ": expected " ^ a ^ ", got " ^ b)

let contains_substring haystack needle =
  let lh = String.length haystack and ln = String.length needle in
  let rec loop i =
    i + ln <= lh
    && (String.sub haystack i ln = needle || loop (i + 1))
  in
  ln = 0 || loop 0

let replace_once haystack needle replacement =
  let lh = String.length haystack and ln = String.length needle in
  let rec loop i =
    if i + ln > lh then fail ("substring not found: " ^ needle)
    else if String.sub haystack i ln = needle then
      String.sub haystack 0 i ^ replacement
      ^ String.sub haystack (i + ln) (lh - i - ln)
    else loop (i + 1)
  in
  loop 0

let replace_nth_once n haystack needle replacement =
  let lh = String.length haystack and ln = String.length needle in
  let rec loop seen i =
    if i + ln > lh then fail ("substring occurrence not found: " ^ needle)
    else if String.sub haystack i ln = needle then
      let seen = seen + 1 in
      if seen = n then
        String.sub haystack 0 i ^ replacement
        ^ String.sub haystack (i + ln) (lh - i - ln)
      else loop seen (i + 1)
    else loop seen (i + 1)
  in
  loop 0 0

let sexp_atom_field name content =
  let rec field = function
    | Sexp.List (Sexp.Atom n :: Sexp.Atom value :: _) when String.equal n name -> Some value
    | Sexp.List xs -> List.find_map field xs
    | _ -> None
  in
  match Sexp.parse content |> List.find_map field with
  | Some value -> value
  | None -> fail ("missing sexp atom field: " ^ name)

let json_field name obj =
  match Json.field name obj with
  | Some v -> v
  | None -> fail ("missing JSON field " ^ name)

let json_string_field name obj =
  match Json.string (json_field name obj) with
  | Some s -> s
  | None -> fail ("JSON field is not string: " ^ name)

let json_array_field name obj =
  match Json.array (json_field name obj) with
  | Some xs -> xs
  | None -> fail ("JSON field is not array: " ^ name)

let json_string_array_field name obj =
  json_array_field name obj
  |> List.map (function Json.String s -> s | _ -> fail ("JSON field is not string array: " ^ name))

let json_string_array_literal xs =
  "[" ^ String.concat ", " (List.map Ast.quote xs) ^ "]"

let () =
  assert_equal "sha256 empty digest"
    "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
    (Hashcons.digest "");
  assert_equal "content address hash prefix"
    "p2:ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
    (Hashcons.hash "abc");
  assert_equal "kernel hash algorithm" "sha256" Kernel.hash_algorithm;
  assert_equal "kernel hash prefix" "p2:" Kernel.hash_prefix

let expect_parse_error input =
  try
    let _ = Parser.parse_string input in
    fail "expected parse error"
  with Parser.Error _ -> ()

let expect_parse_error_contains input needle =
  try
    let _ = Parser.parse_string input in
    fail "expected parse error"
  with Parser.Error msg ->
    assert_true ("parse error should contain " ^ needle ^ ", got " ^ msg)
      (contains_substring msg needle)

let expect_check_error input =
  try
    let p = Parser.parse_string input in
    let _ = Kernel.check_program p in
    fail "expected check error"
  with Kernel.Error _ | Parser.Error _ -> ()

let check input = Parser.parse_string input |> Kernel.check_program

let checked_def checked name =
  match
    checked.Kernel.defs
    |> List.find_opt (fun (d : Kernel.checked_def) -> String.equal d.def.name name)
  with
  | Some d -> d
  | None -> fail ("missing checked def: " ^ name)

let graph_def graph name =
  match
    json_array_field "defs" graph
    |> List.find_opt (fun def -> String.equal (json_string_field "name" def) name)
  with
  | Some def -> def
  | None -> fail ("missing graph def: " ^ name)

let temp_dir name =
  let root =
    Filename.concat (Filename.get_temp_dir_name ())
      ("protoss-test-" ^ name ^ "-" ^ string_of_int (Unix.getpid ()))
  in
  let rec rm path =
    if Sys.file_exists path then
      if Sys.is_directory path then (
        Sys.readdir path |> Array.iter (fun f -> rm (Filename.concat path f));
        Unix.rmdir path)
      else Sys.remove path
  in
  rm root;
  root

let write_file path content =
  let oc = open_out path in
  Fun.protect ~finally:(fun () -> close_out_noerr oc) (fun () -> output_string oc content)

let ensure_dir path = Store.ensure_dir path

let count_objects root = List.length (Store.list_objects root)

let rec count_files root =
  if not (Sys.file_exists root) then 0
  else if Sys.is_directory root then
    Sys.readdir root |> Array.to_list
    |> List.fold_left (fun acc f -> acc + count_files (Filename.concat root f)) 0
  else 1

let patch_file name content =
  let path = Filename.concat (Filename.get_temp_dir_name ()) name in
  write_file path content;
  path

let snapshot root =
  let rec files acc path =
    if not (Sys.file_exists path) then acc
    else if Sys.is_directory path then
      Sys.readdir path |> Array.to_list
      |> List.sort String.compare
      |> List.fold_left (fun a f -> files a (Filename.concat path f)) acc
    else (path, Store.read_file path) :: acc
  in
  files [] root |> List.sort (fun (a, _) (b, _) -> String.compare a b)

let ledger_suspension req cap_scope =
  let suspended = { Runtime.req; cont = Runtime.KDone; cap_scope } in
  ( suspended,
    Runtime.serialize_suspended suspended,
    Runtime.request_id suspended,
    Runtime.continuation_id suspended )

let rec copy_tree src dst =
  if Sys.is_directory src then (
    ensure_dir dst;
    Sys.readdir src |> Array.iter (fun f -> copy_tree (Filename.concat src f) (Filename.concat dst f)))
  else write_file dst (Store.read_file src)

let rec find_up start rel =
  let candidate = Filename.concat start rel in
  if Sys.file_exists candidate then Unix.realpath candidate
  else
    let parent = Filename.dirname start in
    if String.equal parent start then fail ("cannot find " ^ rel) else find_up parent rel

let () =
  let valid = "(def main Nat (succ 1))" in
  let invalid = "(def main Nat (succ 1)" in
  ignore (Parser.parse_string valid);
  expect_parse_error invalid;
  expect_parse_error_contains "(def main Nat\n  (succ 1)" "1:1: unterminated list";
  expect_parse_error_contains "\n)" "2:1: unexpected )";
  expect_parse_error_contains "(def s String\n  \"oops)" "2:3: unterminated string";

  ignore (check valid);
  expect_check_error "(def bad Nat true)";

  let alpha_a = check "(def main (-> Nat Nat) (lambda (x Nat) (succ x)))" in
  let alpha_b = check "(def main (-> Nat Nat) (lambda (y Nat) (succ y)))" in
  assert_equal "alpha hash" (Kernel.hash_program alpha_a) (Kernel.hash_program alpha_b);

  let deep_a =
    check
      "(def main (-> (-> Nat Nat) (-> Nat Nat)) \
       (lambda (f (-> Nat Nat)) (lambda (x Nat) (f x))))"
  in
  let deep_b =
    check
      "(def main (-> (-> Nat Nat) (-> Nat Nat)) \
       (lambda (g (-> Nat Nat)) (lambda (y Nat) (g y))))"
  in
  assert_equal "deep alpha hash" (Kernel.hash_program deep_a) (Kernel.hash_program deep_b);

  let inferred_lambda = check "(def main (-> Nat Nat) (lambda x (succ x)))" in
  let annotated_lambda = check "(def main (-> Nat Nat) (lambda (x Nat) (succ x)))" in
  assert_equal "inferred lambda canonical hash" (Kernel.hash_program annotated_lambda)
    (Kernel.hash_program inferred_lambda);
  let inferred_lambda_parens = check "(def main (-> Nat Nat) (lambda (x) (succ x)))" in
  assert_equal "parenthesized inferred lambda canonical hash" (Kernel.hash_program annotated_lambda)
    (Kernel.hash_program inferred_lambda_parens);
  let inferred_fold =
    check "(def two Nat (foldNat 2 0 (lambda acc (succ acc))))"
  in
  let inferred_fold_value, _ = Runtime.normalize_def inferred_fold "two" in
  assert_equal "inferred foldNat lambda normalizes" "2"
    (Runtime.value_to_string inferred_fold_value);
  let inferred_foldlist =
    check
      "(def xs (List Nat) (Cons Nat 1 (Cons Nat 2 (Nil Nat))))\n\
       (def total Nat \
       (foldList xs 0 (lambda x (lambda acc (succ acc)))))"
  in
  let inferred_foldlist_value, _ = Runtime.normalize_def inferred_foldlist "total" in
  assert_equal "inferred nested foldList lambdas normalize" "2"
    (Runtime.value_to_string inferred_foldlist_value);
  expect_check_error
    "(def xs (List Nat) (Cons Nat 1 (Nil Nat)))\n\
     (def bad Nat (foldList xs 0 (lambda x (lambda acc ((prim.String.eq \"a\") \"a\")))))";
  expect_check_error "(def bad Nat (lambda x x))";
  let inferred_let =
    check "(def main Nat (let (inc (-> Nat Nat) (lambda x (succ x))) (inc 4)))"
  in
  let annotated_let =
    check "(def main Nat (let (inc (lambda (x Nat) (succ x))) (inc 4)))"
  in
  assert_equal "annotated let inferred lambda hash" (Kernel.hash_program annotated_let)
    (Kernel.hash_program inferred_let);
  let inferred_let_value, _ = Runtime.normalize_def inferred_let "main" in
  assert_equal "annotated let inferred lambda normalizes" "5"
    (Runtime.value_to_string inferred_let_value);
  expect_check_error
    "(def bad Nat (let (inc (-> Nat Nat) (lambda x true)) (inc 4)))";
  expect_check_error
    "(capabilities Human.ask)\n\
     (def bad Nat (let (p (Process String) (Human.ask \"x\")) 0))";
  let inferred_list =
    check "(def xs (List Nat) (Cons 1 (Cons 2 Nil)))"
  in
  let annotated_list =
    check "(def xs (List Nat) (Cons Nat 1 (Cons Nat 2 (Nil Nat))))"
  in
  assert_equal "inferred list constructors canonical hash"
    (Kernel.hash_program annotated_list) (Kernel.hash_program inferred_list);
  let inferred_list_value, _ = Runtime.normalize_def inferred_list "xs" in
  assert_equal "inferred list constructors normalize" "[1, 2]"
    (Runtime.value_to_string inferred_list_value);
  let inferred_list_tail =
    check "(def xs (List Nat) (Cons 1 (Nil Nat)))"
  in
  let inferred_list_tail_value, _ = Runtime.normalize_def inferred_list_tail "xs" in
  assert_equal "inferred Cons can use typed tail" "[1]"
    (Runtime.value_to_string inferred_list_tail_value);
  let list_case =
    check
      "(def xs (List Nat) (Cons 1 (Cons 2 Nil)))\n\
       (def first Nat (caseList xs (Nil 0) (Cons head tail head)))\n\
       (def rest (List Nat) (caseList xs (Nil Nil) (Cons head tail tail)))\n\
       (def emptyFirst Nat (caseList (Nil Nat) (Nil 0) (Cons head tail head)))"
  in
  let first, _ = Runtime.normalize_def list_case "first" in
  assert_equal "caseList cons head normalizes" "1" (Runtime.value_to_string first);
  let rest, _ = Runtime.normalize_def list_case "rest" in
  assert_equal "caseList cons tail normalizes" "[2]" (Runtime.value_to_string rest);
  let empty_first, _ = Runtime.normalize_def list_case "emptyFirst" in
  assert_equal "caseList nil normalizes" "0" (Runtime.value_to_string empty_first);
  let list_case_capture =
    check
      "(def out Nat \
       (let (outer 7) \
       (let (xs (List Nat) (Cons outer Nil)) \
       (caseList xs (Nil 0) (Cons head tail head)))))"
  in
  let captured_head, _ = Runtime.normalize_def list_case_capture "out" in
  assert_equal "caseList substitution preserves outer refs" "7"
    (Runtime.value_to_string captured_head);
  let list_case_alpha_a =
    check
      "(def xs (List Nat) (Cons 1 Nil))\n\
       (def out Nat (caseList xs (Nil 0) (Cons head tail head)))"
  in
  let list_case_alpha_b =
    check
      "(def xs (List Nat) (Cons 1 Nil))\n\
       (def out Nat (caseList xs (Nil 0) (Cons value rest value)))"
  in
  assert_equal "caseList binder alpha-stable hash"
    (Kernel.hash_program list_case_alpha_a)
    (Kernel.hash_program list_case_alpha_b);
  assert_equal "caseList graph roundtrip" (Kernel.serialize_checked_program list_case)
    (Canonical_ir.graph_to_program (Canonical_ir.serialize_graph list_case));
  assert_true "caseList canonical graph tag"
    (contains_substring (Canonical_ir.serialize_graph list_case) "\"CaseList\"");
  expect_parse_error "(def bad Nat (caseList xs (Nil 0) (Cons x x x)))";
  expect_check_error "(def bad Nat (caseList 1 (Nil 0) (Cons head tail head)))";
  expect_check_error
    "(def xs (List Nat) (Cons 1 Nil))\n\
     (def bad Nat (caseList xs (Nil 0) (Cons head tail true)))";
  let bool_match = check "(def out Nat (match true (true 1) (false 0)))" in
  let bool_case = check "(def out Nat (case true (true 1) (false 0)))" in
  assert_equal "match Bool hashes as case" (Kernel.hash_program bool_case)
    (Kernel.hash_program bool_match);
  let bool_match_out, _ = Runtime.normalize_def bool_match "out" in
  assert_equal "match Bool normalizes" "1" (Runtime.value_to_string bool_match_out);
  let list_match =
    check
      "(def xs (List Nat) (Cons 1 (Cons 2 Nil)))\n\
       (def first Nat (match xs (Cons head tail head) (Nil 0)))"
  in
  let list_match_explicit =
    check
      "(def xs (List Nat) (Cons 1 (Cons 2 Nil)))\n\
       (def first Nat (caseList xs (Nil 0) (Cons head tail head)))"
  in
  assert_equal "match List hashes as caseList" (Kernel.hash_program list_match_explicit)
    (Kernel.hash_program list_match);
  let list_match_first, _ = Runtime.normalize_def list_match "first" in
  assert_equal "match List normalizes" "1" (Runtime.value_to_string list_match_first);
  assert_true "match List has no canonical match node"
    (not (contains_substring (Kernel.serialize_checked_program list_match) "match"));
  expect_parse_error
    "(def xs (List Nat) (Nil Nat))\n(def bad Nat (match xs (Nil 0)))";
  expect_parse_error
    "(def xs (List Nat) (Nil Nat))\n(def bad Nat (match xs (Nil 0) (Cons x x x)))";
  expect_check_error "(def bad Nat (match 1 (Nil 0) (Cons head tail head)))";
  expect_check_error "(def bad Nat Nil)";
  expect_check_error "(def bad (List Nat) (Cons true Nil))";
  expect_check_error "(def bad (List Nat) (Cons 1 true))";

  let formatted_a = check "(def main Nat (succ 1))" in
  let formatted_b = check "  ; formatting is not canonical\n\n(def   main   Nat\n  (succ   1))" in
  assert_equal "formatting independent hash" (Kernel.hash_program formatted_a)
    (Kernel.hash_program formatted_b);

  assert_equal "golden basic hash"
    "p2:f029c8a33822c2a56c9d4a7ab2abe6f87345308170f625a846829e66ff9ccfff"
    (Kernel.hash_program formatted_a);

  let canonical_def = List.hd formatted_a.Kernel.defs in
  let round_def = Kernel.parse_serialized_def canonical_def.canonical in
  assert_true "canonical v2 version" (String.contains canonical_def.canonical '2');
  assert_equal "canonical def roundtrip" canonical_def.canonical
    (Kernel.serialize_def round_def.cname round_def.cdef_id round_def.ctyp round_def.cbody (fun x -> x));
  let program_canonical =
    Kernel.serialize_program formatted_a.program.capabilities
      (List.map
         (fun (d : Kernel.checked_def) ->
           { Kernel.cname = d.def.name; cdef_id = d.def_id; ctyp = d.def.typ; cbody = d.cterm })
         formatted_a.defs)
  in
  let caps, defs = Kernel.parse_serialized_program program_canonical in
  assert_equal "canonical program roundtrip" program_canonical (Kernel.serialize_program caps defs);
  assert_true "canonical program v2 stable" (String.length program_canonical > 20);
  let graph_json = Canonical_ir.serialize_graph formatted_a in
  let graph = Json.parse graph_json in
  assert_equal "canonical graph version" Kernel.canonical_graph_version
    (json_string_field "version" graph);
  assert_equal "canonical graph hash algorithm" Kernel.hash_algorithm
    (json_string_field "hashAlgorithm" graph);
  assert_equal "canonical graph hash prefix" Kernel.hash_prefix
    (json_string_field "hashPrefix" graph);
  assert_equal "canonical graph program hash" (Kernel.hash_program formatted_a)
    (json_string_field "programHash" graph);
  let graph_content_hash = Kernel.checked_to_graph_content_hash formatted_a in
  assert_equal "canonical graph content hash" graph_content_hash
    (json_string_field "graphHash" graph);
  assert_true "canonical graph has defs" (List.length (json_array_field "defs" graph) = 1);
  assert_true "canonical graph empty capability refs" (json_array_field "capabilityRefs" graph = []);
  assert_true "canonical graph empty capability descriptors"
    (json_array_field "capabilityDescriptors" graph = []);
  let node_graph = json_field "nodeGraph" graph in
  assert_equal "canonical node graph version" Kernel.canonical_node_graph_version
    (json_string_field "version" node_graph);
  assert_equal "canonical node graph hash algorithm" Kernel.hash_algorithm
    (json_string_field "hashAlgorithm" node_graph);
  assert_equal "canonical node graph hash prefix" Kernel.hash_prefix
    (json_string_field "hashPrefix" node_graph);
  assert_equal "canonical node graph root hash" (Kernel.hash_program formatted_a)
    (json_string_field "rootProgramHash" node_graph);
  assert_true "canonical node graph has typed nodes"
    (List.length (json_array_field "nodes" node_graph) >= 3);
  let node_defs = json_array_field "defs" node_graph in
  assert_true "canonical def has node refs"
    (match json_array_field "defs" graph with
    | def :: _ ->
        String.length (json_string_field "typeRef" def) > 3
        && String.length (json_string_field "termRef" def) > 3
    | [] -> false);
  let graph_main_def = graph_def graph "main" in
  let top_type_ref = json_string_field "typeRef" graph_main_def in
  let top_term_ref = json_string_field "termRef" graph_main_def in
  (try
     ignore
       (Canonical_ir.parse_graph
          (replace_once graph_json "\"nodeGraph\": " "\"nodeGraphMissing\": "));
     fail "canonical graph missing nodeGraph should be rejected"
   with Kernel.Error msg ->
     assert_true "canonical graph rejects missing nodeGraph"
       (contains_substring msg "canonical graph missing field: nodeGraph"));
  (try
     ignore
       (Canonical_ir.parse_graph
          (replace_once graph_json "\"graphHash\": " "\"graphHashMissing\": "));
     fail "canonical graph missing graphHash should be rejected"
   with Kernel.Error msg ->
     assert_true "canonical graph rejects missing graphHash"
       (contains_substring msg "canonical graph missing field: graphHash"));
  (try
     ignore
       (Canonical_ir.parse_graph
          (replace_once graph_json
             ("\"graphHash\": " ^ Ast.quote graph_content_hash)
             "\"graphHash\": \"p2:bad\""));
     fail "canonical graph corrupt graphHash should be rejected"
   with Kernel.Error msg ->
     assert_true "canonical graph rejects corrupt graphHash"
       (contains_substring msg "canonical graph graphHash mismatch: p2:bad"));
  (try
     ignore
       (Canonical_ir.parse_graph
          (replace_once graph_json
             ("\"typeRef\": " ^ Ast.quote top_type_ref)
             "\"typeRef\": \"p2:0000000000000000000000000000000000000000000000000000000000000001\""));
     fail "canonical graph top-level typeRef mismatch should be rejected"
   with Kernel.Error msg ->
     assert_true "canonical graph rejects top-level typeRef mismatch"
       (contains_substring msg "canonical graph typeRef mismatch: main"));
  (try
     ignore
       (Canonical_ir.parse_graph
          (replace_once graph_json
             ("\"termRef\": " ^ Ast.quote top_term_ref)
             "\"termRef\": \"p2:0000000000000000000000000000000000000000000000000000000000000002\""));
     fail "canonical graph top-level termRef mismatch should be rejected"
   with Kernel.Error msg ->
     assert_true "canonical graph rejects top-level termRef mismatch"
       (contains_substring msg "canonical graph termRef mismatch: main"));
  (try
     ignore (Canonical_ir.parse_graph (replace_once graph_json "\"deps\": []" "\"depsMissing\": []"));
     fail "canonical graph missing deps should be rejected"
   with Kernel.Error msg ->
     assert_true "canonical graph rejects missing deps"
       (contains_substring msg "canonical graph missing field: deps"));
  (try
     ignore
       (Canonical_ir.parse_graph
          (replace_once graph_json "\"capabilityRefs\": []" "\"capabilityRefsMissing\": []"));
     fail "canonical graph missing capability refs should be rejected"
   with Kernel.Error msg ->
     assert_true "canonical graph rejects missing capability refs"
       (contains_substring msg "canonical graph missing field: capabilityRefs"));
  (try
     ignore
       (Canonical_ir.parse_graph
          (replace_once graph_json "{ \"version\"" "{ \"extra\": true, \"version\""));
     fail "canonical graph extra top-level field should be rejected"
   with Kernel.Error msg ->
     assert_true "canonical graph rejects extra top-level field"
       (contains_substring msg "canonical graph serialization mismatch"));
  (try
     ignore
       (Canonical_ir.parse_graph
          (replace_once graph_json "\"value\": 1" "\"extra\": true, \"value\": 1"));
     fail "canonical graph extra term field should be rejected"
   with Kernel.Error msg ->
     assert_true "canonical graph rejects extra term field"
       (contains_substring msg "canonical graph serialization mismatch"));
  (try
     ignore
       (Canonical_ir.parse_graph
          (replace_once graph_json "\"deps\": []" "\"deps\": [\"main\"]"));
     fail "canonical graph extra deps should be rejected"
   with Kernel.Error msg ->
     assert_true "canonical graph rejects extra deps"
       (contains_substring msg "canonical graph deps mismatch: main"));
  assert_true "canonical node graph def refs"
    (match node_defs with
    | def :: _ ->
        String.length (json_string_field "typeRef" def) > 3
        && String.length (json_string_field "termRef" def) > 3
    | [] -> false);
  assert_equal "canonical graph to program roundtrip" program_canonical
    (Canonical_ir.graph_to_program graph_json);
  let graph_caps, graph_defs = Canonical_ir.parse_graph graph_json in
  assert_equal "canonical graph parsed caps" "" (String.concat "," graph_caps);
  assert_equal "canonical graph parsed defs" "main"
    (String.concat "," (List.map (fun d -> d.Kernel.cname) graph_defs));
  let graph_checked = Canonical_ir.checked_of_graph graph_json in
  assert_equal "canonical graph checked hash" (Kernel.hash_program formatted_a)
    (Kernel.hash_program graph_checked);
  assert_equal "canonical graph checked serialization" (Kernel.serialize_checked_program formatted_a)
    (Kernel.serialize_checked_program graph_checked);
  let graph_value, _ = Runtime.normalize_def graph_checked "main" in
  assert_equal "canonical graph eval" "2" (Runtime.value_to_string graph_value);
  let basic_path = find_up (Sys.getcwd ()) "examples/basic.protoss" in
  let basic_invariants = Invariants.check_file basic_path in
  assert_equal "invariants file hash" (Kernel.hash_program (Loader.check_file basic_path))
    basic_invariants.Invariants.program_hash;
  let invariants_graph_dir = temp_dir "invariants-graph" in
  ensure_dir invariants_graph_dir;
  let invariants_graph_path = Filename.concat invariants_graph_dir "basic.graph.json" in
  write_file invariants_graph_path (Canonical_ir.serialize_graph (Loader.check_file basic_path));
  let graph_invariants = Invariants.check_graph invariants_graph_path in
  assert_equal "invariants graph hash" basic_invariants.program_hash
    graph_invariants.Invariants.program_hash;
  (try
     ignore (Canonical_ir.parse_graph (replace_once graph_json "\"value\": 1" "\"value\": 2"));
     fail "canonical graph typed node mismatch should be rejected"
   with Kernel.Error _ -> ());
  (try
     ignore
       (Canonical_ir.parse_graph
          (replace_once graph_json (Kernel.hash_program formatted_a)
             "p2:0000000000000000000000000000000000000000000000000000000000000000"));
     fail "canonical graph program hash mismatch should be rejected"
   with Kernel.Error _ -> ());
  (try
     ignore (Canonical_ir.parse_graph (replace_once graph_json "\"hashAlgorithm\": \"sha256\""
                                        "\"hashAlgorithm\": \"md5\""));
     fail "canonical graph hash algorithm mismatch should be rejected"
   with Kernel.Error _ -> ());
  (try
     ignore (Canonical_ir.parse_graph (replace_once graph_json "\"hashPrefix\": \"p2:\""
                                        "\"hashPrefix\": \"p1:\""));
     fail "canonical graph hash prefix mismatch should be rejected"
   with Kernel.Error _ -> ());
  (try
     ignore (Canonical_ir.parse_graph (replace_once graph_json "\"kind\": \"Type\"" "\"kind\": \"Term\""));
     fail "canonical node graph mismatch should be rejected"
   with Kernel.Error _ -> ());
  let graph_nodes = json_array_field "nodes" node_graph in
  let node_with_edges =
    match
      graph_nodes
      |> List.find_opt (fun node -> json_string_array_field "edgeRefs" node <> [])
    with
    | Some node -> node
    | None -> fail "missing canonical node with edge refs"
  in
  let edge_refs = json_string_array_field "edgeRefs" node_with_edges in
  let all_node_ids = List.map (json_string_field "id") graph_nodes in
  let extra_edge =
    match List.find_opt (fun id -> not (List.exists (String.equal id) edge_refs)) all_node_ids with
    | Some id -> id
    | None -> fail "missing extra canonical node edge target"
  in
  let edge_refs_json = "\"edgeRefs\": " ^ json_string_array_literal edge_refs in
  (try
     ignore
       (Canonical_ir.parse_graph
          (replace_once graph_json edge_refs_json "\"edgeRefs\": []"));
     fail "canonical node graph missing edge ref should be rejected"
   with Kernel.Error msg ->
     assert_true "canonical node graph rejects missing edgeRefs"
       (contains_substring msg "canonical node edgeRefs mismatch"));
  (try
     ignore
       (Canonical_ir.parse_graph
          (replace_once graph_json edge_refs_json
             ("\"edgeRefs\": " ^ json_string_array_literal (edge_refs @ [ extra_edge ]))));
     fail "canonical node graph extra edge ref should be rejected"
   with Kernel.Error msg ->
     assert_true "canonical node graph rejects extra edgeRefs"
       (contains_substring msg "canonical node edgeRefs mismatch"));
  let first_node_def =
    match node_defs with
    | def :: _ -> def
    | [] -> fail "missing canonical node def"
  in
  let extra_node_def_json =
    "{ \"name\": \"extra\", \"defId\": \"extra\", \"typeRef\": "
    ^ Ast.quote (json_string_field "typeRef" first_node_def)
    ^ ", \"termRef\": " ^ Ast.quote (json_string_field "termRef" first_node_def) ^ " }"
  in
  (try
     ignore
       (Canonical_ir.parse_graph
          (replace_nth_once 2 graph_json "\"defs\": ["
             ("\"defs\": [" ^ extra_node_def_json ^ ", ")));
     fail "canonical node graph extra def should be rejected"
   with Kernel.Error msg ->
     assert_true "canonical node graph rejects extra defs"
       (contains_substring msg "canonical node graph def count mismatch"));
  let extra_string_type_node =
    "{ \"id\": " ^ Ast.quote (Kernel.type_node_id Ast.TString)
    ^ ", \"kind\": \"Type\", \"canonical\": " ^ Ast.quote (Kernel.type_to_canonical Ast.TString)
    ^ ", \"payload\": " ^ Kernel.type_to_graph_json Ast.TString ^ ", \"edgeRefs\": [] }"
  in
  (try
     ignore
       (Canonical_ir.parse_graph
          (replace_once graph_json "\"nodes\": [" ("\"nodes\": [" ^ extra_string_type_node ^ ", ")));
     fail "canonical node graph extra unreachable node should be rejected"
   with Kernel.Error msg ->
     assert_true "canonical node graph rejects unreachable nodes"
       (contains_substring msg "canonical node graph unreachable node"));
  let duplicate_def_ids = check "(def a Nat 1)\n(def b Nat 1)" in
  let duplicate_graph = Json.parse (Canonical_ir.serialize_graph duplicate_def_ids) in
  let duplicate_node_defs = json_array_field "defs" (json_field "nodeGraph" duplicate_graph) in
  let ref_for field name =
    duplicate_node_defs
    |> List.find_opt (fun def -> String.equal (json_string_field "name" def) name)
    |> function
    | Some def -> json_string_field field def
    | None -> fail ("missing node def ref: " ^ name)
  in
  assert_equal "canonical node graph shares type nodes" (ref_for "typeRef" "a")
    (ref_for "typeRef" "b");
  assert_equal "canonical node graph shares term nodes" (ref_for "termRef" "a")
    (ref_for "termRef" "b");
  assert_equal "canonical graph allows shared DefIds"
    (Kernel.serialize_checked_program duplicate_def_ids)
    (Canonical_ir.graph_to_program (Canonical_ir.serialize_graph duplicate_def_ids));
  assert_equal "canonical graph deterministic" graph_json (Canonical_ir.serialize_graph formatted_a);
  assert_equal "canonical graph alpha-stable" (Canonical_ir.serialize_graph alpha_a)
    (Canonical_ir.serialize_graph alpha_b);
  assert_equal "canonical graph checked alpha-stable"
    (Kernel.hash_program (Canonical_ir.checked_of_graph (Canonical_ir.serialize_graph alpha_a)))
    (Kernel.hash_program (Canonical_ir.checked_of_graph (Canonical_ir.serialize_graph alpha_b)));
  let alpha_a_path = find_up (Sys.getcwd ()) "examples/alpha_a.protoss" in
  let alpha_b_path = find_up (Sys.getcwd ()) "examples/alpha_b.protoss" in
  let alpha_invariants = Invariants.check_alpha alpha_a_path alpha_b_path in
  assert_equal "invariants alpha hash" (Kernel.hash_program (Loader.check_file alpha_a_path))
    alpha_invariants.Invariants.alpha_hash;
  assert_true "canonical graph omits bound names"
    (not (contains_substring (Canonical_ir.serialize_graph alpha_a) "\"x\"")
    && not (contains_substring (Canonical_ir.serialize_graph alpha_b) "\"y\""));
  let dep_checked = check "(def two Nat (succ 1))\n(def three Nat (succ two))" in
  let dep_graph_json = Canonical_ir.serialize_graph dep_checked in
  ignore (Canonical_ir.parse_graph dep_graph_json);
  (try
     ignore
       (Canonical_ir.parse_graph
          (replace_once dep_graph_json "\"deps\": [\"two\"]" "\"deps\": []"));
     fail "canonical graph missing dependency should be rejected"
   with Kernel.Error msg ->
     assert_true "canonical graph rejects missing dependency"
       (contains_substring msg "canonical graph deps mismatch: three"));
  (try
     ignore
       (Canonical_ir.parse_graph
          (replace_once dep_graph_json "\"deps\": [\"two\"]" "\"deps\": [\"missing\"]"));
     fail "canonical graph unknown dependency should be rejected"
   with Kernel.Error msg ->
     assert_true "canonical graph rejects unknown dependency"
       (contains_substring msg "canonical graph deps unknown definition in three: missing"));
  let multi_dep_graph_json =
    check
      "(def a Nat 1)\n\
       (def b Nat 2)\n\
       (def c Nat (foldNat a b (lambda (x Nat) x)))"
    |> Canonical_ir.serialize_graph
  in
  (try
     ignore
       (Canonical_ir.parse_graph
          (replace_once multi_dep_graph_json "\"deps\": [\"a\", \"b\"]"
             "\"deps\": [\"b\", \"a\"]"));
     fail "canonical graph unsorted deps should be rejected"
   with Kernel.Error msg ->
     assert_true "canonical graph rejects unsorted deps"
       (contains_substring msg "canonical graph deps not canonical: c"));
  let dep_canon =
    Kernel.serialize_program dep_checked.program.capabilities
      (List.map
         (fun (d : Kernel.checked_def) ->
           { Kernel.cname = d.def.name; cdef_id = d.def_id; ctyp = d.def.typ; cbody = d.cterm })
         dep_checked.defs)
  in
  assert_true "canonical refs use DefId"
    (String.contains dep_canon 'r' && String.contains dep_canon ':');

  let norm = check "(def two Nat (foldNat 2 0 (lambda (x Nat) (succ x))))" in
  let v, _ = Runtime.normalize_def norm "two" in
  assert_equal "normalization" "2" (Runtime.value_to_string v);

  let defrec_nat =
    check
      "(defrec count (-> Nat Nat) (nat n) (zero 0) (step acc (succ acc)))\n\
       (def four Nat (count 4))"
  in
  let defrec_nat_explicit =
    check
      "(def count (-> Nat Nat) \
       (lambda (n Nat) (foldNat n 0 (lambda (acc Nat) (succ acc)))))\n\
       (def four Nat (count 4))"
  in
  assert_equal "defrec Nat desugars to foldNat" (Kernel.hash_program defrec_nat_explicit)
    (Kernel.hash_program defrec_nat);
  let four, _ = Runtime.normalize_def defrec_nat "four" in
  assert_equal "defrec Nat normalization" "4" (Runtime.value_to_string four);

  let defrec_list =
    check
      "(defrec bump (-> (List Nat) (List Nat)) \
       (list xs) (nil (Nil Nat)) (cons x acc (Cons Nat (succ x) acc)))\n\
       (def input (List Nat) (Cons Nat 1 (Cons Nat 2 (Nil Nat))))\n\
       (def out (List Nat) (bump input))"
  in
  let defrec_list_explicit =
    check
      "(def bump (-> (List Nat) (List Nat)) \
       (lambda (xs (List Nat)) \
       (foldList xs (Nil Nat) \
       (lambda (x Nat) (lambda (acc (List Nat)) (Cons Nat (succ x) acc))))))\n\
       (def input (List Nat) (Cons Nat 1 (Cons Nat 2 (Nil Nat))))\n\
       (def out (List Nat) (bump input))"
  in
  assert_equal "defrec List desugars to foldList" (Kernel.hash_program defrec_list_explicit)
    (Kernel.hash_program defrec_list);
  let bumped, _ = Runtime.normalize_def defrec_list "out" in
  assert_equal "defrec List normalization" "[2, 3]" (Runtime.value_to_string bumped);
  expect_parse_error "(defrec bad Nat (nat n) (zero 0) (step acc acc))";
  expect_parse_error "(defrec bad (-> Nat Nat) (zero 0) (step acc acc))";
  expect_check_error
    "(defrec bad (-> Nat Nat) (nat n) (zero 0) (step acc (bad acc)))";

  let image_view =
    check
      "(def hero (View (Variant (Open Unit))) \
       (image \"https://example.com/hero.jpg\" \"Hero\"))"
  in
  let hero, _ = Runtime.normalize_def image_view "hero" in
  assert_equal "image view normalization"
    "(image \"https://example.com/hero.jpg\" \"Hero\")"
    (Runtime.value_to_string hero);

  let variant =
    check
      "(def value (Variant (None Unit) (Some Nat)) \
       (variant (Variant (None Unit) (Some Nat)) Some 7))\n\
       (def out Nat (case value (None u 0) (Some x x)))"
  in
  let out, _ = Runtime.normalize_def variant "out" in
  assert_equal "variant case" "7" (Runtime.value_to_string out);

  let record_order_a =
    check "(def r (Record (a Nat) (b Bool)) (record (b true) (a 1)))"
  in
  let record_order_b =
    check "(def r (Record (b Bool) (a Nat)) (record (a 1) (b true)))"
  in
  assert_equal "record canonical order" (Kernel.hash_program record_order_a)
    (Kernel.hash_program record_order_b);
  let record_destructure =
    check
      "(def p (Record (name String) (count Nat)) (record (name \"Ada\") (count 3)))\n\
       (def out Nat (letRecord p (name (count n)) n))"
  in
  let record_destructure_explicit =
    check
      "(def p (Record (name String) (count Nat)) (record (name \"Ada\") (count 3)))\n\
       (def out Nat \
       (let (__record0 p) \
       (let (n (get __record0 count)) \
       (let (name (get __record0 name)) n))))"
  in
  assert_equal "letRecord hashes as explicit field lets"
    (Kernel.hash_program record_destructure_explicit)
    (Kernel.hash_program record_destructure);
  let record_destructure_out, _ = Runtime.normalize_def record_destructure "out" in
  assert_equal "letRecord normalizes" "3" (Runtime.value_to_string record_destructure_out);
  let record_destructure_order =
    check
      "(def p (Record (name String) (count Nat)) (record (name \"Ada\") (count 3)))\n\
       (def out Nat (letRecord p ((count n) name) n))"
  in
  assert_equal "letRecord field order stable hash"
    (Kernel.hash_program record_destructure)
    (Kernel.hash_program record_destructure_order);
  let record_destructure_renamed =
    check
      "(def p (Record (name String) (count Nat)) (record (name \"Ada\") (count 3)))\n\
       (def out Nat (letRecord p (name (count value)) value))"
  in
  assert_equal "letRecord binder alpha-stable hash"
    (Kernel.hash_program record_destructure)
    (Kernel.hash_program record_destructure_renamed);
  assert_true "letRecord is surface-only canonical syntax"
    (not (contains_substring (Kernel.serialize_checked_program record_destructure) "letRecord"));
  let record_match =
    check
      "(def p (Record (name String) (count Nat)) (record (name \"Ada\") (count 3)))\n\
       (def out Nat (match p ((record name (count n)) n)))"
  in
  assert_equal "match Record hashes as letRecord" (Kernel.hash_program record_destructure)
    (Kernel.hash_program record_match);
  let record_match_out, _ = Runtime.normalize_def record_match "out" in
  assert_equal "match Record normalizes" "3" (Runtime.value_to_string record_match_out);
  assert_true "match Record has no canonical match node"
    (not (contains_substring (Kernel.serialize_checked_program record_match) "match"));
  let record_destructure_fresh =
    check
      "(def out Nat \
       (let (__record0 9) \
       (letRecord (record (value 3)) (value) __record0)))"
  in
  let record_destructure_fresh_out, _ = Runtime.normalize_def record_destructure_fresh "out" in
  assert_equal "letRecord temp avoids source names" "9"
    (Runtime.value_to_string record_destructure_fresh_out);
  expect_parse_error
    "(def bad Nat (letRecord (record (a 1)) (a a) a))";
  expect_parse_error
    "(def bad Nat (letRecord (record (a 1) (b 2)) ((a x) (b x)) x))";
  expect_parse_error
    "(def bad Nat (match (record (a 1)) ((record (a x)) x) ((record (a y)) y)))";
  expect_check_error
    "(def p (Record (name String)) (record (name \"Ada\")))\n\
     (def bad Nat (letRecord p (count) count))";
  expect_check_error "(def bad Nat (letRecord 1 (count) count))";

  let named_model =
    check
      "(type Model (Record (name String) (count Nat)))\n\
       (def init Model (record (name \"Ada\") (count 1)))"
  in
  let named_record_model =
    check
      "(record Model (name String) (count Nat))\n\
       (def init Model (record (name \"Ada\") (count 1)))"
  in
  let expanded_model =
    check "(def init (Record (name String) (count Nat)) (record (name \"Ada\") (count 1)))"
  in
  assert_equal "type alias transparent hash" (Kernel.hash_program expanded_model)
    (Kernel.hash_program named_model);
  assert_equal "named record transparent hash" (Kernel.hash_program expanded_model)
    (Kernel.hash_program named_record_model);
  let named_init, _ = Runtime.normalize_def named_model "init" in
  assert_equal "type alias runtime"
    "{count = 1, name = \"Ada\"}" (Runtime.value_to_string named_init);
  expect_check_error "(def bad MissingType 0)";
  expect_check_error "(type Loop Loop)\n(def bad Loop 0)";
  expect_check_error "(type A B)\n(type B A)\n(def bad A 0)";

  let maybe_alias =
    check
      "(type Maybe (A) (Variant (None Unit) (Some A)))\n\
       (def value (Maybe Nat) (variant (Maybe Nat) Some 4))\n\
       (def out Nat (case value (None _ 0) (Some n n)))"
  in
  let maybe_inferred_ctor =
    check
      "(type Maybe (A) (Variant (None Unit) (Some A)))\n\
       (def value (Maybe Nat) (variant Some 4))\n\
       (def out Nat (case value (None _ 0) (Some n n)))"
  in
  let maybe_variant_decl =
    check
      "(variant Maybe (params A) (None Unit) (Some A))\n\
       (def value (Maybe Nat) (variant (Maybe Nat) Some 4))\n\
       (def out Nat (case value (None _ 0) (Some n n)))"
  in
  let maybe_expanded =
    check
      "(def value (Variant (None Unit) (Some Nat)) \
       (variant (Variant (None Unit) (Some Nat)) Some 4))\n\
       (def out Nat (case value (None _ 0) (Some n n)))"
  in
  let maybe_unit_branch_shorthand =
    check
      "(type Maybe (A) (Variant (None Unit) (Some A)))\n\
       (def value (Maybe Nat) (variant Some 4))\n\
       (def out Nat (case value (None 0) (Some n n)))"
  in
  assert_equal "parametric type alias transparent hash" (Kernel.hash_program maybe_expanded)
    (Kernel.hash_program maybe_alias);
  assert_equal "inferred variant constructor transparent hash" (Kernel.hash_program maybe_expanded)
    (Kernel.hash_program maybe_inferred_ctor);
  assert_equal "named variant transparent hash" (Kernel.hash_program maybe_expanded)
    (Kernel.hash_program maybe_variant_decl);
  assert_equal "unit variant branch shorthand hash" (Kernel.hash_program maybe_inferred_ctor)
    (Kernel.hash_program maybe_unit_branch_shorthand);
  let maybe_match =
    check
      "(type Maybe (A) (Variant (None Unit) (Some A)))\n\
       (def value (Maybe Nat) (variant Some 4))\n\
       (def out Nat (match value (None 0) (Some n n)))"
  in
  assert_equal "match Variant hashes as case" (Kernel.hash_program maybe_unit_branch_shorthand)
    (Kernel.hash_program maybe_match);
  let maybe_out, _ = Runtime.normalize_def maybe_alias "out" in
  assert_equal "parametric type alias runtime" "4" (Runtime.value_to_string maybe_out);
  let maybe_short_out, _ = Runtime.normalize_def maybe_unit_branch_shorthand "out" in
  assert_equal "unit variant branch shorthand runtime" "4" (Runtime.value_to_string maybe_short_out);
  let fold_variant_unit_short =
    check
      "(variant Maybe (params A) (None Unit) (Some A))\n\
       (def value (Maybe Nat) (variant None unit))\n\
       (def out Nat (foldVariant (Maybe Nat) Nat value (None 0) (Some n n)))"
  in
  let fold_variant_unit_explicit =
    check
      "(variant Maybe (params A) (None Unit) (Some A))\n\
       (def value (Maybe Nat) (variant None unit))\n\
       (def out Nat (foldVariant (Maybe Nat) Nat value (None _ 0) (Some n n)))"
  in
  assert_equal "foldVariant unit branch shorthand hash"
    (Kernel.hash_program fold_variant_unit_explicit)
    (Kernel.hash_program fold_variant_unit_short);
  let fold_variant_out, _ = Runtime.normalize_def fold_variant_unit_short "out" in
  assert_equal "foldVariant unit branch shorthand runtime" "0"
    (Runtime.value_to_string fold_variant_out);
  let inferred_ctor_contexts =
    check
      "(variant Maybe (params A) (None Unit) (Some A))\n\
       (record Box (value (Maybe Nat)))\n\
       (def mkSome (-> Nat (Maybe Nat)) (lambda (n Nat) (variant Some n)))\n\
       (def boxed Box (record (value (variant Some 1))))\n\
       (def selected (Maybe Nat) (case true (true (variant Some 2)) (false (variant None unit))))\n\
       (def folded (Maybe Nat) \
       (foldNat 1 (variant None unit) (lambda (acc (Maybe Nat)) (variant Some 3))))"
  in
  let selected, _ = Runtime.normalize_def inferred_ctor_contexts "selected" in
  assert_equal "inferred variant in case branch" "Some 2" (Runtime.value_to_string selected);
  let folded, _ = Runtime.normalize_def inferred_ctor_contexts "folded" in
  assert_equal "inferred variant in fold" "Some 3" (Runtime.value_to_string folded);
  expect_check_error
    "(variant Maybe (params A) (None Unit) (Some A))\n(def bad Nat (variant Some 1))";
  expect_check_error
    "(variant Maybe (params A) (None Unit) (Some A))\n(def bad (Maybe Nat) (variant Nope 1))";
  expect_check_error
    "(variant Maybe (params A) (None Unit) (Some A))\n(def bad (Maybe Nat) (variant Some true))";
  expect_check_error
    "(variant Maybe (params A) (None Unit) (Some A))\n\
     (def value (Maybe Nat) (variant Some 1))\n\
     (def bad Nat (case value (None 0) (Some 1)))";
  expect_check_error
    "(variant Maybe (params A) (None Unit) (Some A))\n\
     (def bad (Maybe Nat) (variant (Variant (None Unit) (Some Bool)) Some true))";
  let poly_a =
    check
      "(type Maybe (A) (Variant (None Unit) (Some A)))\n\
       (defpoly id (params A) (-> A A) (lambda (x A) x))\n\
       (defpoly some (params A) (-> A (Maybe A)) (lambda (x A) (variant Some x)))\n\
       (def n Nat ((inst id Nat) 4))\n\
       (def s String ((inst id String) \"ok\"))\n\
       (def m (Maybe Nat) ((inst some Nat) 9))\n\
       (def out Nat (case m (None _ 0) (Some value value)))"
  in
  let poly_b =
    check
      "(type Maybe (X) (Variant (None Unit) (Some X)))\n\
       (defpoly id (params B) (-> B B) (lambda (y B) y))\n\
       (defpoly some (params B) (-> B (Maybe B)) (lambda (y B) (variant Some y)))\n\
       (def n Nat ((inst id Nat) 4))\n\
       (def s String ((inst id String) \"ok\"))\n\
       (def m (Maybe Nat) ((inst some Nat) 9))\n\
       (def out Nat (case m (None _ 0) (Some value value)))"
  in
  let poly_implicit =
    check
      "(type Maybe (A) (Variant (None Unit) (Some A)))\n\
       (defpoly id (params A) (-> A A) (lambda (x A) x))\n\
       (defpoly some (params A) (-> A (Maybe A)) (lambda (x A) (variant Some x)))\n\
       (def n Nat (id 4))\n\
       (def s String (id \"ok\"))\n\
       (def m (Maybe Nat) (some 9))\n\
       (def out Nat (case m (None 0) (Some value value)))"
  in
  assert_equal "defpoly type parameter alpha-stable hash" (Kernel.hash_program poly_a)
    (Kernel.hash_program poly_b);
  assert_equal "defpoly implicit instantiation hash" (Kernel.hash_program poly_a)
    (Kernel.hash_program poly_implicit);
  let poly_n, _ = Runtime.normalize_def poly_a "n" in
  assert_equal "defpoly Nat instantiation" "4" (Runtime.value_to_string poly_n);
  let poly_s, _ = Runtime.normalize_def poly_a "s" in
  assert_equal "defpoly String instantiation" "\"ok\"" (Runtime.value_to_string poly_s);
  let poly_out, _ = Runtime.normalize_def poly_a "out" in
  assert_equal "defpoly variant instantiation" "9" (Runtime.value_to_string poly_out);
  let poly_contextual_function =
    check
      "(defpoly id (params A) (-> A A) (lambda (x A) x))\n\
       (def f (-> Nat Nat) id)\n\
       (def out Nat (f 4))"
  in
  let poly_contextual_function_explicit =
    check
      "(defpoly id (params A) (-> A A) (lambda (x A) x))\n\
       (def f (-> Nat Nat) (inst id Nat))\n\
       (def out Nat (f 4))"
  in
  assert_equal "defpoly expected function hash"
    (Kernel.hash_program poly_contextual_function_explicit)
    (Kernel.hash_program poly_contextual_function);
  let poly_contextual_out, _ = Runtime.normalize_def poly_contextual_function "out" in
  assert_equal "defpoly expected function normalization" "4"
    (Runtime.value_to_string poly_contextual_out);
  let poly_shadowing =
    check
      "(defpoly id (params A) (-> A A) (lambda (x A) x))\n\
       (def out Nat (let (id (lambda (x Nat) (succ x))) (id 1)))"
  in
  let poly_shadowing_out, _ = Runtime.normalize_def poly_shadowing "out" in
  assert_equal "defpoly inference respects local shadowing" "2"
    (Runtime.value_to_string poly_shadowing_out);
  let poly_map_explicit =
    check
      "(defpoly List.map (params A B) \
       (-> (List A) (-> (-> A B) (List B))) \
       (lambda (xs (List A)) \
       (lambda (f (-> A B)) \
       (foldList xs (Nil B) \
       (lambda (x A) (lambda (acc (List B)) (Cons B (f x) acc)))))))\n\
       (def xs (List Nat) (Cons Nat 1 (Cons Nat 2 (Nil Nat))))\n\
       (def bumped (List Nat) (((inst List.map Nat Nat) xs) (lambda (x Nat) (succ x))))"
  in
  let poly_map_implicit =
    check
      "(defpoly List.map (params A B) \
       (-> (List A) (-> (-> A B) (List B))) \
       (lambda (xs (List A)) \
       (lambda (f (-> A B)) \
       (foldList xs (Nil B) \
       (lambda (x A) (lambda (acc (List B)) (Cons B (f x) acc)))))))\n\
       (def xs (List Nat) (Cons 1 (Cons 2 Nil)))\n\
       (def bumped (List Nat) ((List.map xs) (lambda x (succ x))))"
  in
  assert_equal "defpoly List.map implicit hash" (Kernel.hash_program poly_map_explicit)
    (Kernel.hash_program poly_map_implicit);
  let bumped, _ = Runtime.normalize_def poly_map_implicit "bumped" in
  assert_equal "defpoly List.map implicit normalization" "[2, 3]"
    (Runtime.value_to_string bumped);
  expect_check_error
    "(defpoly empty (params A) (List A) (Nil A))\n\
     (def bad Nat (let (x empty) 0))";
  expect_check_error
    "(defpoly same (params A) (-> A (-> A A)) \
       (lambda (x A) (lambda (y A) x)))\n\
     (def bad Nat ((same 1) true))";
  expect_check_error
    "(defpoly id (params A) (-> A A) (lambda (x A) x))\n(def bad Nat ((inst id) 1))";
  expect_check_error
    "(defpoly id (params A) (-> A A) (lambda (x A) x))\n\
     (def bad Nat ((inst id Nat String) 1))";
  let result_pair =
    check
      "(variant Result (params E A) (Err E) (Ok A))\n\
       (record Pair (params A B) (first A) (second B))\n\
       (def r (Result String Nat) (variant (Result String Nat) Ok 7))\n\
       (def p (Pair String Nat) (record (first \"n\") (second 7)))"
  in
  ignore result_pair;
  expect_parse_error "(record Bad (x Nat) (x Bool))";
  expect_parse_error "(variant Bad (Same Unit) (Same Nat))";
  expect_check_error
    "(type Maybe (A) (Variant (None Unit) (Some A)))\n(def bad Maybe unit)";
  expect_check_error
    "(type Maybe (A) (Variant (None Unit) (Some A)))\n(def bad (Maybe Nat Bool) unit)";
  expect_check_error "(type Bad (A A) A)\n(def bad (Bad Nat Nat) 0)";

  let variant_order_a =
    check
      "(def v (Variant (None Unit) (Some Nat)) \
       (variant (Variant (Some Nat) (None Unit)) Some 1))"
  in
  let variant_order_b =
    check
      "(def v (Variant (Some Nat) (None Unit)) \
       (variant (Variant (None Unit) (Some Nat)) Some 1))"
  in
  assert_equal "variant canonical order" (Kernel.hash_program variant_order_a)
    (Kernel.hash_program variant_order_b);

  let recursive_tree_path = find_up (Sys.getcwd ()) "examples/recursive_tree.protoss" in
  let recursive_tree = Loader.check_file recursive_tree_path in
  let leftmost, _ = Runtime.normalize_def recursive_tree "leftmost" in
  assert_equal "recursive Tree case normalization" "1" (Runtime.value_to_string leftmost);
  let mirrored, _ = Runtime.normalize_def recursive_tree "mirrored" in
  assert_equal "recursive Tree rebuild"
    "Node {left = Leaf 2, right = Leaf 1}" (Runtime.value_to_string mirrored);
  let size, _ = Runtime.normalize_def recursive_tree "size" in
  assert_equal "recursive Tree foldVariant size" "2" (Runtime.value_to_string size);
  let mirrored_fold, _ = Runtime.normalize_def recursive_tree "mirroredFold" in
  assert_equal "recursive Tree foldVariant rebuild"
    "Node {left = Leaf 2, right = Leaf 1}" (Runtime.value_to_string mirrored_fold);
  assert_true "recursive type appears as nominal canonical type"
    (contains_substring (Kernel.serialize_checked_program recursive_tree) "(Named Tree Nat)");
  assert_true "recursive foldVariant appears in canonical program"
    (contains_substring (Kernel.serialize_checked_program recursive_tree) "(foldVariant");
  assert_equal "recursive Tree graph roundtrip" (Kernel.serialize_checked_program recursive_tree)
    (Canonical_ir.graph_to_program (Canonical_ir.serialize_graph recursive_tree));
  let recursive_graph_checked =
    Canonical_ir.checked_of_graph (Canonical_ir.serialize_graph recursive_tree)
  in
  assert_equal "recursive Tree graph checked hash" (Kernel.hash_program recursive_tree)
    (Kernel.hash_program recursive_graph_checked);
  let graph_size, _ = Runtime.normalize_def recursive_graph_checked "size" in
  assert_equal "recursive Tree graph eval" "2" (Runtime.value_to_string graph_size);
  expect_check_error "(type Bad Bad)\n(def bad Bad unit)";
  expect_check_error "(record Bad (next Bad))\n(def bad Bad unit)";
  expect_check_error "(variant Bad (Apply (-> Bad Nat)))\n(def bad Bad unit)";
  let tree_base =
    "(variant Tree (params A) \
     (Leaf A) \
     (Node (Record (left (Tree A)) (right (Tree A)))))\n\
     (def leaf (Tree Nat) (variant Leaf 1))\n\
     (def tree (Tree Nat) \
       (variant Node (record (left leaf) (right (variant Leaf 2)))))\n"
  in
  expect_check_error (tree_base ^ "(def bad Nat (recur tree))");
  expect_check_error
    (tree_base
    ^ "(def bad Nat \
        (foldVariant (Tree Nat) Nat tree \
          (Leaf n 1) \
          (Node pair (recur tree))))");
  expect_check_error
    (tree_base
    ^ "(def bad Nat \
        (foldVariant (Tree Nat) Nat tree \
          (Leaf n 1) \
          (Node pair (recur (variant Leaf 0)))))");
  expect_check_error
    (tree_base
    ^ "(def bad Nat \
        (foldVariant (Tree Nat) Nat tree \
          (Leaf n 1) \
          (Node pair \
            (let (pair (record (left leaf) (right leaf))) \
              (recur (get pair left))))))");
  expect_check_error
    (tree_base
    ^ "(def bad (-> Nat Nat) \
        (foldVariant (Tree Nat) (-> Nat Nat) tree \
          (Leaf n (lambda (x Nat) x)) \
          (Node pair (lambda (x Nat) (recur (get pair left))))))");

  let kernel_nf = Kernel.normalize_checked_def norm "two" in
  assert_equal "kernel pure normalizer" "2" (Kernel.cterm_to_string kernel_nf);

  assert_equal "deterministic hash" (Kernel.hash_program norm) (Kernel.hash_program norm);
  let diff = check "(def two Nat (succ 2))" in
  assert_true "different terms must hash differently" (Kernel.hash_program norm <> Kernel.hash_program diff);

  let memo =
    check "(def inc (-> Nat Nat) (lambda (x Nat) (succ x)))\n\
           (def b Nat (let (x (inc 41)) (let (y (inc 41)) y)))"
  in
  let _, trace = Runtime.eval_entry ~trace_cache:true memo "b" in
  assert_true "memo trace should contain cache hit"
    (List.exists (fun line -> String.length line >= 9 && String.sub line 0 9 = "cache hit") trace);
  let closure_cache =
    check
      "(def eqTo (-> Nat (-> Nat Bool)) \
       (lambda (n Nat) (lambda (x Nat) ((prim.Nat.eq n) x))))\n\
       (def out Bool \
       (let (eqOne (eqTo 1)) \
       (let (eqTwo (eqTo 2)) \
       (case (eqOne 2) (true false) (false (eqTwo 2))))))"
  in
  let closure_cache_out, _ = Runtime.normalize_def ~trace_cache:true closure_cache "out" in
  assert_equal "memo key includes closure environment" "true"
    (Runtime.value_to_string closure_cache_out);

  let cache_dir = temp_dir "persistent-cache" in
  let _, _ = Runtime.eval_entry ~trace_cache:true ~cache_dir memo "b" in
  let _, persistent_trace = Runtime.eval_entry ~trace_cache:true ~cache_dir memo "b" in
  let hits, misses, entries = Runtime.persistent_cache_stats cache_dir in
  assert_true "persistent cache should have entries" (entries > 0);
  assert_true "persistent cache should record misses" (misses > 0);
  assert_true "persistent cache should record hits" (hits > 0);
  assert_true "persistent cache trace should contain disk hit"
    (List.exists
       (fun line -> String.length line >= 20 && String.sub line 0 20 = "cache hit persistent")
       persistent_trace);

  let import_root = temp_dir "imports" in
  ensure_dir import_root;
  let prelude_path = Filename.concat import_root "prelude.protoss" in
  write_file prelude_path
    "(def Nat.add (-> Nat (-> Nat Nat))\n\
     \  (lambda (a Nat) (lambda (b Nat) (foldNat a b (lambda (x Nat) (succ x))))))\n\
     (def Bool.not (-> Bool Bool) (lambda (b Bool) (case b (true false) (false true))))\n\
     (def List.mapNat (-> (List Nat) (-> (-> Nat Nat) (List Nat)))\n\
     \  (lambda (xs (List Nat))\n\
     \    (lambda (f (-> Nat Nat))\n\
     \      (foldList xs (Nil Nat)\n\
     \        (lambda (x Nat) (lambda (acc (List Nat)) (Cons Nat (f x) acc)))))))\n";
  let app_a = Filename.concat import_root "app_a.protoss" in
  let app_b = Filename.concat import_root "app_b.protoss" in
  write_file app_a
    "(import \"prelude.protoss\")\n\
     (def xs (List Nat) (Cons Nat 1 (Cons Nat 2 (Nil Nat))))\n\
     (def mapped (List Nat) ((List.mapNat xs) (lambda (x Nat) (succ x))))\n\
     (def total Nat (foldList mapped 0 (lambda (x Nat) (lambda (acc Nat) ((Nat.add x) acc)))))\n";
  write_file app_b
    "(import \"./prelude.protoss\")\n\
     (def xs (List Nat) (Cons Nat 1 (Cons Nat 2 (Nil Nat))))\n\
     (def mapped (List Nat) ((List.mapNat xs) (lambda (y Nat) (succ y))))\n\
     (def total Nat (foldList mapped 0 (lambda (z Nat) (lambda (acc Nat) ((Nat.add z) acc)))))\n";
  let imported_a = Loader.check_file app_a in
  let imported_b = Loader.check_file app_b in
  let total, _ = Runtime.normalize_def imported_a "total" in
  assert_equal "imports stdlib List/foldList" "5" (Runtime.value_to_string total);
  assert_equal "import path raw text ignored by hash" (Kernel.hash_program imported_a)
    (Kernel.hash_program imported_b);
  let stdlib_generics_path = find_up (Sys.getcwd ()) "examples/stdlib_generics.protoss" in
  let stdlib_generics = Loader.check_file stdlib_generics_path in
  let bumped, _ = Runtime.normalize_def stdlib_generics "bumped" in
  assert_equal "stdlib generic List.map" "[2, 3]" (Runtime.value_to_string bumped);
  let len, _ = Runtime.normalize_def stdlib_generics "len" in
  assert_equal "stdlib generic List.length" "2" (Runtime.value_to_string len);
  let appended, _ = Runtime.normalize_def stdlib_generics "appended" in
  assert_equal "stdlib generic List.append" "[1, 2, 3]" (Runtime.value_to_string appended);
  let filtered, _ = Runtime.normalize_def stdlib_generics "filtered" in
  assert_equal "stdlib generic List.filter" "[2]" (Runtime.value_to_string filtered);
  let reversed, _ = Runtime.normalize_def stdlib_generics "reversed" in
  assert_equal "stdlib generic List.reverse" "[3, 2, 1]" (Runtime.value_to_string reversed);
  let any_two, _ = Runtime.normalize_def stdlib_generics "anyTwo" in
  assert_equal "stdlib generic List.any" "true" (Runtime.value_to_string any_two);
  let all_two, _ = Runtime.normalize_def stdlib_generics "allTwo" in
  assert_equal "stdlib generic List.all" "false" (Runtime.value_to_string all_two);
  let member_three, _ = Runtime.normalize_def stdlib_generics "memberThree" in
  assert_equal "stdlib generic List.member" "true" (Runtime.value_to_string member_three);
  let found_three, _ = Runtime.normalize_def stdlib_generics "foundThree" in
  assert_equal "stdlib generic List.find hit" "Some 3" (Runtime.value_to_string found_three);
  let found_missing, _ = Runtime.normalize_def stdlib_generics "foundMissing" in
  assert_equal "stdlib generic List.find miss" "None unit"
    (Runtime.value_to_string found_missing);
  let label, _ = Runtime.normalize_def stdlib_generics "label" in
  assert_equal "stdlib generic Maybe.map/default" "\"known\"" (Runtime.value_to_string label);
  let maybe_has_age, _ = Runtime.normalize_def stdlib_generics "maybeHasAge" in
  assert_equal "stdlib generic Maybe.isSome" "true" (Runtime.value_to_string maybe_has_age);
  let maybe_missing_age, _ = Runtime.normalize_def stdlib_generics "maybeMissingAge" in
  assert_equal "stdlib generic Maybe.isNone" "true"
    (Runtime.value_to_string maybe_missing_age);
  let maybe_next, _ = Runtime.normalize_def stdlib_generics "maybeNext" in
  assert_equal "stdlib generic Maybe.andThen" "Some 42" (Runtime.value_to_string maybe_next);
  let maybe_pair, _ = Runtime.normalize_def stdlib_generics "maybePair" in
  assert_equal "stdlib generic Maybe.map2" "Some 42" (Runtime.value_to_string maybe_pair);
  let maybe_result, _ = Runtime.normalize_def stdlib_generics "maybeResult" in
  assert_equal "stdlib generic Maybe.toResult" "Ok 41" (Runtime.value_to_string maybe_result);
  let result_label, _ = Runtime.normalize_def stdlib_generics "resultLabel" in
  assert_equal "stdlib generic Result.map" "Ok \"ok\"" (Runtime.value_to_string result_label);
  let result_default, _ = Runtime.normalize_def stdlib_generics "resultDefault" in
  assert_equal "stdlib generic Result.withDefault" "7"
    (Runtime.value_to_string result_default);
  let result_mapped_err, _ = Runtime.normalize_def stdlib_generics "resultMappedErr" in
  assert_equal "stdlib generic Result.mapError" "Err true"
    (Runtime.value_to_string result_mapped_err);
  let result_next, _ = Runtime.normalize_def stdlib_generics "resultNext" in
  assert_equal "stdlib generic Result.andThen" "Ok 8" (Runtime.value_to_string result_next);
  let result_sum, _ = Runtime.normalize_def stdlib_generics "resultSum" in
  assert_equal "stdlib generic Result.map2" "Ok 12" (Runtime.value_to_string result_sum);
  let result_maybe, _ = Runtime.normalize_def stdlib_generics "resultMaybe" in
  assert_equal "stdlib generic Result.toMaybe" "Some 7"
    (Runtime.value_to_string result_maybe);
  let result_is_ok, _ = Runtime.normalize_def stdlib_generics "resultIsOk" in
  assert_equal "stdlib generic Result.isOk" "true" (Runtime.value_to_string result_is_ok);
  let result_is_err, _ = Runtime.normalize_def stdlib_generics "resultIsErr" in
  assert_equal "stdlib generic Result.isErr" "true" (Runtime.value_to_string result_is_err);
  let swapped, _ = Runtime.normalize_def stdlib_generics "swapped" in
  assert_equal "stdlib generic Pair.swap" "{first = 7, second = \"n\"}"
    (Runtime.value_to_string swapped);
  let assoc_age, _ = Runtime.normalize_def stdlib_generics "assocAge" in
  assert_equal "stdlib generic Assoc.get" "Some 41" (Runtime.value_to_string assoc_age);
  let assoc_has_count, _ = Runtime.normalize_def stdlib_generics "assocHasCount" in
  assert_equal "stdlib generic Assoc.contains" "true"
    (Runtime.value_to_string assoc_has_count);
  let assoc_keys, _ = Runtime.normalize_def stdlib_generics "assocKeys" in
  assert_equal "stdlib generic Assoc.keys" "[\"age\", \"count\"]"
    (Runtime.value_to_string assoc_keys);
  let assoc_values, _ = Runtime.normalize_def stdlib_generics "assocValues" in
  assert_equal "stdlib generic Assoc.values" "[41, 2]"
    (Runtime.value_to_string assoc_values);
  let set_has_two, _ = Runtime.normalize_def stdlib_generics "setHasTwo" in
  assert_equal "stdlib generic Set.contains" "true" (Runtime.value_to_string set_has_two);
  let set_union, _ = Runtime.normalize_def stdlib_generics "setUnion" in
  assert_equal "stdlib generic Set.union" "[1, 2, 3]" (Runtime.value_to_string set_union);
  let set_removed, _ = Runtime.normalize_def stdlib_generics "setRemoved" in
  assert_equal "stdlib generic Set.remove" "[1, 3]" (Runtime.value_to_string set_removed);
  let set_intersect, _ = Runtime.normalize_def stdlib_generics "setIntersect" in
  assert_equal "stdlib generic Set.intersect" "[2]"
    (Runtime.value_to_string set_intersect);
  let set_difference, _ = Runtime.normalize_def stdlib_generics "setDifference" in
  assert_equal "stdlib generic Set.difference" "[1, 3]"
    (Runtime.value_to_string set_difference);
  let json_name, _ = Runtime.normalize_def stdlib_generics "jsonName" in
  assert_equal "stdlib Json.getField hit" "Some JString \"Ada\""
    (Runtime.value_to_string json_name);
  let json_missing, _ = Runtime.normalize_def stdlib_generics "jsonMissing" in
  assert_equal "stdlib Json.getField missing" "None unit"
    (Runtime.value_to_string json_missing);
  let json_name_string, _ = Runtime.normalize_def stdlib_generics "jsonNameString" in
  assert_equal "stdlib Json.expectString" "Ok \"Ada\""
    (Runtime.value_to_string json_name_string);
  let json_age_nat, _ = Runtime.normalize_def stdlib_generics "jsonAgeNat" in
  assert_equal "stdlib Json.expectNat" "Ok 41" (Runtime.value_to_string json_age_nat);
  let json_null_is_null, _ = Runtime.normalize_def stdlib_generics "jsonNullIsNull" in
  assert_equal "stdlib Json.isNull" "true" (Runtime.value_to_string json_null_is_null);
  let json_profile_object, _ = Runtime.normalize_def stdlib_generics "jsonProfileObject" in
  assert_equal "stdlib Json.expectObject"
    "Ok [{first = \"name\", second = JString \"Ada\"}, {first = \"age\", second = JNat 41}]"
    (Runtime.value_to_string json_profile_object);
  let json_name_field, _ = Runtime.normalize_def stdlib_generics "jsonNameField" in
  assert_equal "stdlib Json.expectField" "Ok JString \"Ada\""
    (Runtime.value_to_string json_name_field);
  let json_name_via_decoder, _ =
    Runtime.normalize_def stdlib_generics "jsonNameViaDecoder"
  in
  assert_equal "stdlib Json.expectFieldAs" "Ok \"Ada\""
    (Runtime.value_to_string json_name_via_decoder);
  let json_missing_field, _ = Runtime.normalize_def stdlib_generics "jsonMissingField" in
  assert_equal "stdlib Json.expectField missing" "Err \"missing field\""
    (Runtime.value_to_string json_missing_field);
  let module_root = temp_dir "modules" in
  ensure_dir module_root;
  let module_math = Filename.concat module_root "math.protoss" in
  let module_app = Filename.concat module_root "app.protoss" in
  let module_bad = Filename.concat module_root "bad.protoss" in
  let stdlib_path = find_up (Sys.getcwd ()) "stdlib/prelude.protoss" in
  write_file module_math
    ("(module Demo.Math)\n(import " ^ Ast.quote stdlib_path
   ^ ")\n(export Number double)\n(type Number Nat)\n(def hidden Number 2)\n\
      (def double (-> Number Number) (lambda (x Number) ((Nat.mul x) hidden)))\n");
  write_file module_app
    "(import \"math.protoss\")\n(def result Demo.Math.Number (Demo.Math.double 3))\n";
  let module_checked = Loader.check_file module_app in
  let module_result, _ = Runtime.normalize_def module_checked "result" in
  assert_equal "module export with private dependency" "6" (Runtime.value_to_string module_result);
  write_file module_bad
    "(import \"math.protoss\")\n(def leak Demo.Math.Number Demo.Math.hidden)\n";
  (try
     ignore (Loader.check_file module_bad);
     fail "private module definition should not be importable"
   with Loader.Error msg -> assert_true "private module export error" (String.contains msg 'e'));
  let cycle_a = Filename.concat import_root "cycle_a.protoss" in
  let cycle_b = Filename.concat import_root "cycle_b.protoss" in
  write_file cycle_a "(import \"cycle_b.protoss\")\n(def a Nat 1)\n";
  write_file cycle_b "(import \"cycle_a.protoss\")\n(def b Nat 2)\n";
  (try
     ignore (Loader.check_file cycle_a);
     fail "import cycle should be rejected"
   with Loader.Error msg -> assert_true "import cycle message" (String.contains msg 'c'));
  (try
     ignore (check "(def bad Nat (suc 1))");
     fail "unknown name should suggest"
   with Kernel.Error msg -> assert_true "unknown name suggestion" (String.contains msg '?'));
  let bad_file = Filename.concat import_root "bad.protoss" in
  write_file bad_file "(def bad Nat true)\n";
  (try
     ignore (Loader.check_file bad_file);
     fail "loader error should be localized"
   with Loader.Error msg -> assert_true "loader error has file-ish location" (String.contains msg ':'));
  let bad_syntax_file = Filename.concat import_root "bad_syntax.protoss" in
  write_file bad_syntax_file "(def bad Nat\n  (succ 1)\n";
  (try
     ignore (Loader.check_file bad_syntax_file);
     fail "loader syntax error should be localized"
   with Loader.Error msg ->
     assert_true "loader syntax error has file line column"
       (contains_substring msg (bad_syntax_file ^ ":1:1: unterminated list")));

  expect_check_error "(def loop Nat loop)";
  expect_check_error "(def a Nat b)\n(def b Nat a)";

  (try
     ignore (check "(def bad Nat true)");
     fail "expected detailed type error"
   with Kernel.Error msg ->
     assert_true "type error has definition" (String.contains msg 'b');
     assert_true "type error has expected" (String.contains msg 'N');
     assert_true "type error has got" (String.contains msg 'B');
     assert_true "type error has expression" (String.contains msg 't'));

  let process = check "(capabilities Human.ask)\n(def askName (Process String) (Human.ask \"Name?\"))" in
  let pv, _ = Runtime.eval_entry process "askName" in
  assert_true "process should suspend"
    (match pv with Runtime.VProcessRequest { Runtime.req = Ast.AskHuman "Name?"; _ } -> true | _ -> false);
  let process_graph = Json.parse (Canonical_ir.serialize_graph process) in
  let process_capabilities = json_string_array_field "capabilities" process_graph in
  assert_equal "process graph capability list" "Human.ask" (String.concat "," process_capabilities);
  let process_descriptors = json_array_field "capabilityDescriptors" process_graph in
  assert_true "process graph capability descriptor count" (List.length process_descriptors = 1);
  let human_descriptor = List.hd process_descriptors in
  let human_capability_ref =
    match Kernel.req_capability_ref (Ast.AskHuman "") with
    | Some ref -> ref
    | None -> fail "missing Human.ask capability ref"
  in
  let human_signature_ref = Kernel.req_signature_ref (Ast.AskHuman "") in
  let clock_capability_ref =
    match Kernel.req_capability_ref Ast.ReadClock with
    | Some ref -> ref
    | None -> fail "missing Clock.read capability ref"
  in
  assert_equal "process graph capability descriptor ref" human_capability_ref
    (json_string_field "ref" human_descriptor);
  assert_equal "process graph capability refs" human_capability_ref
    (String.concat "," (json_string_array_field "capabilityRefs" process_graph));
  (try
     ignore
       (Canonical_ir.parse_graph
          (replace_once (Canonical_ir.serialize_graph process)
             ("\"capabilityRefs\": [" ^ Ast.quote human_capability_ref ^ "]")
             "\"capabilityRefs\": [\"p2:bad\"]"));
     fail "canonical graph should reject corrupt program capability refs"
   with Kernel.Error msg ->
     assert_true "canonical graph rejects corrupt program capability refs"
       (contains_substring msg "canonical graph capabilityRefs mismatch"));
  assert_equal "process graph capability descriptor name" "Human.ask"
    (json_string_field "name" human_descriptor);
  let human_requests = json_array_field "requests" human_descriptor in
  assert_true "process graph capability request count" (List.length human_requests = 1);
  let ask_request = List.hd human_requests in
  assert_equal "process graph request signature ref" human_signature_ref
    (json_string_field "ref" ask_request);
  assert_equal "process graph request tag" "AskHuman" (json_string_field "tag" ask_request);
  assert_equal "process graph response type" "String"
    (json_string_field "tag" (json_field "responseType" ask_request));
  let process_request =
    json_field "request" (json_field "term" (graph_def process_graph "askName"))
  in
  assert_equal "process request capability ref" human_capability_ref
    (json_string_field "capabilityRef" process_request);
  assert_equal "process request signature ref" human_signature_ref
    (json_string_field "requestSignatureRef" process_request);
  (try
     ignore
       (Canonical_ir.parse_graph
          (replace_once (Canonical_ir.serialize_graph process)
             "\"capabilityRef\": " "\"capabilityRefMissing\": "));
     fail "canonical graph should reject missing request capability ref"
   with Kernel.Error msg ->
     assert_true "canonical graph rejects missing request capability ref"
       (contains_substring msg "canonical graph missing field: capabilityRef"));
  (try
     ignore
       (Canonical_ir.parse_graph
          (replace_once (Canonical_ir.serialize_graph process)
             ("\"capabilityRef\": " ^ Ast.quote human_capability_ref)
             "\"capabilityRef\": \"p2:bad\""));
     fail "canonical graph should reject corrupt request capability ref"
   with Kernel.Error msg ->
     assert_true "canonical graph rejects corrupt request capability ref"
       (contains_substring msg "canonical graph request capabilityRef mismatch: AskHuman"));
  (try
     ignore
       (Canonical_ir.parse_graph
          (replace_once (Canonical_ir.serialize_graph process)
             ("\"requestSignatureRef\": " ^ Ast.quote human_signature_ref)
             "\"requestSignatureRef\": \"p2:bad\""));
     fail "canonical graph should reject corrupt request signature ref"
   with Kernel.Error msg ->
     assert_true "canonical graph rejects corrupt request signature ref"
       (contains_substring msg "canonical graph requestSignatureRef mismatch: AskHuman"));
  assert_equal "process graph def capability scope refs" human_capability_ref
    (String.concat ","
       (json_string_array_field "capabilityScopeRefs" (graph_def process_graph "askName")));
  (try
     ignore
       (Canonical_ir.parse_graph
          (replace_once (Canonical_ir.serialize_graph process)
             "\"capabilityScopeRefs\": ["
             "\"capabilityScopeRefsMissing\": ["));
     fail "canonical graph should reject missing capability scope refs"
   with Kernel.Error msg ->
     assert_true "canonical graph rejects missing capability scope refs"
       (contains_substring msg "canonical graph missing field: capabilityScopeRefs"));
  (try
     ignore
       (Canonical_ir.parse_graph
          (replace_once (Canonical_ir.serialize_graph process)
             ("\"capabilityScopeRefs\": [" ^ Ast.quote human_capability_ref ^ "]")
             "\"capabilityScopeRefs\": [\"p2:bad\"]"));
     fail "canonical graph should reject corrupt capability scope refs"
   with Kernel.Error msg ->
     assert_true "canonical graph rejects corrupt capability scope refs"
       (contains_substring msg "canonical graph capabilityScopeRefs mismatch: askName"));

  let scoped_process =
    check
      "(capabilities Human.ask Clock.read)\n\
       (def ask (Process String) (Human.ask \"Name?\"))\n\
       (def wrapped (Process String) ask)\n\
       (def pure Nat 1)"
  in
  assert_equal "direct process capability scope" "Human.ask"
    (String.concat "," (checked_def scoped_process "ask").Kernel.capabilities);
  assert_equal "transitive process capability scope" "Human.ask"
    (String.concat "," (checked_def scoped_process "wrapped").Kernel.capabilities);
  assert_equal "pure def has empty capability scope" ""
    (String.concat "," (checked_def scoped_process "pure").Kernel.capabilities);
  let scoped_graph = Json.parse (Canonical_ir.serialize_graph scoped_process) in
  assert_equal "graph def capability scope" "Human.ask"
    (String.concat "," (json_string_array_field "capabilityScope" (graph_def scoped_graph "wrapped")));
  ignore (Canonical_ir.parse_graph (Canonical_ir.serialize_graph scoped_process));
  (try
     ignore
       (Canonical_ir.parse_graph
          (replace_once (Canonical_ir.serialize_graph scoped_process)
             "\"capabilityScope\": [\"Human.ask\"]" "\"capabilityScopeMissing\": [\"Human.ask\"]"));
     fail "canonical graph should reject missing capability scope"
   with Kernel.Error msg ->
     assert_true "canonical graph rejects missing capability scope"
       (contains_substring msg "canonical graph missing field: capabilityScope"));
  (try
     ignore
       (Canonical_ir.parse_graph
          (replace_once (Canonical_ir.serialize_graph scoped_process)
             ("\"capabilityScope\": [\"Human.ask\"], \"capabilityScopeRefs\": ["
            ^ Ast.quote human_capability_ref ^ "]")
             ("\"capabilityScope\": [\"Clock.read\"], \"capabilityScopeRefs\": ["
            ^ Ast.quote clock_capability_ref ^ "]")));
     fail "canonical graph should reject corrupt capability scope"
   with Kernel.Error msg ->
     assert_true "canonical graph rejects corrupt capability scope"
       (contains_substring msg "canonical graph capabilityScope mismatch: ask"));
  let multi_cap_process =
    check
      "(capabilities Human.ask Clock.read)\n\
       (def both (Process String)\n\
       \  (bind (Clock.read) (lambda (now String) (Human.ask \"Name?\"))))"
  in
  let multi_cap_graph_json = Canonical_ir.serialize_graph multi_cap_process in
  ignore (Canonical_ir.parse_graph multi_cap_graph_json);
  let multi_cap_graph = Json.parse multi_cap_graph_json in
  assert_equal "graph multi top-level capability refs"
    (clock_capability_ref ^ "," ^ human_capability_ref)
    (String.concat "," (json_string_array_field "capabilityRefs" multi_cap_graph));
  (try
     ignore
       (Canonical_ir.parse_graph
          (replace_once multi_cap_graph_json
             "\"capabilities\": [\"Clock.read\", \"Human.ask\"]"
             "\"capabilities\": [\"Human.ask\", \"Clock.read\"]"));
     fail "canonical graph should reject unsorted program capabilities"
   with Kernel.Error msg ->
     assert_true "canonical graph rejects unsorted program capabilities"
       (contains_substring msg "canonical graph capabilities not canonical"));
  (try
     ignore
       (Canonical_ir.parse_graph
          (replace_once multi_cap_graph_json
             ("\"capabilityRefs\": [" ^ Ast.quote clock_capability_ref ^ ", "
            ^ Ast.quote human_capability_ref ^ "]")
             ("\"capabilityRefs\": [" ^ Ast.quote human_capability_ref ^ ", "
            ^ Ast.quote clock_capability_ref ^ "]")));
     fail "canonical graph should reject unsorted program capability refs"
   with Kernel.Error msg ->
     assert_true "canonical graph rejects unsorted program capability refs"
       (contains_substring msg "canonical graph capabilityRefs mismatch"));
  assert_equal "graph multi capability scope refs"
    (clock_capability_ref ^ "," ^ human_capability_ref)
    (String.concat ","
       (json_string_array_field "capabilityScopeRefs" (graph_def multi_cap_graph "both")));
  (try
     ignore
       (Canonical_ir.parse_graph
          (replace_once multi_cap_graph_json
             "\"capabilityScope\": [\"Clock.read\", \"Human.ask\"]"
             "\"capabilityScope\": [\"Human.ask\", \"Clock.read\"]"));
     fail "canonical graph should reject unsorted capability scope"
   with Kernel.Error msg ->
     assert_true "canonical graph rejects unsorted capability scope"
       (contains_substring msg "canonical graph capabilityScope not canonical: both"));
  let scoped_suspended =
    match fst (Runtime.eval_entry scoped_process "wrapped") with
    | Runtime.VProcessRequest s -> s
    | other -> fail ("expected scoped process request, got " ^ Runtime.value_to_string other)
  in
  assert_equal "runtime request capability scope is minimal" "Human.ask"
    (String.concat "," scoped_suspended.Runtime.cap_scope);
  let scoped_serialized = Runtime.serialize_suspended scoped_suspended in
  assert_true "serialized suspended keeps cap scope"
    (contains_substring scoped_serialized "(cap-scope (Human.ask))");
  let scoped_parsed = Runtime.parse_suspended scoped_serialized in
  assert_equal "parsed suspended keeps cap scope" "Human.ask"
    (String.concat "," scoped_parsed.Runtime.cap_scope);
  let scoped_graph_checked =
    Canonical_ir.checked_of_graph (Canonical_ir.serialize_graph scoped_process)
  in
  let scoped_graph_suspended =
    match fst (Runtime.eval_entry scoped_graph_checked "wrapped") with
    | Runtime.VProcessRequest s -> s
    | other -> fail ("expected graph process request, got " ^ Runtime.value_to_string other)
  in
  assert_equal "graph process request cap scope" "Human.ask"
    (String.concat "," scoped_graph_suspended.Runtime.cap_scope);

  let bind_scoped_process =
    check
      "(capabilities Human.ask Clock.read)\n\
       (def askTwice (Process String)\n\
       (bind (Human.ask \"A?\") (lambda (x String) (Human.ask \"B?\"))))"
  in
  let first_request =
    match fst (Runtime.eval_entry bind_scoped_process "askTwice") with
    | Runtime.VProcessRequest s -> s
    | other -> fail ("expected first bind request, got " ^ Runtime.value_to_string other)
  in
  assert_equal "first bind request cap scope" "Human.ask"
    (String.concat "," first_request.Runtime.cap_scope);
  let second_request =
    match
      Runtime.resume bind_scoped_process
        (Runtime.parse_suspended (Runtime.serialize_suspended first_request))
        (Runtime.response_value first_request.Runtime.req "Ada")
    with
    | Runtime.VProcessRequest s -> s
    | other -> fail ("expected second bind request, got " ^ Runtime.value_to_string other)
  in
  assert_equal "resumed bind request cap scope" "Human.ask"
    (String.concat "," second_request.Runtime.cap_scope);

  let process_resume =
    check
      "(capabilities Human.ask)\n\
       (def askName (Process String) \
       (bind (Human.ask \"Name?\") (lambda (x String) (done x))))"
  in
  let process_resume_inferred =
    check
      "(capabilities Human.ask)\n\
       (def askName (Process String) \
       (bind (Human.ask \"Name?\") (lambda x (done x))))"
  in
  assert_equal "inferred bind canonical hash" (Kernel.hash_program process_resume)
    (Kernel.hash_program process_resume_inferred);
  let suspended =
    match fst (Runtime.eval_entry process_resume "askName") with
    | Runtime.VProcessRequest s -> s
    | _ -> fail "expected suspended process"
  in
  let serialized = Runtime.serialize_suspended suspended in
  let parsed = Runtime.parse_suspended serialized in
  let resumed = Runtime.resume process_resume parsed (Runtime.response_value parsed.req "Ada") in
  assert_equal "process resume" "Done \"Ada\"" (Runtime.value_to_string resumed);
  let process_resume_graph =
    Canonical_ir.checked_of_graph (Canonical_ir.serialize_graph process_resume)
  in
  let graph_suspended =
    match fst (Runtime.eval_entry process_resume_graph "askName") with
    | Runtime.VProcessRequest s -> Runtime.parse_suspended (Runtime.serialize_suspended s)
    | other -> fail ("expected graph suspended process, got " ^ Runtime.value_to_string other)
  in
  let graph_resumed =
    Runtime.resume process_resume_graph graph_suspended
      (Runtime.response_value graph_suspended.Runtime.req "Ada")
  in
  assert_equal "graph process resume" "Done \"Ada\"" (Runtime.value_to_string graph_resumed);
  let ask_human_path = find_up (Sys.getcwd ()) "examples/ask_human.protoss" in
  let process_invariants = Invariants.check_process ask_human_path "askName" "String:Ada" in
  assert_equal "invariants process result" "Done \"Ada\""
    process_invariants.Invariants.result;
  let ledger_invariants_root = temp_dir "invariants-ledger" in
  let ledger_invariants =
    Invariants.check_ledger_process ~ledger:ledger_invariants_root ask_human_path "askName"
      "String:Ada"
  in
  assert_equal "invariants ledger result" "Done \"Ada\""
    ledger_invariants.Invariants.result;
  assert_equal "invariants ledger capability" "Human.ask"
    ledger_invariants.Invariants.capability;
  assert_equal "invariants ledger capability ref" human_capability_ref
    ledger_invariants.Invariants.capability_ref;
  assert_equal "invariants ledger request tag" "AskHuman"
    ledger_invariants.Invariants.request_tag;
  assert_equal "invariants ledger request signature ref" human_signature_ref
    ledger_invariants.Invariants.request_signature_ref;
  assert_equal "invariants ledger response type" "String"
    ledger_invariants.Invariants.response_type;
  let process_graph_dir = temp_dir "invariants-process-graph" in
  ensure_dir process_graph_dir;
  let process_graph_path = Filename.concat process_graph_dir "ask_human.graph.json" in
  write_file process_graph_path (Canonical_ir.serialize_graph (Loader.check_file ask_human_path));
  let graph_process_invariants =
    Invariants.check_graph_process process_graph_path "askName" "String:Ada"
  in
  assert_equal "invariants graph process result" process_invariants.result
    graph_process_invariants.Invariants.result;
  let graph_ledger_invariants =
    Invariants.check_graph_ledger_process ~ledger:(temp_dir "invariants-graph-ledger")
      process_graph_path "askName" "String:Ada"
  in
  assert_equal "invariants graph ledger result" process_invariants.result
    graph_ledger_invariants.Invariants.result;
  let inferred_suspended =
    match fst (Runtime.eval_entry process_resume_inferred "askName") with
    | Runtime.VProcessRequest s -> Runtime.parse_suspended (Runtime.serialize_suspended s)
    | _ -> fail "expected inferred suspended process"
  in
  let inferred_resumed =
    Runtime.resume process_resume_inferred inferred_suspended
      (Runtime.response_value inferred_suspended.Runtime.req "Ada")
  in
  assert_equal "inferred bind process resume" "Done \"Ada\""
    (Runtime.value_to_string inferred_resumed);
  (try
     ignore (Runtime.response_value parsed.req "Nat:1");
     fail "wrong typed response should be rejected"
   with Kernel.Error _ -> ());
  (try
     ignore
       (Runtime.parse_suspended
          "(protoss-runtime-v2 (suspended (request-id bad) (request (AskHuman \"x\")) \
           (continuation-id bad) (cont KDone) (cap-scope (Human.ask))))");
     fail "mismatched continuation protocol ids should be rejected"
   with Kernel.Error _ -> ());

  expect_check_error "(def askName (Process String) (Human.ask \"Name?\"))";
  expect_check_error "(capabilities Space.laser)\n(def main Nat 0)";
  expect_check_error
    "(capabilities Human.ask)\n(def bad Nat (let (p (Human.ask \"x\")) 0))";

  let store = temp_dir "patch" in
  let patch_ok =
    patch_file "protoss-add-two.json"
      "{ \"op\":\"AddDef\", \"name\":\"two\", \"deps\":[], \"type\":\"Nat\", \"expr\":[\"succ\",1] }"
  in
  let patch_audit_path store ref =
    Filename.concat (Filename.concat store "patches") (ref ^ ".patch")
  in
  let patch_latest_path store = Filename.concat (Filename.concat store "patches") "latest" in
  let patch_ok_ref = Patch.apply store patch_ok in
  assert_true "valid patch writes object" (count_objects store > 0);
  assert_true "valid patch writes audit ref" (Sys.file_exists (patch_audit_path store patch_ok_ref));
  assert_equal "valid patch latest pointer" patch_ok_ref
    (String.trim (Store.read_file (patch_latest_path store)));
  let patch_ok_audit = Store.read_file (patch_audit_path store patch_ok_ref) in
  assert_true "patch audit records format"
    (contains_substring patch_ok_audit "protoss-patch-audit-v1");
  assert_true "patch audit records source hash" (contains_substring patch_ok_audit "source-hash=p2:");
  assert_true "patch audit records program hash" (contains_substring patch_ok_audit "program-hash=p2:");
  assert_true "patch audit records operation"
    (contains_substring patch_ok_audit "op=1 kind=AddDef name=two target=two");
  assert_true "first patch audit has no previous ref"
    (contains_substring patch_ok_audit "previous-ref=none");
  let verified_patch_ok = Patch.verify_audit store in
  assert_equal "patch audit verify latest ref" patch_ok_ref verified_patch_ok.Patch.audit_ref;
  assert_equal "patch audit verify ops" "1" (string_of_int verified_patch_ok.Patch.ops);
  assert_true "patch audit verify source hash"
    (contains_substring verified_patch_ok.Patch.source_hash "p2:");
  let verified_latest_patch_ok = Patch.verify_latest_matches_store store in
  assert_equal "patch audit latest matches store" patch_ok_ref
    verified_latest_patch_ok.Patch.audit_ref;
  let inspected_patch_ok = Patch.inspect_audit ~ref:patch_ok_ref store in
  assert_true "patch audit inspect returns content"
    (contains_substring inspected_patch_ok ("Patch audit OK " ^ patch_ok_ref)
    && contains_substring inspected_patch_ok "op=1 kind=AddDef name=two");
  let chain_replace_patch =
    patch_file "protoss-chain-replace-two.json"
      "{ \"op\":\"ReplaceDef\", \"name\":\"two\", \"deps\":[], \"type\":\"Nat\", \"expr\":[\"succ\",2] }"
  in
  let chain_next_patch =
    patch_file "protoss-chain-next-two.json"
      "{ \"op\":\"ReplaceDef\", \"name\":\"two\", \"deps\":[], \"type\":\"Nat\", \"expr\":[\"succ\",3] }"
  in
  let chain_ref = Patch.apply store chain_replace_patch in
  let chain_audit = Store.read_file (patch_audit_path store chain_ref) in
  assert_true "second patch audit links previous"
    (contains_substring chain_audit ("previous-ref=" ^ patch_ok_ref));
  assert_equal "patch audit latest pointer moves" chain_ref
    (String.trim (Store.read_file (patch_latest_path store)));
  assert_equal "patch audit chain latest matches store" chain_ref
    (Patch.verify_latest_matches_store store).Patch.audit_ref;
  let stale_latest_store = temp_dir "patch-audit-stale-latest" in
  let stale_first = Patch.apply stale_latest_store patch_ok in
  let stale_second = Patch.apply stale_latest_store chain_replace_patch in
  Store.write_file_atomic (patch_latest_path stale_latest_store) (stale_first ^ "\n");
  (try
     ignore (Patch.inspect_audit stale_latest_store);
     fail "stale latest patch audit should reject store mismatch"
   with Patch.Error msg ->
     assert_true "stale latest detects store mismatch"
       (contains_substring msg "patch audit program hash mismatch"));
  assert_true "historical latest target remains inspectable by ref"
    (contains_substring (Patch.inspect_audit ~ref:stale_second stale_latest_store)
       ("Patch audit OK " ^ stale_second));
  let stale_before = snapshot stale_latest_store in
  (try
     ignore (Patch.apply stale_latest_store chain_next_patch);
     fail "patch apply should reject stale latest"
   with Patch.Error msg ->
     assert_true "patch apply rejects stale latest"
       (contains_substring msg "patch audit program hash mismatch"));
  assert_true "stale latest patch apply mutates nothing"
    (snapshot stale_latest_store = stale_before);
  let broken_chain_store = temp_dir "patch-audit-chain-broken" in
  let broken_first = Patch.apply broken_chain_store patch_ok in
  let broken_second = Patch.apply broken_chain_store chain_replace_patch in
  ignore broken_second;
  Sys.remove (patch_audit_path broken_chain_store broken_first);
  (try
     ignore (Patch.inspect_audit broken_chain_store);
     fail "broken patch audit chain should be rejected"
   with Patch.Error msg ->
     assert_true "broken patch audit chain detects missing previous"
       (contains_substring msg ("patch audit not found: " ^ broken_first)));
  let broken_before = snapshot broken_chain_store in
  (try
     ignore (Patch.apply broken_chain_store chain_next_patch);
     fail "patch apply should reject broken audit chain"
   with Patch.Error msg ->
     assert_true "patch apply rejects broken audit chain"
       (contains_substring msg ("patch audit not found: " ^ broken_first)));
  assert_true "broken audit chain patch apply mutates nothing"
    (snapshot broken_chain_store = broken_before);
  let corrupt_audit_store = temp_dir "patch-audit-corrupt" in
  let corrupt_ref = Patch.apply corrupt_audit_store patch_ok in
  let corrupt_path = patch_audit_path corrupt_audit_store corrupt_ref in
  Store.write_file_atomic corrupt_path (Store.read_file corrupt_path ^ "corrupt\n");
  (try
     ignore (Patch.inspect_audit corrupt_audit_store);
     fail "corrupt patch audit should be rejected"
   with Patch.Error msg ->
     assert_true "corrupt patch audit detects hash mismatch"
       (contains_substring msg "patch audit hash mismatch"));
  let drift_audit_store = temp_dir "patch-audit-drift" in
  let drift_ref = Patch.apply drift_audit_store patch_ok in
  Store.write_file_atomic
    (Filename.concat (Filename.concat drift_audit_store "defs") "two.protoss")
    "(def two Nat 0)\n";
  (try
     ignore (Patch.inspect_audit drift_audit_store);
     fail "latest patch audit should reject store drift"
   with Patch.Error msg ->
     assert_true "latest patch audit detects store drift"
       (contains_substring msg "patch audit program hash mismatch"));
  assert_true "historical patch audit remains inspectable by ref"
    (contains_substring (Patch.inspect_audit ~ref:drift_ref drift_audit_store)
       ("Patch audit OK " ^ drift_ref));
  let inferred_lambda_patch_store = temp_dir "patch-inferred-lambda" in
  let inferred_lambda_patch =
    patch_file "protoss-inferred-lambda-patch.json"
      "{ \"op\":\"AddDef\", \"name\":\"inc\", \"deps\":[], \
       \"type\":{\"source\":\"(-> Nat Nat)\"}, \
       \"expr\":{\"source\":\"(lambda x (succ x))\"} }"
  in
  ignore (Patch.apply inferred_lambda_patch_store inferred_lambda_patch);
  let inferred_patch_checked = Store.load_program inferred_lambda_patch_store |> Kernel.check_program in
  let inferred_patch_value, _ = Runtime.eval_entry inferred_patch_checked "inc" in
  let inferred_patch_applied = Runtime.apply inferred_patch_checked inferred_patch_value (Runtime.VNat 1) in
  assert_equal "patch inferred lambda applies" "2" (Runtime.value_to_string inferred_patch_applied);
  let inferred_let_patch_store = temp_dir "patch-inferred-let" in
  let inferred_let_patch =
    patch_file "protoss-inferred-let-patch.json"
      "{ \"op\":\"AddDef\", \"name\":\"five\", \"deps\":[], \
       \"type\":\"Nat\", \
       \"expr\":{\"source\":\"(let (inc (-> Nat Nat) (lambda x (succ x))) (inc 4))\"} }"
  in
  ignore (Patch.apply inferred_let_patch_store inferred_let_patch);
  let inferred_let_patch_checked = Store.load_program inferred_let_patch_store |> Kernel.check_program in
  let inferred_let_patch_value, _ = Runtime.normalize_def inferred_let_patch_checked "five" in
  assert_equal "patch inferred let normalizes" "5"
    (Runtime.value_to_string inferred_let_patch_value);
  let inferred_list_patch_store = temp_dir "patch-inferred-list" in
  let inferred_list_patch =
    patch_file "protoss-inferred-list-patch.json"
      "{ \"op\":\"AddDef\", \"name\":\"xs\", \"deps\":[], \
       \"type\":{\"source\":\"(List Nat)\"}, \
       \"expr\":{\"source\":\"(Cons 1 (Cons 2 Nil))\"} }"
  in
  ignore (Patch.apply inferred_list_patch_store inferred_list_patch);
  let inferred_list_patch_checked = Store.load_program inferred_list_patch_store |> Kernel.check_program in
  let inferred_list_patch_value, _ = Runtime.normalize_def inferred_list_patch_checked "xs" in
  assert_equal "patch inferred list normalizes" "[1, 2]"
    (Runtime.value_to_string inferred_list_patch_value);
  let unit_branch_patch_store = temp_dir "patch-unit-branch" in
  let unit_branch_patch =
    patch_file "protoss-unit-branch-patch.json"
      "{ \"ops\": [\
       { \"op\":\"AddDef\", \"name\":\"value\", \"deps\":[], \
       \"type\":{\"source\":\"(Variant (None Unit) (Some Nat))\"}, \
       \"expr\":{\"source\":\"(variant (Variant (None Unit) (Some Nat)) Some 4)\"} },\
       { \"op\":\"AddDef\", \"name\":\"out\", \"deps\":[\"value\"], \
       \"type\":\"Nat\", \
       \"expr\":{\"source\":\"(case value (None 0) (Some n n))\"} }\
       ] }"
  in
  ignore (Patch.apply unit_branch_patch_store unit_branch_patch);
  let unit_branch_patch_checked = Store.load_program unit_branch_patch_store |> Kernel.check_program in
  let unit_branch_patch_value, _ = Runtime.normalize_def unit_branch_patch_checked "out" in
  assert_equal "patch unit branch shorthand normalizes" "4"
    (Runtime.value_to_string unit_branch_patch_value);
  let before = count_objects store in
  let ledger = temp_dir "patch-ledger" in
  let _, patch_suspended, patch_request_id, patch_continuation_id =
    ledger_suspension (Ast.AskHuman "x") [ "Human.ask" ]
  in
  let _ =
    Ledger.record_request ledger Ledger.initial_world (Ast.AskHuman "x") patch_suspended
      patch_request_id patch_continuation_id [ "Human.ask" ]
  in
  let ledger_before = count_files ledger in
  let malformed_json_patch =
    patch_file "protoss-malformed-json.json"
      "{\n\
       \  \"op\":\"AddDef\"\n\
       \  \"name\":\"bad\"\n\
       }\n"
  in
  let malformed_json_before = snapshot store in
  (try
     ignore (Patch.apply store malformed_json_patch);
     fail "malformed JSON patch should be rejected"
   with Patch.Error msg ->
     assert_true "malformed JSON patch has file location"
       (contains_substring msg (malformed_json_patch ^ ":3:3:"));
     assert_true "malformed JSON patch names JSON parse"
       (contains_substring msg "invalid JSON patch"));
  assert_true "malformed JSON patch must not modify store"
    (snapshot store = malformed_json_before);
  let missing_deps_patch =
    patch_file "protoss-missing-deps.json"
      "{ \"op\":\"AddDef\", \"name\":\"noDeps\", \"type\":\"Nat\", \"expr\":0 }"
  in
  let missing_deps_before = snapshot store in
  (try
     ignore (Patch.apply store missing_deps_patch);
     fail "missing deps patch should be rejected"
   with Patch.Error msg ->
     assert_true "missing deps patch has file context"
       (contains_substring msg (missing_deps_patch ^ ": patch op #1 AddDef noDeps field deps"));
     assert_true "missing deps patch names field"
       (contains_substring msg "patch missing field: deps"));
  assert_true "missing deps patch must not modify store"
    (snapshot store = missing_deps_before);
  let patch_bad =
    patch_file "protoss-bad.json"
      "{ \"op\":\"AddDef\", \"name\":\"bad\", \"deps\":[], \"type\":\"Nat\", \"expr\":true }"
  in
  (try
     let _ = Patch.apply store patch_bad in
     fail "invalid patch should be rejected"
   with Patch.Error msg ->
     assert_true "invalid patch includes file path" (contains_substring msg patch_bad);
     assert_true "invalid patch names patch op"
       (contains_substring msg "patch op #1 AddDef bad");
     assert_true "invalid patch keeps kernel definition context"
       (contains_substring msg "definition bad"));
  assert_true "invalid patch must not modify store" (count_objects store = before);
  assert_true "invalid patch must not modify ledger" (count_files ledger = ledger_before);

  let patch_bad_expr_shape =
    patch_file "protoss-bad-expr-shape.json"
      "{ \"op\":\"AddDef\", \"name\":\"badShape\", \"deps\":[], \"type\":\"Nat\", \
       \"expr\":{\"unknown\":1} }"
  in
  let bad_expr_shape_before = snapshot store in
  (try
     ignore (Patch.apply store patch_bad_expr_shape);
     fail "invalid structural patch expr should be rejected"
   with Patch.Error msg ->
     assert_true "invalid patch expr has field context"
       (contains_substring msg "patch op #1 AddDef badShape field expr"));
  assert_true "invalid structural expr patch must not modify store"
    (snapshot store = bad_expr_shape_before);

  let unknown_cap_patch =
    patch_file "protoss-unknown-cap.json"
      "{ \"op\":\"AddDef\", \"name\":\"capBad\", \"deps\":[], \"capabilities\":[\"Space.laser\"], \
       \"type\":\"Nat\", \"expr\":0 }"
  in
  let unknown_cap_before = snapshot store in
  (try
     let _ = Patch.apply store unknown_cap_patch in
     fail "unknown capability patch should be rejected"
   with Patch.Error msg ->
     assert_true "unknown capability patch has capability field context"
       (contains_substring msg "patch op #1 AddDef capBad field capabilities");
     assert_true "unknown capability patch names capability"
       (contains_substring msg "Space.laser"));
  assert_true "unknown capability patch must not modify store" (snapshot store = unknown_cap_before);

  let cap_patch_store = temp_dir "patch-cap-scope" in
  let cap_scope_path name =
    Filename.concat (Filename.concat cap_patch_store "capability-scopes") (name ^ ".capabilities")
  in
  let add_scoped =
    patch_file "protoss-add-scoped.json"
      "{ \"op\":\"AddDef\", \"name\":\"askName\", \"deps\":[], \
       \"capabilities\":[\"Human.ask\"], \"type\":{\"source\":\"(Process String)\"}, \
       \"expr\":{\"source\":\"(Human.ask \\\"Name?\\\")\"} }"
  in
  ignore (Patch.apply cap_patch_store add_scoped);
  assert_equal "patch AddDef writes capability scope" "Human.ask"
    (String.trim (Store.read_file (cap_scope_path "askName")));
  let cap_patch_graph =
    Json.parse (Store.read_file (Filename.concat cap_patch_store "program.graph.json"))
  in
  assert_equal "patch graph writes capability scope" "Human.ask"
    (String.concat "," (json_string_array_field "capabilityScope" (graph_def cap_patch_graph "askName")));
  let replace_scoped =
    patch_file "protoss-replace-scoped.json"
      "{ \"op\":\"ReplaceDef\", \"name\":\"askName\", \"deps\":[], \
       \"type\":{\"source\":\"(Process String)\"}, \
       \"expr\":{\"source\":\"(done \\\"Ada\\\")\"} }"
  in
  ignore (Patch.apply cap_patch_store replace_scoped);
  assert_equal "patch ReplaceDef refreshes capability scope" ""
    (String.trim (Store.read_file (cap_scope_path "askName")));
  let delete_scoped =
    patch_file "protoss-delete-scoped.json"
      "{ \"op\":\"DeleteDef\", \"name\":\"askName\", \"deps\":[] }"
  in
  ignore (Patch.apply cap_patch_store delete_scoped);
  assert_true "patch DeleteDef removes capability scope"
    (not (Sys.file_exists (cap_scope_path "askName")));

  let duplicate_before = count_files store in
  (try
     let _ = Patch.apply store patch_ok in
     fail "duplicate AddDef should be rejected"
   with Patch.Error msg ->
     assert_true "duplicate AddDef has patch context"
       (contains_substring msg "patch op #1 AddDef two");
     assert_true "duplicate AddDef names conflict"
       (contains_substring msg "AddDef target already exists"));
  assert_true "conflicting AddDef must not modify store" (count_files store = duplicate_before);

  let replace =
    patch_file "protoss-replace-two.json"
      "{ \"op\":\"ReplaceDef\", \"name\":\"two\", \"deps\":[], \"type\":\"Nat\", \"expr\":[\"succ\",2] }"
  in
  let check_snapshot = snapshot store in
  let checked_patch = Patch.check store replace in
  assert_true "patch check returns target" (Patch.describe_checked checked_patch <> "");
  assert_true "patch check must not mutate store" (snapshot store = check_snapshot);
  let _ = Patch.apply store replace in
  let replaced = Store.load_program store |> Kernel.check_program in
  let two, _ = Runtime.normalize_def replaced "two" in
  assert_equal "ReplaceDef accepted" "3" (Runtime.value_to_string two);

  let missing_replace =
    patch_file "protoss-replace-missing.json"
      "{ \"op\":\"ReplaceDef\", \"name\":\"missing\", \"deps\":[], \"type\":\"Nat\", \"expr\":0 }"
  in
  (try
     let _ = Patch.apply store missing_replace in
     fail "missing ReplaceDef should be rejected"
   with Patch.Error msg ->
     assert_true "missing ReplaceDef has patch context"
       (contains_substring msg "patch op #1 ReplaceDef missing"));

  let dep_bad =
    patch_file "protoss-dep-bad.json"
      "{ \"op\":\"AddDef\", \"name\":\"three\", \"deps\":[], \"type\":\"Nat\", \"expr\":[\"succ\",\"two\"] }"
  in
  (try
     let _ = Patch.apply store dep_bad in
     fail "dependency mismatch should be rejected"
   with Patch.Error msg ->
     assert_true "dependency mismatch has patch context"
       (contains_substring msg "patch op #1 AddDef three");
     assert_true "dependency mismatch names mismatch"
       (contains_substring msg "dependency mismatch"));

  let protected_delete =
    patch_file "protoss-add-three.json"
      "{ \"op\":\"AddDef\", \"name\":\"three\", \"deps\":[\"two\"], \"type\":\"Nat\", \"expr\":[\"succ\",\"two\"] }"
  in
  let _ = Patch.apply store protected_delete in
  let delete_two =
    patch_file "protoss-delete-two.json" "{ \"op\":\"DeleteDef\", \"name\":\"two\", \"deps\":[] }"
  in
  (try
     let _ = Patch.apply store delete_two in
     fail "DeleteDef with dependents should be rejected"
   with Patch.Error _ -> ());

  let rename_store = temp_dir "rename" in
  let _ = Patch.apply rename_store patch_ok in
  let rename_two =
    patch_file "protoss-rename-two.json"
      "{ \"op\":\"RenameDef\", \"name\":\"two\", \"newName\":\"dos\", \"deps\":[] }"
  in
  let before_rename = Store.load_program rename_store |> Kernel.check_program in
  let before_id =
    (List.hd before_rename.defs : Kernel.checked_def).def_id
  in
  let _ = Patch.apply rename_store rename_two in
  let after_rename = Store.load_program rename_store |> Kernel.check_program in
  let after_id =
    (List.hd after_rename.defs : Kernel.checked_def).def_id
  in
  assert_equal "RenameDef keeps canonical body hash" before_id after_id;

  let batch_store = temp_dir "batch" in
  let _ = Patch.apply batch_store patch_ok in
  let batch_valid =
    patch_file "protoss-batch-valid.json"
      "{ \"ops\": [\n\
       \  { \"op\":\"ReplaceDef\", \"name\":\"two\", \"deps\":[], \"type\":\"Nat\", \"expr\":[\"succ\",2] },\n\
       \  { \"op\":\"AddDef\", \"name\":\"three\", \"deps\":[\"two\"], \"type\":\"Nat\", \"expr\":[\"succ\",\"two\"] }\n\
       ] }"
  in
  let batch_check_snapshot = snapshot batch_store in
  ignore (Patch.check batch_store batch_valid);
  assert_true "batch check must not mutate store" (snapshot batch_store = batch_check_snapshot);
  let _ = Patch.apply batch_store batch_valid in
  let batch_checked = Store.load_program batch_store |> Kernel.check_program in
  let batch_three, _ = Runtime.normalize_def batch_checked "three" in
  assert_equal "patch batch accepted" "4" (Runtime.value_to_string batch_three);

  let invalid_batch =
    patch_file "protoss-batch-invalid.json"
      "{ \"ops\": [\n\
       \  { \"op\":\"AddDef\", \"name\":\"four\", \"deps\":[\"three\"], \"type\":\"Nat\", \"expr\":[\"succ\",\"three\"] },\n\
       \  { \"op\":\"AddDef\", \"name\":\"badBatch\", \"deps\":[], \"type\":\"Nat\", \"expr\":true }\n\
       ] }"
  in
  let invalid_before = snapshot batch_store in
  (try
     ignore (Patch.apply batch_store invalid_batch);
     fail "invalid batch should be rejected"
   with Patch.Error msg ->
     assert_true "invalid batch points to second op"
       (contains_substring msg "patch op #2 AddDef badBatch"));
  assert_true "invalid batch must not mutate store" (snapshot batch_store = invalid_before);

  let conflict_batch =
    patch_file "protoss-batch-conflict.json"
      "{ \"ops\": [\n\
       \  { \"op\":\"AddDef\", \"name\":\"dupe\", \"deps\":[], \"type\":\"Nat\", \"expr\":1 },\n\
       \  { \"op\":\"AddDef\", \"name\":\"dupe\", \"deps\":[], \"type\":\"Nat\", \"expr\":2 }\n\
       ] }"
  in
  let conflict_before = snapshot batch_store in
  (try
     ignore (Patch.apply batch_store conflict_batch);
     fail "conflicting batch should be rejected"
   with Patch.Error msg ->
     assert_true "conflicting batch points to second op"
       (contains_substring msg "patch op #2 AddDef dupe"));
  assert_true "conflicting batch must not mutate store" (snapshot batch_store = conflict_before);

  let interop_batch =
    patch_file "protoss-batch-rename-replace.json"
      "{ \"ops\": [\n\
       \  { \"op\":\"RenameDef\", \"name\":\"two\", \"newName\":\"dos\", \"deps\":[] },\n\
       \  { \"op\":\"ReplaceDef\", \"name\":\"three\", \"deps\":[\"dos\"], \"type\":\"Nat\", \"expr\":[\"succ\",\"dos\"] }\n\
       ] }"
  in
  let _ = Patch.apply batch_store interop_batch in
  let interop_checked = Store.load_program batch_store |> Kernel.check_program in
  let interop_three, _ = Runtime.normalize_def interop_checked "three" in
  assert_equal "patch batch rename/replace interop" "4" (Runtime.value_to_string interop_three);
  let delete_interop =
    patch_file "protoss-batch-delete-interop.json"
      "{ \"ops\": [\n\
       \  { \"op\":\"DeleteDef\", \"name\":\"three\", \"deps\":[\"dos\"] },\n\
       \  { \"op\":\"DeleteDef\", \"name\":\"dos\", \"deps\":[] }\n\
       ] }"
  in
  let _ = Patch.apply batch_store delete_interop in
  assert_true "patch batch delete interop" ((Store.load_program batch_store).defs = []);

  let objects = Store.list_objects store in
  let first_object = List.hd objects in
  assert_true "store get" (String.length (Store.get_object store first_object) > 0);
  let objects_count, defs_count, canonical_count = Store.stats store in
  assert_true "store stats objects" (objects_count > 0);
  assert_true "store stats defs" (defs_count > 0);
  assert_true "store stats canonical" (canonical_count > 0);

  let store_a = temp_dir "store-a" and store_b = temp_dir "store-b" in
  let patch_ref_a = Patch.apply store_a patch_ok in
  let patch_ref_b = Patch.apply store_b patch_ok in
  assert_equal "deterministic patch audit ref" patch_ref_a patch_ref_b;
  assert_equal "deterministic patch audit content"
    (Store.read_file (patch_audit_path store_a patch_ref_a))
    (Store.read_file (patch_audit_path store_b patch_ref_b));
  assert_equal "deterministic store objects"
    (String.concat "," (Store.list_objects store_a))
    (String.concat "," (Store.list_objects store_b));

  let dedupe_store = temp_dir "dedupe" in
  let h1 = Store.put_object dedupe_store "test" "same" in
  let h2 = Store.put_object dedupe_store "test" "same" in
  assert_equal "store dedupe hash" h1 h2;
  assert_true "store dedupe object count" (List.length (Store.list_objects dedupe_store) = 1);

  let project_init_root = temp_dir "project-init" in
  ignore (Workspace.init project_init_root);
  let init_manifest = Workspace.parse_manifest project_init_root in
  Workspace.check_project init_manifest;
  let init_build = Workspace.build init_manifest in
  assert_true "project init build writes store" (Sys.file_exists init_build.Workspace.store);
  let init_dot_before_locked = snapshot (Filename.concat project_init_root ".protoss") in
  (try
     ignore (Workspace.build_locked init_manifest);
     fail "locked build should require a lockfile"
   with Workspace.Error _ -> ());
  assert_true "locked build missing lock leaves .protoss untouched"
    (init_dot_before_locked = snapshot (Filename.concat project_init_root ".protoss"));
  let bad_project_root = temp_dir "project-bad-syntax" in
  ignore (Workspace.init bad_project_root);
  let bad_project_file = Filename.concat bad_project_root "src/main.protoss" in
  write_file bad_project_file "(def main Nat\n  (succ 1)\n";
  let bad_project_manifest = Workspace.parse_manifest bad_project_root in
  (try
     ignore (Workspace.build bad_project_manifest);
     fail "project syntax error should be localized"
   with Workspace.Error msg ->
     assert_true "project syntax error has file line column"
       (contains_substring msg (bad_project_file ^ ":1:1: unterminated list")));

  let stdlib_path = find_up (Sys.getcwd ()) "stdlib/prelude.protoss" in
  let make_workspace name base_value bound =
    let root = temp_dir name in
    ensure_dir root;
    ensure_dir (Filename.concat root "src");
    write_file (Filename.concat root "protoss.toml")
      ("name = \"" ^ name ^ "\"\nversion = \"0.4.0\"\nentrypoints = [\"src/app.protoss\"]\nstdlib = \""
     ^ stdlib_path
      ^ "\"\nsource_dirs = [\"src\"]\nstore_dir = \".protoss/store\"\ncache_dir = \".protoss/cache\"\ncapabilities = [\"Human.ask\"]\n");
    write_file (Filename.concat root "src/math.protoss")
      ("(def base Nat " ^ string_of_int base_value
     ^ ")\n(def total Nat ((Nat.add base) 40))\n");
	    write_file (Filename.concat root "src/app.protoss")
	      ("(import \"math.protoss\")\n\
	        (record PublicBox (value Nat))\n\
	        (def numbers (List Nat) (Cons Nat 1 (Cons Nat 2 (Nil Nat))))\n\
	        (def bumped (List Nat) ((List.mapNat numbers) (lambda ("
	      ^ bound ^ " Nat) (succ " ^ bound
	      ^ "))))\n(def appMain Nat ((Nat.add total) base))\n\
	        (def askName (Process String) (Human.ask \"Name?\"))\n");
    root
  in
  let ws_a = make_workspace "workspace-a" 2 "x" in
  let manifest_a = Workspace.parse_manifest ws_a in
  Workspace.check_project manifest_a;
  let build_a = Workspace.build manifest_a in
  assert_true "project build parsed sources" (build_a.Workspace.stats.Workspace.parsed > 0);
  assert_true "project build normalized defs" (build_a.Workspace.stats.Workspace.normalized > 0);
  assert_true "project store list" (String.contains (Workspace.list_store build_a.store) 'a');
  assert_true "project store get" (String.length (Workspace.get_store build_a.store "appMain") > 0);
  assert_equal "project store deps" "Nat.add,base,total"
    (String.concat "," (Workspace.read_deps build_a.store "appMain"));
  assert_true "project store roots" (String.length (Workspace.roots_store build_a.store) > 0);
  assert_equal "project audit" "Audit OK\n" (Workspace.audit manifest_a);
  let patch_audit_ws = make_workspace "workspace-patch-audit" 5 "p" in
  let patch_audit_manifest = Workspace.parse_manifest patch_audit_ws in
  let patch_audit_build = Workspace.build patch_audit_manifest in
  let patch_audit_project_patch =
    patch_file "protoss-project-patch-audit.json"
      "{ \"op\":\"AddDef\", \"name\":\"auditExtra\", \"deps\":[], \"type\":\"Nat\", \"expr\":7 }"
  in
  ignore (Patch.apply patch_audit_build.Workspace.store patch_audit_project_patch);
  assert_equal "project audit verifies patch latest" "Audit OK\n"
    (Workspace.audit patch_audit_manifest);
  Store.write_file_atomic
    (Filename.concat (Filename.concat patch_audit_build.store "defs") "auditExtra.protoss")
    "(def auditExtra Nat 0)\n";
  (try
     ignore (Workspace.audit patch_audit_manifest);
     fail "project audit should reject patch audit drift"
   with Workspace.Error msg ->
     assert_true "project audit reports patch audit drift"
       (contains_substring msg "patch audit invalid"
       && contains_substring msg "patch audit program hash mismatch"));
  let loaded_store_program = Store.load_program build_a.store in
  assert_true "project store preserves recursive Json alias"
    (List.exists
       (fun (a : Ast.type_alias) -> String.equal a.type_name "Json")
       loaded_store_program.type_aliases);
  ignore (Kernel.check_program loaded_store_program);
  let store_graph_path = Filename.concat build_a.store "program.graph.json" in
  assert_true "project store canonical graph" (Sys.file_exists store_graph_path);
  let store_graph = Json.parse (Store.read_file store_graph_path) in
  assert_equal "project store graph version" Kernel.canonical_graph_version
    (json_string_field "version" store_graph);
  assert_equal "project store graph hash algorithm" Kernel.hash_algorithm
    (json_string_field "hashAlgorithm" store_graph);
  assert_equal "project store graph hash prefix" Kernel.hash_prefix
    (json_string_field "hashPrefix" store_graph);
  assert_equal "project store graph hash" (Kernel.hash_program build_a.Workspace.checked)
    (json_string_field "programHash" store_graph);
  assert_equal "project store graph content hash"
    (Kernel.checked_to_graph_content_hash build_a.Workspace.checked)
    (json_string_field "graphHash" store_graph);
  let store_graph_hash = json_string_field "graphHash" store_graph in
  let store_graph_object_path = Store.graph_path build_a.store store_graph_hash in
  assert_true "project store graph object" (Sys.file_exists store_graph_object_path);
  assert_equal "project store graph object content" (Store.read_file store_graph_path)
    (Store.read_file store_graph_object_path);
  assert_true "project store graphs lists graph hash"
    (contains_substring (Workspace.graphs_store build_a.store) store_graph_hash);
  assert_equal "project store graph by hash" (Store.read_file store_graph_path)
    (Workspace.graph_store build_a.store store_graph_hash);
  let store_graph_checked = Workspace.checked_store_graph build_a.store store_graph_hash in
  assert_equal "project checked store graph hash" (Kernel.hash_program build_a.Workspace.checked)
    (Kernel.hash_program store_graph_checked);
  let store_graph_value, _ = Runtime.eval_entry store_graph_checked "appMain" in
  assert_equal "project checked store graph eval" "44" (Runtime.value_to_string store_graph_value);
  let store_graph_dot = Workspace.store_graph_dot build_a.store store_graph_hash in
  assert_true "project store graph dot header"
    (contains_substring store_graph_dot "digraph protoss");
  assert_true "project store graph dot deps"
    (contains_substring store_graph_dot "\"base\" -> \"appMain\"");
  let graph_hash_mismatch_store = temp_dir "workspace-graph-hash-mismatch-store" in
  copy_tree build_a.store graph_hash_mismatch_store;
  Store.write_file_atomic
    (Store.graph_path graph_hash_mismatch_store "p2:bad")
    (Store.read_file store_graph_path);
  (try
     ignore (Workspace.graph_store graph_hash_mismatch_store "p2:bad");
     fail "store graph should reject content stored under wrong hash"
   with Workspace.Error msg ->
     assert_true "store graph reports graph hash mismatch"
       (contains_substring msg "stored canonical graph hash mismatch"));
  assert_equal "project store graph exact" (Kernel.checked_to_graph_json build_a.Workspace.checked)
    (Store.read_file store_graph_path);
  let capability_scope_file name =
    Filename.concat (Filename.concat build_a.store "capability-scopes") (name ^ ".capabilities")
  in
  assert_equal "project store process capability scope" "Human.ask"
    (String.trim (Store.read_file (capability_scope_file "askName")));
  assert_equal "project store pure capability scope" ""
    (String.trim (Store.read_file (capability_scope_file "appMain")));
  let lock_path, lock_hash = Workspace.write_lock manifest_a in
  assert_true "project lock writes file" (Sys.file_exists lock_path);
  let lock_before = Store.read_file lock_path in
  assert_true "project lock records version" (contains_substring lock_before "protoss-lock-v1");
  assert_true "project lock records hash algorithm"
    (contains_substring lock_before "(hash-algorithm \"sha256\")");
  assert_true "project lock records hash prefix"
    (contains_substring lock_before "(hash-prefix \"p2:\")");
  assert_true "project lock records program hash" (contains_substring lock_before build_a.build_id);
  assert_equal "project lock records canonical graph hash" (json_string_field "graphHash" store_graph)
    (sexp_atom_field "program-graph-hash" lock_before);
  assert_true "project lock records source units" (contains_substring lock_before "(source-hash p2:");
  assert_equal "project lock check hash" lock_hash (Workspace.check_lock manifest_a);
  let lock_path_again, lock_hash_again = Workspace.write_lock manifest_a in
  assert_equal "project lock deterministic path" lock_path lock_path_again;
  assert_equal "project lock deterministic hash" lock_hash lock_hash_again;
  assert_equal "project lock deterministic content" lock_before (Store.read_file lock_path_again);
  let package_a = Workspace.write_package manifest_a in
  assert_true "project package writes file" (Sys.file_exists package_a.Workspace.package_path);
  assert_equal "project package records build" build_a.build_id package_a.build_id;
  assert_equal "project package records lock hash" lock_hash package_a.lock_hash;
  assert_equal "project package records canonical graph hash" (json_string_field "graphHash" store_graph)
    (sexp_atom_field "program-graph-hash" (Store.read_file package_a.package_path));
  assert_equal "project package current pointer" package_a.package_ref
    (String.trim (Store.read_file (Workspace.package_current_path manifest_a)));
  assert_true "project package writes interface artifact"
    (Sys.file_exists package_a.Workspace.interface_path);
  assert_equal "project package current interface pointer" package_a.interface_ref
    (String.trim (Store.read_file (Workspace.package_interface_current_path manifest_a)));
  let stored_package_interface_json = Store.read_file package_a.interface_path in
  assert_equal "project package interface artifact hash" package_a.interface_ref
    (Kernel.hash_string stored_package_interface_json);
  let stored_package_interface_obj = Json.parse stored_package_interface_json in
  assert_equal "project package interface artifact package ref" package_a.package_ref
    (json_string_field "packageRef" stored_package_interface_obj);
  assert_equal "project package interface artifact contract hash"
    package_a.interface_contract_hash
    (json_string_field "contractHash" stored_package_interface_obj);
  let package_content = Store.read_file package_a.package_path in
  assert_true "project package records version"
    (contains_substring package_content "protoss-package-v1");
  assert_true "project package records hash algorithm"
    (contains_substring package_content "(hash-algorithm \"sha256\")");
  assert_true "project package records hash prefix"
    (contains_substring package_content "(hash-prefix \"p2:\")");
  assert_true "project package records program hash"
    (contains_substring package_content build_a.build_id);
  assert_true "project package records lock hash in content"
    (contains_substring package_content lock_hash);
  assert_true "project package records interface hash"
    (contains_substring package_content "(interface-hash p2:");
  assert_true "project package records public interface"
    (contains_substring package_content "(interface ");
  let interface_hash = sexp_atom_field "interface-hash" package_content in
  assert_true "project package public interface records capability scope"
    (contains_substring package_content "(capability-scope \"Human.ask\")");
  assert_true "project package public interface records canonical types"
    (contains_substring package_content "(type-canonical ");
  assert_true "project package records recursive Json type"
    (contains_substring package_content "(name \"Json\")");
  let package_checked = Workspace.check_package manifest_a in
  assert_equal "project package check ref" package_a.package_ref package_checked.package_ref;
  assert_equal "project package check lock" lock_hash package_checked.lock_hash;
  assert_equal "project package check interface ref" package_a.interface_ref
    package_checked.interface_ref;
  assert_equal "project package check interface path" package_a.interface_path
    package_checked.interface_path;
  assert_equal "project package check interface contract" package_a.interface_contract_hash
    package_checked.interface_contract_hash;
  let package_interface_text = Workspace.package_interface_text manifest_a in
  assert_true "project package interface prints ref"
    (contains_substring package_interface_text ("package_ref=" ^ package_a.package_ref));
  assert_true "project package interface prints public def"
    (contains_substring package_interface_text "export def appMain");
  assert_true "project package interface prints capability scope"
    (contains_substring package_interface_text "export def askName"
    && contains_substring package_interface_text "capabilities=Human.ask");
  assert_true "project package interface prints canonical def type"
    (contains_substring package_interface_text "export def askName type=(Process String)");
  assert_true "project package interface prints public type export"
    (contains_substring package_interface_text "export type PublicBox");
  let package_interface_json = Workspace.package_interface_json manifest_a in
  assert_equal "project package interface artifact content" package_interface_json
    stored_package_interface_json;
  let package_interface_obj = Json.parse package_interface_json in
  assert_equal "project package interface json format" "protoss-package-interface-v1"
    (json_string_field "format" package_interface_obj);
  assert_equal "project package interface json package ref" package_a.package_ref
    (json_string_field "packageRef" package_interface_obj);
  assert_equal "project package interface json hash" interface_hash
    (json_string_field "interfaceHash" package_interface_obj);
  let package_interface_contract_hash = json_string_field "contractHash" package_interface_obj in
  assert_true "project package interface json contract hash"
    (String.length package_interface_contract_hash > 3);
  assert_equal "project package interface json capability list" "Human.ask"
    (String.concat "," (json_string_array_field "capabilities" package_interface_obj));
  let package_capability_descriptors =
    json_array_field "capabilityDescriptors" package_interface_obj
  in
  assert_true "project package interface json capability descriptor count"
    (List.length package_capability_descriptors = 1);
  let package_capability_descriptor = List.hd package_capability_descriptors in
  assert_equal "project package interface json capability descriptor ref"
    human_capability_ref
    (json_string_field "ref" package_capability_descriptor);
  assert_equal "project package interface json capability descriptor name" "Human.ask"
    (json_string_field "name" package_capability_descriptor);
  let package_capability_requests = json_array_field "requests" package_capability_descriptor in
  assert_equal "project package interface json capability request ref" human_signature_ref
    (json_string_field "ref" (List.hd package_capability_requests));
  assert_equal "project package interface json capability request tag" "AskHuman"
    (json_string_field "tag" (List.hd package_capability_requests));
  let package_interface_exports = json_array_field "exports" package_interface_obj in
  let ask_export =
    match
      package_interface_exports
      |> List.find_opt (fun item -> String.equal "askName" (json_string_field "name" item))
    with
    | Some item -> item
    | None -> fail "missing askName interface export"
  in
  assert_equal "project package interface json capability scope" "Human.ask"
    (String.concat "," (json_string_array_field "capabilities" ask_export));
  assert_equal "project package interface json canonical def type" "(Process String)"
    (json_string_field "typeCanonical" ask_export);
  assert_equal "project package interface json def type hash matches canonical"
    (Kernel.hash_string (json_string_field "typeCanonical" ask_export))
    (json_string_field "typeHash" ask_export);
  let public_box_export =
    match
      package_interface_exports
      |> List.find_opt (fun item -> String.equal "PublicBox" (json_string_field "name" item))
    with
    | Some item -> item
    | None -> fail "missing PublicBox interface export"
  in
  assert_equal "project package interface json type export kind" "type"
    (json_string_field "kind" public_box_export);
  assert_equal "project package interface json canonical type body" "(Record (value Nat))"
    (json_string_field "typeCanonical" public_box_export);
  assert_equal "project package interface json type hash matches canonical"
    (Kernel.hash_string (json_string_field "typeCanonical" public_box_export))
    (json_string_field "typeHash" public_box_export);
  assert_equal "project package interface json deterministic" package_interface_json
    (Workspace.package_interface_json manifest_a);
  let package_interface_file = Filename.concat ws_a "interface.json" in
  write_file package_interface_file package_interface_json;
  let package_interface_check = Workspace.check_package_interface_contract manifest_a package_interface_file in
  assert_true "project package interface check accepts saved contract"
    (contains_substring package_interface_check "PackageInterfaceCheck OK");
  assert_true "project package interface check prints contract hash"
    (contains_substring package_interface_check
       ("contract_hash=" ^ package_interface_contract_hash));
  let bad_package_interface_file = Filename.concat ws_a "interface-bad.json" in
  write_file bad_package_interface_file
    (replace_once package_interface_json package_interface_contract_hash "p2:bad-contract");
  (try
     ignore (Workspace.check_package_interface_contract manifest_a bad_package_interface_file);
     fail "package interface contract check should reject mismatched contract hash"
   with Workspace.Error _ -> ());
  let bad_package_capabilities_file = Filename.concat ws_a "interface-bad-capabilities.json" in
  write_file bad_package_capabilities_file
    (replace_once package_interface_json "\"AskHuman\"" "\"ReadClock\"");
  (try
     ignore (Workspace.check_package_interface_contract manifest_a bad_package_capabilities_file);
     fail "package interface contract check should reject corrupt capability descriptors"
   with Workspace.Error _ -> ());
  let package_invariants = Invariants.check_package ws_a in
  assert_equal "package invariant interface ref" package_a.interface_ref
    package_invariants.Invariants.interface_ref;
  assert_equal "package invariant interface hash" interface_hash
    package_invariants.Invariants.interface_hash;
  assert_equal "package invariant interface contract hash" package_interface_contract_hash
    package_invariants.Invariants.interface_contract_hash;
  assert_equal "package invariant interface capability count" "1"
    (string_of_int package_invariants.Invariants.interface_capabilities);
  assert_true "package invariant counts interface exports"
    (package_invariants.Invariants.interface_exports > 0);
  assert_equal "package invariant validates all interface type hashes"
    (string_of_int package_invariants.Invariants.interface_exports)
    (string_of_int package_invariants.Invariants.interface_type_hashes);
  assert_equal "project package audit" "Audit OK\n" (Workspace.audit manifest_a);
  let package_again = Workspace.write_package manifest_a in
  assert_equal "project package deterministic ref" package_a.package_ref package_again.package_ref;
  assert_equal "project package deterministic path" package_a.package_path package_again.package_path;
  assert_equal "project package deterministic interface ref" package_a.interface_ref
    package_again.interface_ref;
  assert_equal "project package deterministic interface path" package_a.interface_path
    package_again.interface_path;
  assert_equal "project package deterministic content" package_content
    (Store.read_file package_again.package_path);
  assert_equal "project package deterministic interface content" package_interface_json
    (Store.read_file package_again.interface_path);
  let package_locked = Workspace.write_package ~locked:true manifest_a in
  assert_equal "project package locked ref" package_a.package_ref package_locked.package_ref;
  assert_equal "project package locked interface ref" package_a.interface_ref
    package_locked.interface_ref;
  let package_copy_root = temp_dir "workspace-package-copy" in
  copy_tree ws_a package_copy_root;
  let package_copy_manifest = Workspace.parse_manifest package_copy_root in
  let package_copy = Workspace.write_package package_copy_manifest in
  assert_equal "project package ref is path independent" package_a.package_ref
    package_copy.package_ref;
  assert_equal "project package interface ref is path independent" package_a.interface_ref
    package_copy.interface_ref;
  assert_equal "project package interface JSON is path independent" package_interface_json
    (Store.read_file package_copy.interface_path);
  let interface_ws = temp_dir "workspace-interface-constraint" in
  copy_tree ws_a interface_ws;
  let manifest_path_interface = Filename.concat interface_ws "protoss.toml" in
  let manifest_without_interface = Store.read_file manifest_path_interface in
  write_file manifest_path_interface
    (manifest_without_interface ^ "package_interfaces = [\"workspace-a=" ^ interface_hash ^ "\"]\n");
  let manifest_with_interface = Workspace.parse_manifest interface_ws in
  let package_with_interface = Workspace.write_package manifest_with_interface in
  assert_equal "package interface constraint accepts current package" package_with_interface.package_ref
    (Workspace.check_package manifest_with_interface).Workspace.package_ref;
  write_file manifest_path_interface
    (manifest_without_interface ^ "package_interfaces = [\"workspace-a=p2:bad\"]\n");
  let manifest_bad_interface = Workspace.parse_manifest interface_ws in
  let package_dot_before_bad = snapshot (Filename.concat interface_ws ".protoss") in
  (try
     ignore (Workspace.check_package manifest_bad_interface);
     fail "package interface constraint should reject mismatch"
   with Workspace.Error _ -> ());
  (try
     ignore (Workspace.write_package manifest_bad_interface);
     fail "invalid package interface write should reject without mutation"
   with Workspace.Error _ -> ());
  assert_true "invalid package interface write leaves package store untouched"
    (package_dot_before_bad = snapshot (Filename.concat interface_ws ".protoss"));
  let consumer_ws = make_workspace "workspace-consumer" 5 "z" in
  let consumer_manifest_path = Filename.concat consumer_ws "protoss.toml" in
  let consumer_manifest_base = Store.read_file consumer_manifest_path in
  write_file consumer_manifest_path
    (consumer_manifest_base ^ "package_imports = [\"workspace-a=" ^ ws_a
   ^ "\"]\npackage_interfaces = [\"workspace-a=" ^ interface_hash
   ^ "\"]\npackage_contracts = [\"workspace-a=" ^ package_interface_contract_hash ^ "\"]\n");
  let consumer_manifest = Workspace.parse_manifest consumer_ws in
  let consumer_package = Workspace.write_package consumer_manifest in
  let consumer_package_content = Store.read_file consumer_package.package_path in
  assert_true "package dependency records package ref"
    (contains_substring consumer_package_content ("workspace-a=" ^ package_a.package_ref));
  assert_true "package dependency records interface hash"
    (contains_substring consumer_package_content ("workspace-a=" ^ interface_hash));
  assert_true "package dependency records contract hash"
    (contains_substring consumer_package_content ("workspace-a=" ^ package_interface_contract_hash));
  let consumer_interface_text = Workspace.package_interface_text consumer_manifest in
  assert_true "package interface prints imported package"
    (contains_substring consumer_interface_text ("import workspace-a package=" ^ package_a.package_ref));
  assert_true "package interface prints imported interface"
    (contains_substring consumer_interface_text ("interface=" ^ interface_hash));
  assert_true "package interface prints imported contract"
    (contains_substring consumer_interface_text ("contract=" ^ package_interface_contract_hash));
  let consumer_interface_obj = Json.parse (Workspace.package_interface_json consumer_manifest) in
  let consumer_imports = json_array_field "imports" consumer_interface_obj in
  let workspace_import =
    match
      consumer_imports
      |> List.find_opt (fun item -> String.equal "workspace-a" (json_string_field "name" item))
    with
    | Some item -> item
    | None -> fail "missing workspace-a interface import"
  in
  assert_equal "package interface json imported package" package_a.package_ref
    (json_string_field "packageRef" workspace_import);
  assert_equal "package interface json imported interface" interface_hash
    (json_string_field "interfaceHash" workspace_import);
  assert_equal "package interface json imported contract" package_interface_contract_hash
    (json_string_field "contractHash" workspace_import);
  assert_equal "package dependency check ref" consumer_package.package_ref
    (Workspace.check_package consumer_manifest).Workspace.package_ref;
  let consumer_package_invariants = Invariants.check_package consumer_ws in
  assert_equal "package invariant ref" consumer_package.package_ref
    consumer_package_invariants.Invariants.package_ref;
  assert_equal "package invariant imported count" "1"
    (string_of_int consumer_package_invariants.Invariants.imported_packages);
  assert_true "package invariant imported contract hash"
    (String.length consumer_package_invariants.Invariants.interface_contract_hash > 3);
  assert_true "package invariant imported capability count"
    (consumer_package_invariants.Invariants.interface_capabilities > 0);
  assert_true "package invariant imported interface exports"
    (consumer_package_invariants.Invariants.interface_exports > 0);
  assert_equal "package invariant imported type hash count"
    (string_of_int consumer_package_invariants.Invariants.interface_exports)
    (string_of_int consumer_package_invariants.Invariants.interface_type_hashes);
  let import_math_path = Filename.concat ws_a "src/math.protoss" in
  let import_math_before = Store.read_file import_math_path in
  let consumer_dot_before_import_drift = snapshot (Filename.concat consumer_ws ".protoss") in
  write_file import_math_path (import_math_before ^ "(def importedDrift Nat 9)\n");
  (try
     ignore (Invariants.check_package consumer_ws);
     fail "package invariant should reject imported package source drift"
   with Workspace.Error _ | Kernel.Error _ -> ());
  (try
     ignore (Workspace.check_package consumer_manifest);
     fail "package check should reject imported package source drift"
   with Workspace.Error _ -> ());
  (try
     ignore (Workspace.write_package consumer_manifest);
     fail "package write should reject imported package source drift"
   with Workspace.Error _ -> ());
  assert_true "imported package source drift leaves consumer package store untouched"
    (consumer_dot_before_import_drift = snapshot (Filename.concat consumer_ws ".protoss"));
  write_file import_math_path import_math_before;
  let consumer_dot_before_bad = snapshot (Filename.concat consumer_ws ".protoss") in
  write_file consumer_manifest_path
    (consumer_manifest_base ^ "package_imports = [\"workspace-a=" ^ ws_a
   ^ "\"]\npackage_interfaces = [\"workspace-a=p2:bad\"]\n");
  let consumer_bad_interface = Workspace.parse_manifest consumer_ws in
  (try
     ignore (Workspace.write_package consumer_bad_interface);
     fail "bad imported package interface should reject without mutation"
   with Workspace.Error _ -> ());
  assert_true "bad imported package interface leaves package store untouched"
    (consumer_dot_before_bad = snapshot (Filename.concat consumer_ws ".protoss"));
  let consumer_dot_before_bad_contract = snapshot (Filename.concat consumer_ws ".protoss") in
  write_file consumer_manifest_path
    (consumer_manifest_base ^ "package_imports = [\"workspace-a=" ^ ws_a
   ^ "\"]\npackage_contracts = [\"workspace-a=p2:bad\"]\n");
  let consumer_bad_contract = Workspace.parse_manifest consumer_ws in
  (try
     ignore (Workspace.write_package consumer_bad_contract);
     fail "bad imported package contract should reject without mutation"
   with Workspace.Error _ -> ());
  assert_true "bad imported package contract leaves package store untouched"
    (consumer_dot_before_bad_contract = snapshot (Filename.concat consumer_ws ".protoss"));
  let mismatch_ws = make_workspace "workspace-import-mismatch" 6 "q" in
  let mismatch_manifest_path = Filename.concat mismatch_ws "protoss.toml" in
  let mismatch_manifest_base = Store.read_file mismatch_manifest_path in
  write_file mismatch_manifest_path
    (mismatch_manifest_base ^ "package_imports = [\"wrong-name=" ^ ws_a ^ "\"]\n");
  let mismatch_manifest = Workspace.parse_manifest mismatch_ws in
  (try
     ignore (Workspace.write_package mismatch_manifest);
     fail "package import name mismatch should reject"
   with Workspace.Error _ -> ());
  let capability_interface_ws = temp_dir "workspace-interface-capability" in
  copy_tree ws_a capability_interface_ws;
  let capability_manifest_path = Filename.concat capability_interface_ws "protoss.toml" in
  write_file capability_manifest_path
    (replace_once (Store.read_file capability_manifest_path) "capabilities = [\"Human.ask\"]"
       "capabilities = [\"Clock.read\"]");
  let capability_app_path = Filename.concat capability_interface_ws "src/app.protoss" in
  write_file capability_app_path
    (replace_once (Store.read_file capability_app_path) "(Human.ask \"Name?\")" "(Clock.read)");
  let capability_manifest = Workspace.parse_manifest capability_interface_ws in
  let capability_package = Workspace.write_package capability_manifest in
  let capability_interface_hash =
    sexp_atom_field "interface-hash" (Store.read_file capability_package.package_path)
  in
  assert_true "package interface hash includes public capability scope"
    (not (String.equal interface_hash capability_interface_hash));
  let dot_before_drift = snapshot (Filename.concat ws_a ".protoss") in
  let math_path = Filename.concat ws_a "src/math.protoss" in
  let math_before = Store.read_file math_path in
  write_file math_path (math_before ^ "(def drift Nat 0)\n");
  (try
     ignore (Workspace.check_lock manifest_a);
     fail "project lock check should reject source drift"
   with Workspace.Error _ -> ());
  (try
     ignore (Workspace.check_package manifest_a);
     fail "package check should reject source drift"
   with Workspace.Error _ -> ());
  (try
     ignore (Workspace.write_package ~locked:true manifest_a);
     fail "locked package should reject source drift"
   with Workspace.Error _ -> ());
  (try
     ignore (Workspace.build_locked manifest_a);
     fail "locked build should reject source drift"
   with Workspace.Error _ -> ());
  assert_equal "project lock drift keeps lockfile" lock_before (Store.read_file lock_path);
  assert_true "project lock drift leaves .protoss untouched"
    (dot_before_drift = snapshot (Filename.concat ws_a ".protoss"));
  write_file math_path math_before;
  let locked_build = Workspace.build_locked manifest_a in
  let locked_build_meta =
    Filename.concat
      (Filename.concat locked_build.Workspace.store "builds")
      (Workspace.sanitize_id locked_build.Workspace.build_id ^ ".build")
  in
  assert_true "locked build records lock hash"
    (contains_substring (Store.read_file locked_build_meta) ("lock_hash=" ^ lock_hash));
  let scope_corrupt_root = temp_dir "workspace-scope-corrupt" in
  copy_tree ws_a scope_corrupt_root;
  let scope_corrupt_manifest = Workspace.parse_manifest scope_corrupt_root in
  let scope_corrupt_store = Workspace.store_root scope_corrupt_manifest in
  write_file
    (Filename.concat (Filename.concat scope_corrupt_store "capability-scopes")
       "askName.capabilities")
    "Clock.read\n";
  (try
     ignore (Workspace.audit scope_corrupt_manifest);
     fail "audit should reject corrupt capability scope"
   with Workspace.Error _ | Kernel.Error _ -> ());
  let package_corrupt_root = temp_dir "workspace-package-corrupt" in
  copy_tree ws_a package_corrupt_root;
  let package_corrupt_manifest = Workspace.parse_manifest package_corrupt_root in
  let package_corrupt_ref =
    String.trim (Store.read_file (Workspace.package_current_path package_corrupt_manifest))
  in
  let package_corrupt_path =
    Filename.concat (Workspace.packages_dir package_corrupt_manifest)
      (Workspace.sanitize_id package_corrupt_ref ^ ".package")
  in
  write_file package_corrupt_path
    (replace_once (Store.read_file package_corrupt_path) "protoss-package-v1" "protoss-package-bad");
  (try
     ignore (Workspace.check_package package_corrupt_manifest);
     fail "package check should reject corrupt descriptor"
   with Workspace.Error _ -> ());
  (try
     ignore (Workspace.audit package_corrupt_manifest);
     fail "audit should reject corrupt package descriptor"
   with Workspace.Error _ -> ());
  let interface_corrupt_root = temp_dir "workspace-interface-artifact-corrupt" in
  copy_tree ws_a interface_corrupt_root;
  let interface_corrupt_manifest = Workspace.parse_manifest interface_corrupt_root in
  let interface_corrupt_ref =
    String.trim
      (Store.read_file (Workspace.package_interface_current_path interface_corrupt_manifest))
  in
  let interface_corrupt_path =
    Workspace.package_interface_path_for_ref interface_corrupt_manifest interface_corrupt_ref
  in
  write_file interface_corrupt_path
    (replace_once (Store.read_file interface_corrupt_path)
       "protoss-package-interface-v1" "protoss-package-interface-bad");
  (try
     ignore (Workspace.check_package interface_corrupt_manifest);
     fail "package check should reject corrupt interface artifact"
   with Workspace.Error _ -> ());
  (try
     ignore (Workspace.audit interface_corrupt_manifest);
     fail "audit should reject corrupt interface artifact"
   with Workspace.Error _ -> ());
  let package_outdated_root = temp_dir "workspace-package-outdated" in
  copy_tree ws_a package_outdated_root;
  let package_outdated_manifest = Workspace.parse_manifest package_outdated_root in
  let package_outdated_ref =
    String.trim (Store.read_file (Workspace.package_current_path package_outdated_manifest))
  in
  let package_outdated_content =
    Store.read_file
      (Filename.concat (Workspace.packages_dir package_outdated_manifest)
         (Workspace.sanitize_id package_outdated_ref ^ ".package"))
    |> fun content -> replace_once content "(interface-hash p2:" "(interface-hash p2:bad"
  in
  let package_outdated_ref = Kernel.hash_string package_outdated_content in
  let package_outdated_path =
    Filename.concat (Workspace.packages_dir package_outdated_manifest)
      (Workspace.sanitize_id package_outdated_ref ^ ".package")
  in
  write_file package_outdated_path package_outdated_content;
  write_file (Workspace.package_current_path package_outdated_manifest)
    (package_outdated_ref ^ "\n");
  (try
     ignore (Workspace.check_package package_outdated_manifest);
     fail "package check should reject out-of-date descriptor"
   with Workspace.Error _ -> ());
  let graph_corrupt_root = temp_dir "workspace-graph-corrupt" in
  copy_tree ws_a graph_corrupt_root;
  let graph_corrupt_manifest = Workspace.parse_manifest graph_corrupt_root in
  let graph_corrupt_store = Workspace.store_root graph_corrupt_manifest in
  let graph_corrupt_path = Filename.concat graph_corrupt_store "program.graph.json" in
  write_file graph_corrupt_path
    (replace_once (Store.read_file graph_corrupt_path)
       (Kernel.hash_program build_a.Workspace.checked)
       "p2:0000000000000000000000000000000000000000000000000000000000000000");
  (try
     ignore (Workspace.audit graph_corrupt_manifest);
     fail "audit should reject corrupt canonical graph"
   with Workspace.Error _ | Kernel.Error _ -> ());
  let graph_object_corrupt_root = temp_dir "workspace-graph-object-corrupt" in
  copy_tree ws_a graph_object_corrupt_root;
  let graph_object_corrupt_manifest = Workspace.parse_manifest graph_object_corrupt_root in
  let graph_object_corrupt_store = Workspace.store_root graph_object_corrupt_manifest in
  let graph_object_corrupt_graph =
    Json.parse (Store.read_file (Filename.concat graph_object_corrupt_store "program.graph.json"))
  in
  let graph_object_corrupt_path =
    Store.graph_path graph_object_corrupt_store (json_string_field "graphHash" graph_object_corrupt_graph)
  in
  Store.write_file_atomic graph_object_corrupt_path
    (Store.read_file graph_object_corrupt_path ^ "corrupt\n");
  (try
     ignore (Workspace.audit graph_object_corrupt_manifest);
     fail "audit should reject corrupt content-addressed canonical graph"
   with Workspace.Error msg ->
     assert_true "audit reports content-addressed graph mismatch"
       (contains_substring msg "content-addressed canonical graph mismatch"));

  let module_ws = temp_dir "workspace-modules" in
  ensure_dir module_ws;
  ensure_dir (Filename.concat module_ws "src");
  write_file (Filename.concat module_ws "protoss.toml")
    ("name = \"workspace-modules\"\nversion = \"0.5.0\"\nentrypoints = [\"src/app.protoss\"]\nstdlib = \""
   ^ stdlib_path
    ^ "\"\nsource_dirs = [\"src\"]\nstore_dir = \".protoss/store\"\ncache_dir = \".protoss/cache\"\ncapabilities = []\n");
  write_file (Filename.concat module_ws "src/math.protoss")
    "(module Demo.Math)\n(export Number double)\n(type Number Nat)\n(def hidden Number 2)\n\
     (def double (-> Number Number) (lambda (x Number) ((Nat.mul x) hidden)))\n";
  write_file (Filename.concat module_ws "src/app.protoss")
    "(import \"math.protoss\")\n(def result Demo.Math.Number (Demo.Math.double 4))\n";
  let module_manifest = Workspace.parse_manifest module_ws in
  let module_build = Workspace.build module_manifest in
  let module_checked = module_build.Workspace.checked in
  let module_value, _ = Runtime.normalize_def module_checked "result" in
  assert_equal "workspace module export" "8" (Runtime.value_to_string module_value);
  write_file (Filename.concat module_ws "src/bad.protoss")
    "(def leak Demo.Math.Number Demo.Math.hidden)\n";
  (try
     ignore (Workspace.build module_manifest);
     fail "workspace should reject non-imported private module symbol"
   with Workspace.Error msg -> assert_true "workspace private module error" (String.contains msg 'i'));

  let second_build = Workspace.build manifest_a in
  assert_equal "incremental parsed" "0" (string_of_int second_build.Workspace.stats.Workspace.parsed);
  assert_true "incremental reused" (second_build.Workspace.stats.Workspace.reused > 0);
  assert_equal "incremental normalized" "0"
    (string_of_int second_build.Workspace.stats.Workspace.normalized);

  let ws_alpha = make_workspace "workspace-alpha" 2 "y" in
  let build_alpha = Workspace.build (Workspace.parse_manifest ws_alpha) in
  assert_true "alpha-only diff ignored"
    (Workspace.diff build_a.store build_alpha.store = []);

  let ws_b = make_workspace "workspace-b" 3 "x" in
  let build_b = Workspace.build (Workspace.parse_manifest ws_b) in
  let semantic_diff = Workspace.diff build_a.store build_b.store in
  assert_true "semantic diff should not be empty" (semantic_diff <> []);
  assert_true "semantic diff text names change"
    (String.contains (Workspace.diff_to_text semantic_diff) 'b');
  assert_true "semantic diff json" (String.contains (Workspace.diff_to_json semantic_diff) '{');

  let patch_from_diff = patch_file "protoss-from-diff.json" (Workspace.patch_from_diff build_a.store build_b.store) in
  let patched_store = temp_dir "patched-from-diff" in
  copy_tree build_a.store patched_store;
  ignore (Patch.apply patched_store patch_from_diff);
  assert_true "patch from diff applicable" (Workspace.diff patched_store build_b.store = []);
  let patched_checked = Store.load_program patched_store |> Kernel.check_program in
  assert_equal "patch updates program canon"
    (Kernel.serialize_checked_program patched_checked ^ "\n")
    (Store.read_file (Filename.concat patched_store "program.canon"));
  assert_equal "patch updates program graph" (Kernel.checked_to_graph_json patched_checked)
    (Store.read_file (Filename.concat patched_store "program.graph.json"));
  let patched_graph = Json.parse (Store.read_file (Filename.concat patched_store "program.graph.json")) in
  assert_equal "patch updates graph object"
    (Store.read_file (Filename.concat patched_store "program.graph.json"))
    (Store.read_file (Store.graph_path patched_store (json_string_field "graphHash" patched_graph)));

  let corrupt_root = temp_dir "workspace-corrupt" in
  copy_tree ws_a corrupt_root;
  let corrupt_manifest = Workspace.parse_manifest corrupt_root in
  let corrupt_store = Workspace.store_root corrupt_manifest in
  write_file (Store.canonical_path corrupt_store "base") "corrupt\n";
  (try
     ignore (Workspace.audit corrupt_manifest);
     fail "audit should reject corrupt store"
   with Workspace.Error _ | Kernel.Error _ -> ());

  let todo_src = find_up (Sys.getcwd ()) "examples/web/todo_app" in
  let todo = temp_dir "web-todo" in
  copy_tree todo_src todo;
  write_file (Filename.concat todo "protoss.toml")
    ("name = \"todo-web-alpha-test\"\nversion = \"0.1.0\"\nentrypoints = [\"src/app.protoss\"]\nstdlib = \""
    ^ stdlib_path
    ^ "\"\nsource_dirs = [\"src\"]\nstore_dir = \".protoss/store\"\ncache_dir = \".protoss/cache\"\ncapabilities = [\"Local.storage\"]\n");
  let contract = Web.app_check todo in
  assert_equal "web app model"
    "(Record (draft String) (items (List String)) (next Nat))"
    (Ast.string_of_typ contract.Web.model_ty);
  let web_dist_a = temp_dir "web-dist-a" in
  let web_a = Web.build ~out:web_dist_a todo in
  List.iter
    (fun file ->
      assert_true ("web artifact " ^ file) (Sys.file_exists (Filename.concat web_dist_a file)))
    [
      "index.html";
      "protoss-runtime.js";
      "protoss-app.json";
      "protoss-graph.json";
      "protoss-canon-graph.json";
      "protoss-capabilities.json";
      "protoss-world.json";
    ];
  let web_canon_graph =
    Json.parse (Store.read_file (Filename.concat web_dist_a "protoss-canon-graph.json"))
  in
  let web_app_json = Json.parse (Store.read_file (Filename.concat web_dist_a "protoss-app.json")) in
  let embedded_program = json_field "program" web_app_json in
  assert_equal "web canonical graph version" Kernel.canonical_graph_version
    (json_string_field "version" web_canon_graph);
  assert_equal "web canonical graph hash" (Kernel.hash_program web_a.Web.build.Workspace.checked)
    (json_string_field "programHash" web_canon_graph);
  assert_equal "web embedded canonical graph version" Kernel.canonical_graph_version
    (json_string_field "version" embedded_program);
  assert_equal "web embedded canonical graph hash" (Kernel.hash_program web_a.Web.build.Workspace.checked)
    (json_string_field "programHash" embedded_program);
  assert_true "web app embeds canonical node graph"
    (List.length (json_array_field "nodes" (json_field "nodeGraph" embedded_program)) > 0);
  assert_true "web embedded graph matches artifact" (web_canon_graph = embedded_program);
  let web_runtime_js = Store.read_file (Filename.concat web_dist_a "protoss-runtime.js") in
  assert_true "web runtime interprets canonical graph"
    (contains_substring web_runtime_js "evalProgram(app.program)");
  assert_true "web runtime exposes suspended requests"
    (contains_substring web_runtime_js "protoss:request");
  assert_true "web runtime is not Todo hardcoded"
    (not (contains_substring web_runtime_js "applyMsg")
    && not (contains_substring web_runtime_js "NewTodoChanged")
    && not (contains_substring web_runtime_js "AddTodo"));
  let web_capabilities =
    Json.parse (Store.read_file (Filename.concat web_dist_a "protoss-capabilities.json"))
  in
  assert_equal "web capabilities names" "Local.storage"
    (String.concat "," (json_string_array_field "capabilities" web_capabilities));
  let web_capability_descriptors = json_array_field "capabilityDescriptors" web_capabilities in
  assert_true "web capability descriptor count" (List.length web_capability_descriptors = 1);
  let local_storage_descriptor = List.hd web_capability_descriptors in
  let local_storage_ref =
    match Kernel.capability_ref "Local.storage" with
    | Some ref -> ref
    | None -> fail "missing Local.storage capability ref"
  in
  assert_equal "web capability descriptor ref" local_storage_ref
    (json_string_field "ref" local_storage_descriptor);
  assert_equal "web capability descriptor name" "Local.storage"
    (json_string_field "name" local_storage_descriptor);
  let local_storage_requests = json_array_field "requests" local_storage_descriptor in
  assert_true "web capability request signatures" (List.length local_storage_requests = 2);
  assert_true "web capability request refs"
    (List.for_all
       (fun request -> contains_substring (json_string_field "ref" request) "p2:")
       local_storage_requests);
  let web_dist_b = temp_dir "web-dist-b" in
  ignore (Web.build ~out:web_dist_b todo);
  List.iter
    (fun file ->
      assert_equal ("deterministic artifact " ^ file)
        (Store.read_file (Filename.concat web_dist_a file))
        (Store.read_file (Filename.concat web_dist_b file)))
    [
      "index.html";
      "protoss-runtime.js";
      "protoss-app.json";
      "protoss-graph.json";
      "protoss-canon-graph.json";
      "protoss-capabilities.json";
      "protoss-world.json";
    ];
  let web_second = Workspace.build (Workspace.parse_manifest todo) in
  assert_equal "web incremental parsed" "0" (string_of_int web_second.Workspace.stats.Workspace.parsed);
  assert_true "web incremental reused" (web_second.Workspace.stats.Workspace.reused > 0);
  assert_true "web inspect" (String.contains (Web.inspect todo) 'm');

  let init_value, _ = Runtime.eval_entry contract.Web.checked "init" in
  let model =
    match init_value with Runtime.VProcessDone model -> model | _ -> fail "web init should be Done"
  in
  let update_value, _ = Runtime.eval_entry contract.Web.checked "update" in
  let add_msg = Runtime.VVariant (contract.Web.msg_ty, "AddTodo", Runtime.VUnit) in
  let update_for_msg = Runtime.apply contract.Web.checked update_value add_msg in
  let process = Runtime.apply contract.Web.checked update_for_msg model in
  let suspended =
    match process with
    | Runtime.VProcessRequest s -> s
    | other -> fail ("web update should suspend, got " ^ Runtime.value_to_string other)
  in
  assert_true "web update SaveLocal request"
    (match suspended.Runtime.req with Ast.SaveLocal ("todos", "updated") -> true | _ -> false);
  (try
     ignore (Runtime.response_value suspended.req "String:oops");
     fail "wrong SaveLocal response should be rejected"
   with Kernel.Error _ -> ());

  let patch_dir = find_up (Sys.getcwd ()) "patches/web" in
  let web_store = web_a.Web.build.Workspace.store in
  let before_web_patch = snapshot web_store in
  ignore (Patch.apply web_store (Filename.concat patch_dir "change_button_text.json"));
  let after_valid_patch = Workspace.diff_to_text (Workspace.diff web_store web_store) in
  assert_equal "valid web patch leaves self diff empty" "No semantic changes\n" after_valid_patch;
  let before_invalid = snapshot web_store in
  (try
     ignore (Patch.apply web_store (Filename.concat patch_dir "invalid_msg_view_mismatch.json"));
     fail "invalid web View/Msg patch should be rejected"
   with Patch.Error _ -> ());
  assert_true "invalid web patch mutates nothing" (snapshot web_store = before_invalid);
  assert_true "valid web patch changed store" (before_web_patch <> snapshot web_store);

  ignore (Web.build ~out:(temp_dir "web-dist-reset") todo);
  (try
     ignore (Patch.check web_store (Filename.concat patch_dir "model_without_migration.json"));
     fail "model patch without migration should be rejected"
   with Patch.Error _ -> ());
  ignore (Patch.check web_store (Filename.concat patch_dir "model_with_migration.json"));

  let corrupt_todo = temp_dir "web-corrupt" in
  copy_tree todo corrupt_todo;
  let corrupt_manifest = Workspace.parse_manifest corrupt_todo in
  ignore (Web.build ~out:(temp_dir "web-corrupt-dist") corrupt_todo);
  write_file (Store.canonical_path (Workspace.store_root corrupt_manifest) "buttonLabel") "corrupt\n";
  (try
     ignore (Workspace.audit corrupt_manifest);
     fail "web audit should reject corrupt store"
   with Workspace.Error _ | Kernel.Error _ -> ());

  let ledger_web = temp_dir "web-ledger" in
  let world0 = Ledger.init ledger_web in
  let save_req = Ast.SaveLocal ("todos", "updated") in
  let _, save_suspended, save_request_id, save_continuation_id =
    ledger_suspension save_req [ "Local.storage" ]
  in
  let event, world1 =
    Ledger.record_request ledger_web world0 save_req save_suspended save_request_id
      save_continuation_id [ "Local.storage" ]
  in
  assert_true "ledger inspect alias" (String.length (Ledger.inspect ledger_web world1) > 0);
  assert_true "ledger replay" (String.contains (Ledger.replay ledger_web world1) 'E');
  assert_equal "ledger diff same" "only_a=\nonly_b=\n" (Ledger.diff ledger_web world1 world1);
  assert_true "ledger event in replay" (String.contains (Ledger.replay ledger_web world1) event.[0]);
  let exported = Ledger.export ledger_web world1 in
  let imported_ref = Ledger.import ledger_web exported in
  assert_true "ledger export/import" (String.length imported_ref > 3);
  assert_true "ledger branches" (String.contains (Ledger.branches ledger_web) 'p');

  let ledger_root = temp_dir "ledger-inspect" in
  let world = Ledger.init ledger_root in
  let ask_req = Ast.AskHuman "x" in
  let _ask_suspended, suspended, request_id, continuation_id =
    ledger_suspension ask_req [ "Human.ask" ]
  in
  let event, next_world =
    Ledger.record_request ledger_root world ask_req suspended request_id continuation_id
      [ "Human.ask" ]
  in
  assert_true "ledger event inspectable" (String.length (Ledger.inspect_event ledger_root event) > 0);
  let inspected_request_event = Ledger.inspect_event ledger_root event in
  assert_true "ledger request records capability signature"
    (contains_substring inspected_request_event "capability=Human.ask");
  assert_true "ledger request records capability ref"
    (contains_substring inspected_request_event ("capability-ref=" ^ human_capability_ref));
  assert_true "ledger request records request tag"
    (contains_substring inspected_request_event "request-tag=AskHuman");
  assert_true "ledger request records signature ref"
    (contains_substring inspected_request_event ("request-signature-ref=" ^ human_signature_ref));
  assert_true "ledger request records response type"
    (contains_substring inspected_request_event "response-type=String");
  assert_true "ledger world inspectable" (String.length (Ledger.inspect_world ledger_root next_world) > 0);
  let resume_event, resume_world =
    Ledger.record_resume ledger_root next_world event "String:Ada" "Done \"Ada\""
  in
  assert_true "ledger resume event inspectable"
    (String.length (Ledger.inspect_event ledger_root resume_event) > 0);
  assert_true "ledger resume records response type"
    (contains_substring (Ledger.inspect_event ledger_root resume_event) "response-type=String");
  assert_true "ledger resume records signature ref"
    (contains_substring (Ledger.inspect_event ledger_root resume_event)
       ("request-signature-ref=" ^ human_signature_ref));
  assert_true "ledger resume world inspectable"
    (String.length (Ledger.inspect_world ledger_root resume_world) > 0);
  let bad_resume_signature_ref_event = "p2:bad-resume-signature-ref-event" in
  Store.write_file_atomic (Ledger.event_path ledger_root bad_resume_signature_ref_event)
    ("world=x\nkind=resume\nresume=" ^ event
   ^ "\nrequest-signature-ref=p2:bad\nresponse-type=String\nresponse=String:Ada\nresult=Done \"Ada\"\n");
  (try
     ignore (Ledger.inspect_event ledger_root bad_resume_signature_ref_event);
     fail "ledger resume event with bad signature ref should be rejected"
   with Failure _ -> ());
  let before_bad_resume = snapshot ledger_root in
  (try
     ignore (Ledger.record_resume ledger_root next_world event "Nat:1" "bad");
     fail "ledger resume wrong response type should be rejected"
   with Failure _ -> ());
  assert_true "invalid ledger resume must not create files"
    (snapshot ledger_root = before_bad_resume);
  let invalid_scope_ledger = temp_dir "ledger-invalid-cap-scope" in
  (try
     ignore
       (Ledger.record_request invalid_scope_ledger Ledger.initial_world (Ast.AskHuman "x")
          suspended request_id continuation_id []);
     fail "ledger request missing capability should be rejected"
   with Failure _ -> ());
  assert_true "invalid ledger request must not create files"
    (count_files invalid_scope_ledger = 0);
  let unknown_scope_ledger = temp_dir "ledger-unknown-cap-scope" in
  (try
     ignore
       (Ledger.record_request unknown_scope_ledger Ledger.initial_world (Ast.AskHuman "x")
          suspended request_id continuation_id [ "Human.ask"; "Space.laser" ]);
     fail "ledger request unknown capability should be rejected"
   with Failure _ -> ());
  assert_true "unknown capability ledger request must not create files"
    (count_files unknown_scope_ledger = 0);
  let bad_event = "p2:bad-event" in
  Store.write_file_atomic (Ledger.event_path ledger_root bad_event) "world=x\nkind=request\n";
  (try
     ignore (Ledger.inspect_event ledger_root bad_event);
     fail "maltyped ledger event should be rejected"
   with Failure _ -> ());
  let bad_scope_event = "p2:bad-scope-event" in
  Store.write_file_atomic (Ledger.event_path ledger_root bad_scope_event)
    "world=x\nkind=request\nrequest-id=req\nrequest=AskHuman:x\ncontinuation-id=cont\n\
     cap-scope=Clock.read\nsuspended=s\n";
  (try
     ignore (Ledger.inspect_event ledger_root bad_scope_event);
     fail "ledger event with wrong cap-scope should be rejected"
   with Failure _ -> ());
  let unknown_scope_event = "p2:unknown-scope-event" in
  Store.write_file_atomic (Ledger.event_path ledger_root unknown_scope_event)
    "world=x\nkind=request\nrequest-id=req\nrequest=AskHuman:x\ncontinuation-id=cont\n\
     cap-scope=Human.ask,Space.laser\nsuspended=s\n";
  (try
     ignore (Ledger.inspect_event ledger_root unknown_scope_event);
     fail "ledger event with unknown cap should be rejected"
   with Failure _ -> ());
  let bad_signature_event = "p2:bad-signature-event" in
  Store.write_file_atomic (Ledger.event_path ledger_root bad_signature_event)
    (replace_once inspected_request_event "request-tag=AskHuman" "request-tag=HttpGet");
  (try
     ignore (Ledger.inspect_event ledger_root bad_signature_event);
     fail "ledger request event with bad signature should be rejected"
   with Failure _ -> ());
  let bad_capability_ref_event = "p2:bad-capability-ref-event" in
  Store.write_file_atomic (Ledger.event_path ledger_root bad_capability_ref_event)
    (replace_once inspected_request_event ("capability-ref=" ^ human_capability_ref)
       "capability-ref=p2:bad");
  (try
     ignore (Ledger.inspect_event ledger_root bad_capability_ref_event);
     fail "ledger request event with bad capability ref should be rejected"
   with Failure _ -> ());
  let bad_request_signature_ref_event = "p2:bad-request-signature-ref-event" in
  Store.write_file_atomic (Ledger.event_path ledger_root bad_request_signature_ref_event)
    (replace_once inspected_request_event ("request-signature-ref=" ^ human_signature_ref)
       "request-signature-ref=p2:bad");
  (try
     ignore (Ledger.inspect_event ledger_root bad_request_signature_ref_event);
     fail "ledger request event with bad request signature ref should be rejected"
   with Failure _ -> ());
  let bad_resume_event = "p2:bad-resume-event" in
  Store.write_file_atomic (Ledger.event_path ledger_root bad_resume_event)
    ("world=x\nkind=resume\nresume=" ^ event
   ^ "\nrequest-signature-ref=" ^ human_signature_ref
   ^ "\nresponse-type=String\nresponse=Nat:1\nresult=bad\n");
  (try
     ignore (Ledger.inspect_event ledger_root bad_resume_event);
     fail "ledger resume event with wrong response type should be rejected"
   with Failure _ -> ());
  let mismatched_request_event = "p2:mismatched-request-event" in
  let http_capability_ref =
    match Kernel.req_capability_ref (Ast.HttpGet "") with
    | Some ref -> ref
    | None -> fail "missing Http.get capability ref"
  in
  let http_signature_ref = Kernel.req_signature_ref (Ast.HttpGet "") in
  Store.write_file_atomic (Ledger.event_path ledger_root mismatched_request_event)
    ("world=x\nkind=request\nrequest-id=req\nrequest=HttpGet:https://example.invalid\n\
      capability=Http.get\ncapability-ref="
    ^ http_capability_ref
    ^ "\nrequest-tag=HttpGet\nrequest-signature-ref=" ^ http_signature_ref
    ^ "\nrequest-payload-type=(Record (url String))\n\
      response-type=String\ncontinuation-id=cont\ncap-scope=Http.get\nsuspended="
    ^ String.escaped suspended ^ "\n");
  let bad_resume_mismatch_event = "p2:bad-resume-mismatch-event" in
  Store.write_file_atomic (Ledger.event_path ledger_root bad_resume_mismatch_event)
    ("world=x\nkind=resume\nresume=" ^ mismatched_request_event
   ^ "\nrequest-signature-ref=" ^ http_signature_ref
   ^ "\nresponse-type=String\nresponse=String:Ada\nresult=bad\n");
  (try
     ignore (Ledger.inspect_event ledger_root bad_resume_mismatch_event);
     fail "ledger resume event with mismatched suspended request should be rejected"
   with Failure _ -> ());
  (try
     ignore (Runtime.parse_suspended "(protoss-runtime-v2 (suspended (request ReadClock)))");
     fail "invalid resume suspension should be rejected"
   with Kernel.Error _ -> ());

  print_endline "protoss tests ok"
