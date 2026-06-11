open Protoss

(* Batch-tool GC tuning: a large minor heap and a relaxed space overhead cut
   GC time substantially on allocation-heavy canonicalization/eval workloads.
   Purely a time/memory trade-off; no observable behavior change. *)
let () =
  Gc.set
    { (Gc.get ()) with Gc.minor_heap_size = 16 * 1024 * 1024; Gc.space_overhead = 120 }

let fail msg = raise (Failure msg)

let assert_true msg b = if not b then fail msg

let assert_equal msg a b = if a <> b then fail (msg ^ ": expected " ^ a ^ ", got " ^ b)

let test_trace_enabled () = Sys.getenv_opt "PROTOSS_TEST_TRACE" = Some "1"

let test_started_at = Unix.gettimeofday ()

let trace_test label =
  if test_trace_enabled () then (
    Printf.eprintf "[test %.3fs] %s\n%!" (Unix.gettimeofday () -. test_started_at) label)

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

let json_nat_field name obj =
  match json_field name obj with
  | Json.Num n when n >= 0 -> n
  | _ -> fail ("JSON field is not natural number: " ^ name)

let json_bool_field name obj =
  match json_field name obj with
  | Json.Bool b -> b
  | _ -> fail ("JSON field is not bool: " ^ name)

let json_string_array_field name obj =
  json_array_field name obj
  |> List.map (function Json.String s -> s | _ -> fail ("JSON field is not string array: " ^ name))

let json_string_array_literal xs =
  "[" ^ String.concat ", " (List.map Ast.quote xs) ^ "]"

(* The integration suite is split into independent parts so the fulltest alias
   can run them as parallel processes; PROTOSS_INTEGRATION_PART selects one
   (unset = run every part, preserving the single-process behavior). *)
let run_integration_tests = Sys.getenv_opt "PROTOSS_RUN_INTEGRATION_TESTS" = Some "1"

let integration_part name =
  run_integration_tests
  && (match Sys.getenv_opt "PROTOSS_INTEGRATION_PART" with
      | None -> true
      | Some part -> String.equal part name)

(* The workspace integration part is itself split into independent slices
   (project / consumer / corruption) so dune can run them as parallel
   processes; PROTOSS_WORKSPACE_PART selects one (unset = run every slice).
   Slices that need the workspace-a project rebuild it from scratch — the
   store is content-addressed and every artifact write is deterministic, so
   the rebuild is byte-identical to the state the project slice leaves
   behind, and cheap when it already exists. *)
let workspace_part =
  let known = [ "project"; "consumer"; "corruption" ] in
  (match Sys.getenv_opt "PROTOSS_WORKSPACE_PART" with
  | Some part when not (List.mem part known) ->
      (* An unknown slice would silently skip every workspace test and still
         report success — fail loudly instead. *)
      fail ("unknown PROTOSS_WORKSPACE_PART: " ^ part)
  | _ -> ());
  fun name ->
    match Sys.getenv_opt "PROTOSS_WORKSPACE_PART" with
    | None -> true
    | Some part -> String.equal part name

(* The web integration part is sliced the same way (PROTOSS_WEB_PART:
   app artifacts / patches / audit+corruption+ledger). Every slice rebuilds
   the deterministic todo project from the example sources, so slices run as
   independent processes; unset = run every slice in one process. *)
let web_part =
  let known = [ "app"; "patches"; "audit" ] in
  (match Sys.getenv_opt "PROTOSS_WEB_PART" with
  | Some part when not (List.mem part known) ->
      fail ("unknown PROTOSS_WEB_PART: " ^ part)
  | _ -> ());
  fun name ->
    match Sys.getenv_opt "PROTOSS_WEB_PART" with
    | None -> true
    | Some part -> String.equal part name

let () =
  assert_equal "sha256 empty digest"
    "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
    (Hashcons.digest "");
  assert_equal "content address hash prefix"
    "p2:ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
    (Hashcons.hash "abc");
  (* Padding boundaries: last-chunk handling is the delicate part of the
     streaming digest (63 = max single-block payload, 64 = empty tail block,
     65 = one spilled byte, 128 = chunk-aligned multi-block message). *)
  assert_equal "sha256 padding boundary 63"
    "7d3e74a05d7db15bce4ad9ec0658ea98e3f06eeecf16b4c6fff2da457ddc2f34"
    (Hashcons.digest (String.make 63 'a'));
  assert_equal "sha256 padding boundary 64"
    "ffe054fe7ae0cb6dc65c3af9b61d5209f439851db43d0ba5997337df154668eb"
    (Hashcons.digest (String.make 64 'a'));
  assert_equal "sha256 padding boundary 65"
    "635361c48bb9eab14198e76ea8ab7f1a41685d6ad62aa9146d301d4f17eb0ae0"
    (Hashcons.digest (String.make 65 'a'));
  assert_equal "sha256 padding boundary 128"
    "6836cf13bac400e9105071cd6af47084dfacad4e5e302c94bfed24e013afb73e"
    (Hashcons.digest (String.make 128 'a'));
  (* The dispatching digest (hardware-accelerated where available) must be
     bit-identical to the portable pure-OCaml implementation. Sweep every
     length across the first few padding boundaries so a platform-specific
     digest bug cannot slip in silently. *)
  for n = 0 to 300 do
    let s = String.init n (fun i -> Char.chr ((i * 7 + n) land 0xff)) in
    assert_equal
      (Printf.sprintf "sha256 dispatch agrees with pure (len %d)" n)
      (Hashcons.digest_pure s) (Hashcons.digest s)
  done;
  let utf8_sample = "A" ^ "\195\169" ^ "\240\157\132\158" in
  assert_equal "string primitive utf8 length" "3" (string_of_int (String_prim.length utf8_sample));
  assert_equal "string primitive utf8 slice" "\195\169" (String_prim.slice utf8_sample 1 1);
  assert_equal "kernel hash algorithm" "sha256" Kernel.hash_algorithm;
  let public_error_codes =
    List.map (fun (entry : Public_error.entry) -> entry.code) Public_error.catalog
  in
  assert_equal "public error codes are unique"
    (string_of_int (List.length public_error_codes))
    (string_of_int (List.length (List.sort_uniq String.compare public_error_codes)));
  List.iter
    (fun name ->
      assert_true ("public error taxonomy contains " ^ name)
        (List.mem name (Public_error.taxonomy_names ())))
    [
      "TypeMismatch";
      "UnknownReference";
      "CapabilityDenied";
      "NonTerminatingRecursion";
      "NonProductiveProcess";
      "HarnessRegression";
      "AmbiguousHumanSyntax";
      "UnsafeMigration";
      "PolicyViolation";
      "SecretLeakRisk";
    ];
  assert_true "explain WEB007 uses public error catalog"
    (contains_substring (Public_error.explain "WEB007") "message type");
  assert_true "explain CAPABILITY keeps legacy public code"
    (contains_substring (Public_error.explain "CAPABILITY") "explicit capabilities");
  assert_true "explain --list exposes stable codes"
    (contains_substring (Public_error.list_text ()) "TYPE001 TypeMismatch");
  assert_equal "CLI parse errors get syntax code" "SYN001"
    (Public_error.code_for_cli_kind "parse error" "1:1: unterminated list");
  assert_equal "CLI type errors get type mismatch code" "TYPE001"
    (Public_error.code_for_cli_kind "check error" "expected Nat, got Bool");
  assert_equal "CLI capability errors get capability code" "CAP001"
    (Public_error.code_for_cli_kind "check error" "missing capability Human.ask");
  assert_equal "CLI localized load type errors keep type code" "TYPE001"
    (Public_error.code_for_cli_kind "load error"
       "examples/bad.protoss:1:14: definition bad: expected Nat, got Bool");
  assert_equal "CLI localized load syntax errors keep syntax code" "SYN001"
    (Public_error.code_for_cli_kind "load error"
       "examples/bad.protoss:1:1: unterminated list");
  assert_equal "CLI web errors keep explicit web code" "WEB007"
    (Public_error.code_for_cli_kind "web error" "WEB007 view message mismatch");
  let spec_report =
    Spec_audit.report
      "## Demo\n\nPreuves de section: test/test_protoss.ml\n- [x] Done item\n- [ ] Pending item\n"
  in
  assert_equal "spec audit checked count" "1" (string_of_int spec_report.checked_count);
  assert_equal "spec audit accepts section evidence" "0"
    (string_of_int (List.length spec_report.missing));
  let spec_missing = Spec_audit.report "## Demo\n\n- [x] Missing proof\n" in
  assert_equal "spec audit reports missing evidence" "1"
    (string_of_int (List.length spec_missing.missing));
  assert_true "spec audit report names missing line"
    (contains_substring (Spec_audit.report_text spec_missing) "line 3");
  assert_true "kernel executable grammar exposes defs"
    (contains_substring Kernel.executable_grammar_text "declaration ::= (def Name type expr)");
  assert_true "kernel executable grammar exposes Process"
    (contains_substring Kernel.executable_grammar_text "Process");
  assert_true "kernel executable grammar exposes requests"
    (contains_substring Kernel.executable_grammar_text "request ::= (AskHuman String)");
  assert_true "human official grammar versioned"
    (contains_substring Surface_syntax.human_grammar_text "protoss-human-grammar-v1");
  assert_true "human official grammar exposes sexp declarations"
    (contains_substring Surface_syntax.human_grammar_text
       "declaration ::= (module ModuleName)");
  assert_true "human official grammar exposes Elm-like declarations"
    (contains_substring Surface_syntax.human_grammar_text
       "elm_declaration ::= module_decl");
  assert_true "human official grammar exposes capability process type"
    (contains_substring Surface_syntax.human_grammar_text
       "Process { CapabilityName* } elm_type");
  assert_equal "kernel hash prefix" "p2:" Kernel.hash_prefix

(* protoss doctor --v1: the executable V1.0 acceptance proofs. *)
let () =
  (* Every wired proof passes on the kernel as shipped. *)
  let results = List.map (fun (c : Doctor.check) -> (c, Doctor.run_one c)) Doctor.checks in
  List.iter
    (fun ((c : Doctor.check), st) ->
      match st with
      | Doctor.Fail msg -> fail ("doctor proof " ^ c.id ^ " failed: " ^ msg)
      | Doctor.Pass | Doctor.Not_yet _ -> ())
    results;
  (* At least the pure proofs are wired (guards against an empty/hollow run). *)
  let passed =
    List.length (List.filter (function _, Doctor.Pass -> true | _ -> false) results)
  in
  assert_true "doctor wires a healthy floor of real proofs (>= 10)" (passed >= 10);
  (* The golden-projects proof (G2) must be wired and passing, not deferred. *)
  let status_of id =
    List.find_map
      (fun ((c : Doctor.check), st) -> if String.equal c.id id then Some st else None)
      results
  in
  assert_true "doctor golden-projects proof is wired and passing"
    (match status_of "golden-projects" with Some Doctor.Pass -> true | _ -> false);
  (* Panic injection: the doctor must exit non-zero iff an available proof
     breaks, and zero when only Not_yet remain. *)
  assert_equal "doctor passes when all proofs pass or defer" "0"
    (string_of_int (Doctor.aggregate_exit [ Doctor.Pass; Doctor.Not_yet "later"; Doctor.Pass ]));
  assert_equal "doctor fails hard when an available proof breaks" "1"
    (string_of_int
       (Doctor.aggregate_exit [ Doctor.Pass; Doctor.Fail "injected"; Doctor.Not_yet "later" ]));
  assert_equal "doctor does not fail on Not_yet alone" "0"
    (string_of_int (Doctor.aggregate_exit [ Doctor.Not_yet "a"; Doctor.Not_yet "b" ]))

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

let expect_check_error_contains input needle =
  try
    let p = Parser.parse_string input in
    let _ = Kernel.check_program p in
    fail "expected check error"
  with
  | Kernel.Error msg | Parser.Error msg ->
      assert_true ("check error should contain " ^ needle ^ ", got " ^ msg)
        (contains_substring msg needle)

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

(* The kernel caches per-definition elaborations across check_program calls
   (keyed on the definition plus the recorded global-type lookups). The cache
   must never reuse an elaboration once the type of a referenced global
   changed, and must reuse it when only unrelated definitions appear. *)
let () =
  (* Prime the cache: b elaborates against a : Nat. *)
  ignore (Parser.parse_string "(def a Nat 1)\n(def b Nat a)" |> Kernel.check_program);
  (* Same definition text for b, but a's type changed: the cached elaboration
     must be invalidated and checking must fail with the usual error. *)
  expect_check_error_contains "(def a Bool true)\n(def b Nat a)" "definition b";
  (* An unrelated extra definition must leave b's identity untouched (hit or
     miss, the content-addressed result is the same). *)
  let base = Parser.parse_string "(def a Nat 1)\n(def b Nat a)" |> Kernel.check_program in
  let extended =
    Parser.parse_string "(def a Nat 1)\n(def b Nat a)\n(def unrelated Nat 7)"
    |> Kernel.check_program
  in
  assert_equal "per-def elaboration cache: unrelated def keeps b's def id"
    (checked_def base "b").Kernel.def_id
    (checked_def extended "b").Kernel.def_id

let rec runtime_value_matches_type value typ =
  match (value, typ) with
  | Runtime.VUnit, Ast.TUnit -> true
  | Runtime.VBool _, Ast.TBool -> true
  | Runtime.VNat _, Ast.TNat -> true
  | Runtime.VString _, Ast.TString -> true
  | Runtime.VClosure _, Ast.TFun _ -> true
  | Runtime.VList (item_ty, items), Ast.TList expected_item_ty ->
      Ast.equal_typ item_ty expected_item_ty
      && List.for_all
           (fun item -> runtime_value_matches_type item expected_item_ty)
           items
  | Runtime.VRecord actual_fields, Ast.TRecord expected_fields ->
      let actual_fields = Ast.sort_fields actual_fields in
      let expected_fields = Ast.sort_fields expected_fields in
      List.length actual_fields = List.length expected_fields
      && List.for_all2
           (fun (actual_name, value) (expected_name, typ) ->
             String.equal actual_name expected_name && runtime_value_matches_type value typ)
           actual_fields expected_fields
  | Runtime.VVariant (actual_ty, con, payload), Ast.TVariant expected_cases ->
      Ast.equal_typ actual_ty typ
      && (match List.assoc_opt con expected_cases with
         | Some payload_ty -> runtime_value_matches_type payload payload_ty
         | None -> false)
  | Runtime.VView _, Ast.TView _ -> true
  | Runtime.VAttribute _, Ast.TAttr _ -> true
  | Runtime.VProcessDone value, Ast.TProcess (_, typ) -> runtime_value_matches_type value typ
  | Runtime.VProcessRequest _, Ast.TProcess _ -> true
  | Runtime.VUnit, Ast.TCmd _ -> true
  | _ -> false

let assert_normalized_value_preserves_declared_type checked name =
  let def = checked_def checked name in
  let value, _ = Runtime.normalize_def checked name in
  assert_true
    ("normalized value for " ^ name ^ " matches declared type "
    ^ Ast.string_of_typ def.Kernel.def.typ ^ ", got " ^ Runtime.value_to_string value)
    (runtime_value_matches_type value def.Kernel.def.typ)

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
  (* Pid-qualified: suite sections run as parallel processes and would race on
     fixed temp paths otherwise (the file name stays last for error matching). *)
  let path =
    Filename.concat (Filename.get_temp_dir_name ())
      (string_of_int (Unix.getpid ()) ^ "-" ^ name)
  in
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

(* Tries an APFS copy-on-write clone first (one syscall for a whole tree, no
   data I/O — workspace tests copy ~1.5k-file stores a dozen times); falls back
   to the portable recursive copy, re-trying the clone per subtree so partially
   existing destinations still benefit. *)
let rec copy_tree src dst =
  if Store.try_clone src dst then ()
  else if Sys.is_directory src then (
    ensure_dir dst;
    Sys.readdir src |> Array.iter (fun f -> copy_tree (Filename.concat src f) (Filename.concat dst f)))
  else write_file dst (Store.read_file src)

let rec find_up start rel =
  let candidate = Filename.concat start rel in
  if Sys.file_exists candidate then Unix.realpath candidate
  else
    let parent = Filename.dirname start in
    if String.equal parent start then fail ("cannot find " ^ rel) else find_up parent rel

(* Bytecode format (G4): every example fixture that checks in isolation must
   compile to bytecode whose encoding is deterministic and decode-round-trip
   stable. Execution parity with the interpreter is G5. *)
let () =
  let examples_dir = Filename.dirname (find_up (Sys.getcwd ()) "examples/basic.protoss") in
  let covered = ref 0 in
  Sys.readdir examples_dir |> Array.to_list |> List.sort String.compare
  |> List.iter (fun f ->
         if Filename.check_suffix f ".protoss" then
           let path = Filename.concat examples_dir f in
           match Parser.parse_string (Store.read_file path) |> Kernel.check_program with
           | exception _ -> () (* does not check in isolation: out of scope *)
           | checked ->
               let m = Bytecode.compile_checked checked in
               let bytes1 = Bytecode.encode_module m in
               let bytes2 = Bytecode.encode_module (Bytecode.decode_module bytes1) in
               assert_true ("bytecode decode round-trip stable for " ^ f)
                 (String.equal bytes1 bytes2);
               assert_true ("bytecode hash deterministic for " ^ f)
                 (String.equal (Bytecode.hash_module m)
                    (Bytecode.hash_module (Bytecode.compile_checked checked)));
               (* G5: the VM executes every def at parity with the interpreter. *)
               List.iter
                 (fun (d : Kernel.checked_def) ->
                   let name = d.Kernel.def.Ast.name in
                   assert_true
                     ("bytecode VM parity for " ^ f ^ ":" ^ name)
                     (String.equal
                        (Bytecode_vm.vm_canonical checked name)
                        (Runtime.value_to_canonical (fst (Runtime.normalize_def checked name)))))
                 checked.Kernel.defs;
               incr covered);
  assert_true
    (Printf.sprintf "bytecode sweep covers a healthy floor of fixtures (%d, need >= 20)" !covered)
    (!covered >= 20)

(* Regression guards for crashes found by the deterministic fuzzer (G3). *)
let () =
  (* "case ofx": find_sub used to match the space inside "case ", driving
     String.sub negative (Invalid_argument). Must now be a structured error. *)
  (match Parser.parse_string "f : Nat\nf = case ofx\n" with
   | _ -> fail "case ofx should be a structured parse error"
   | exception (Parser.Error _ | Kernel.Error _) -> ()
   | exception e -> fail ("case ofx crashed unstructured: " ^ Printexc.to_string e));
  (* JSON round-trip: to_string emits \u00XX for control bytes; parse must
     decode \u (it previously dropped the escape, breaking parse(to_string v)=v). *)
  List.iter
    (fun s ->
      let v = Json.String s in
      assert_true ("json round-trips control bytes: " ^ String.escaped s)
        (Json.parse (Json.to_string v) = v))
    [ "\x01"; "\x1f\x00 ok"; "tab\tnewline\n" ]

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

  let elm_like_path = find_up (Sys.getcwd ()) "examples/elm_like.protoss" in
  let elm_like_equiv_path = find_up (Sys.getcwd ()) "examples/elm_like_equiv.protoss" in
  let basic_protoss_path = find_up (Sys.getcwd ()) "examples/basic.protoss" in
  let basic_pt_path = find_up (Sys.getcwd ()) "examples/basic.pt" in
  let basic_ptc_path = find_up (Sys.getcwd ()) "examples/basic.ptc" in
  let basic_ptb_path = find_up (Sys.getcwd ()) "examples/basic.ptb" in
  let preservation_progression_path =
    find_up (Sys.getcwd ()) "examples/preservation_progression.protoss"
  in
  let preservation_progression_type_error_path =
    find_up (Sys.getcwd ()) "examples/preservation_progression_type_error.protoss"
  in
  let preservation_progression_recursion_error_path =
    find_up (Sys.getcwd ()) "examples/preservation_progression_recursion_error.protoss"
  in
  let basic_protoss = Loader.check_file basic_protoss_path in
  let basic_pt = Loader.check_file basic_pt_path in
  assert_equal ".pt source hashes as .protoss source" (Kernel.hash_program basic_protoss)
    (Kernel.hash_program basic_pt);
  let basic_ptc = Loader.check_file basic_ptc_path in
  assert_equal ".ptc source hashes as .protoss source" (Kernel.hash_program basic_protoss)
    (Kernel.hash_program basic_ptc);
  assert_equal ".ptc fixture matches canonical serialization"
    (Kernel.serialize_checked_program basic_protoss)
    (String.trim (Store.read_file basic_ptc_path));
  let basic_ptb = Loader.check_file basic_ptb_path in
  assert_equal ".ptb source hashes as .protoss source" (Kernel.hash_program basic_protoss)
    (Kernel.hash_program basic_ptb);
  assert_equal ".ptb fixture matches canonical binary serialization"
    (Canonical_binary.checked_to_binary basic_protoss)
    (Store.read_file basic_ptb_path);
  let basic_pt_projection = Ast.string_of_program basic_protoss.Kernel.program in
  assert_equal ".pt projection parses with same hash" (Kernel.hash_program basic_protoss)
    (Kernel.hash_program (check basic_pt_projection));
  let formatter_full_source =
    "(module Demo.App)\n\
     (import \"prelude.protoss\")\n\
     (export Count Pair Tree id askName count main)\n\
     (capabilities Human.ask)\n\
     (type Count Nat)\n\
     (record Pair (params A B) (first A) (second B))\n\
     (variant Tree (params A) (Leaf A) (Node (Tree A)))\n\
     (defpoly id (params A) (-> A A) (lambda (x A) x))\n\
     (defcap askName (capabilities Human.ask) (Process String) (Human.ask \"Name?\"))\n\
     (defrec count (-> Nat Nat) (nat n) (zero 0) (step acc (succ acc)))\n\
     (def main Nat (count 2))"
  in
  let formatter_once = Ast.string_of_program (Parser.parse_string formatter_full_source) in
  let formatter_twice = Ast.string_of_program (Parser.parse_string formatter_once) in
  assert_equal "Protoss/H formatter full grammar idempotent" formatter_once formatter_twice;
  assert_true "Protoss/H formatter keeps module"
    (contains_substring formatter_once "(module Demo.App)");
  assert_true "Protoss/H formatter keeps Process type"
    (contains_substring formatter_once "(Process String)");
  assert_true "Protoss/H formatter keeps capability request"
    (contains_substring formatter_once "(Human.ask \"Name?\")");
  let preservation_progression =
    Loader.check_file preservation_progression_path
  in
  List.iter
    (assert_normalized_value_preserves_declared_type preservation_progression)
    [ "two"; "flag"; "label"; "pair"; "nums"; "choice"; "inc"; "applied" ];
  (try
     ignore (Loader.check_file preservation_progression_type_error_path);
     fail "preservation/progression type-error fixture should be rejected"
   with Loader.Error _ -> ());
  (try
     ignore (Loader.check_file preservation_progression_recursion_error_path);
     fail "preservation/progression recursion fixture should be rejected"
   with Loader.Error _ -> ());
  let elm_like = Loader.check_file elm_like_path in
  let elm_like_equiv = Loader.check_file elm_like_equiv_path in
  assert_equal "Elm-like surface hashes as S-expression surface"
    (Kernel.hash_program elm_like_equiv) (Kernel.hash_program elm_like);
  let inferred_add_surface = check "add a b =\n  a + b\n" in
  assert_equal "Elm-like signature-free Nat add type" "(-> Nat (-> Nat Nat))"
    (Ast.string_of_typ (checked_def inferred_add_surface "add").Kernel.def.typ);
  let inferred_add_equiv =
    check
      "(def add (-> Nat (-> Nat Nat)) \
       (lambda a (lambda b (foldNat a b (lambda acc (succ acc))))))"
  in
  assert_equal "Elm-like signature-free Nat add hash"
    (Kernel.hash_program inferred_add_equiv)
    (Kernel.hash_program inferred_add_surface);
  let inferred_literal_surface = check "total =\n  2 + 5\n" in
  let inferred_literal_value, _ = Runtime.normalize_def inferred_literal_surface "total" in
  assert_equal "Elm-like signature-free Nat literal add normalizes" "7"
    (Runtime.value_to_string inferred_literal_value);
  expect_parse_error_contains "id x =\n  x\n" "missing type signature for id";
  let elm_like_main, _ = Runtime.normalize_def elm_like "main" in
  assert_equal "Elm-like pipeline normalizes" "5"
    (Runtime.value_to_string elm_like_main);
  let elm_like_selected, _ = Runtime.normalize_def elm_like "selected" in
  assert_equal "Elm-like case normalizes" "1"
    (Runtime.value_to_string elm_like_selected);
  let elm_like_layout_nested_case, _ =
    Runtime.normalize_def elm_like "layoutNestedCase"
  in
  assert_equal "Elm-like nested layout case normalizes" "2"
    (Runtime.value_to_string elm_like_layout_nested_case);
  let elm_like_with_let, _ = Runtime.normalize_def elm_like "withLet" in
  assert_equal "Elm-like let normalizes" "5"
    (Runtime.value_to_string elm_like_with_let);
  let elm_like_layout_let_case, _ = Runtime.normalize_def elm_like "layoutLetCase" in
  assert_equal "Elm-like layout let case normalizes" "5"
    (Runtime.value_to_string elm_like_layout_let_case);
  let elm_like_layout_let_function, _ =
    Runtime.normalize_def elm_like "layoutLetFunction"
  in
  assert_equal "Elm-like layout let function normalizes" "8"
    (Runtime.value_to_string elm_like_layout_let_function);
  let elm_like_picked, _ = Runtime.normalize_def elm_like "picked" in
  assert_equal "Elm-like if and multi-parameter lambda normalize" "8"
    (Runtime.value_to_string elm_like_picked);
  let elm_like_nested_picked, _ = Runtime.normalize_def elm_like "nestedPicked" in
  assert_equal "Elm-like nested if normalizes" "3"
    (Runtime.value_to_string elm_like_nested_picked);
  let elm_like_comparison_passed, _ = Runtime.normalize_def elm_like "comparisonPassed" in
  assert_equal "Elm-like Nat equality normalizes" "true"
    (Runtime.value_to_string elm_like_comparison_passed);
  let elm_like_comparison_failed, _ = Runtime.normalize_def elm_like "comparisonFailed" in
  assert_equal "Elm-like Nat inequality normalizes" "true"
    (Runtime.value_to_string elm_like_comparison_failed);
  let elm_like_ordered, _ = Runtime.normalize_def elm_like "ordered" in
  assert_equal "Elm-like boolean and comparison normalizes" "true"
    (Runtime.value_to_string elm_like_ordered);
  let elm_like_not_too_large, _ = Runtime.normalize_def elm_like "notTooLarge" in
  assert_equal "Elm-like boolean not/or normalizes" "true"
    (Runtime.value_to_string elm_like_not_too_large);
  let elm_like_user, _ = Runtime.normalize_def elm_like "user" in
  assert_equal "Elm-like record literal normalizes" "{active = true, name = \"Ada\"}"
    (Runtime.value_to_string elm_like_user);
  let elm_like_user_name, _ = Runtime.normalize_def elm_like "userName" in
  assert_equal "Elm-like field access normalizes" "\"Ada\""
    (Runtime.value_to_string elm_like_user_name);
  let elm_like_renamed_user_name, _ = Runtime.normalize_def elm_like "renamedUserName" in
  assert_equal "Elm-like record update field normalizes" "\"Grace\""
    (Runtime.value_to_string elm_like_renamed_user_name);
  let elm_like_renamed_user_active, _ = Runtime.normalize_def elm_like "renamedUserActive" in
  assert_equal "Elm-like record update preserves fields" "true"
    (Runtime.value_to_string elm_like_renamed_user_active);
  let elm_like_numbers, _ = Runtime.normalize_def elm_like "numbers" in
  assert_equal "Elm-like list literal normalizes" "[1, 2, 3]"
    (Runtime.value_to_string elm_like_numbers);
  let elm_like_number_count, _ = Runtime.normalize_def elm_like "numberCount" in
  assert_equal "Elm-like list literal type inference normalizes" "3"
    (Runtime.value_to_string elm_like_number_count);
  let elm_like_inferred_literal, _ = Runtime.normalize_def elm_like "inferredLiteral" in
  assert_equal "Elm-like inferred literal add normalizes" "7"
    (Runtime.value_to_string elm_like_inferred_literal);
  let elm_like_inferred_total, _ = Runtime.normalize_def elm_like "inferredTotal" in
  assert_equal "Elm-like inferred add call normalizes" "7"
    (Runtime.value_to_string elm_like_inferred_total);

  (* Protoss/H emitter: rendering a program to human syntax and re-parsing it
     must preserve the canonical hash, and the rendering must be idempotent. *)
  let human_projection label source =
    let original = check source in
    let rendered = Surface_syntax.render_program (Parser.parse_string source) in
    let reparsed = check rendered in
    assert_equal (label ^ " hash round-trip") (Kernel.hash_program original)
      (Kernel.hash_program reparsed);
    assert_equal (label ^ " idempotent") rendered
      (Surface_syntax.render_program (Parser.parse_string rendered));
    rendered
  in
  let human_rich_source =
    "(module Demo.Shop)\n\
     (export Status User total fetch)\n\
     (capabilities Http.get)\n\
     (record User (name String) (vip Bool))\n\
     (variant Status (params A) (Open A) (Closed Unit))\n\
     (def total (-> (List Nat) Nat) (lambda (xs (List Nat)) \
     (foldList xs 0 (lambda x (lambda acc (foldNat x acc (lambda n (succ n))))))))\n\
     (defcap fetch (capabilities Http.get) (Process (capabilities Http.get) String) \
     (bind (Http.get \"https://example\") (lambda (r String) (done r))))\n\
     (def user User (record (name \"Ada\") (vip true)))\n\
     (def renamed User (recordUpdate user (name \"Grace\")))\n\
     (def pick (-> Bool Nat) (lambda (f Bool) (case f (true 1) (false 0))))\n\
     (def nums (List Nat) (Cons 1 (Cons 2 Nil)))\n\
     (def hd Nat (caseList nums (Nil 0) (Cons h t h)))\n\
     (def opened (Status Nat) (variant (Status Nat) Open 4))\n\
     (def vipFlag Bool (get user vip))\n\
     (def leadName String (letRecord user ((name n)) n))\n\
     (def scoped Nat (let (a Nat 1) (let (b (succ a)) \
     (foldNat a b (lambda acc (succ acc))))))"
  in
  let human_rich = human_projection "Protoss/H emitter rich program" human_rich_source in
  assert_true "Protoss/H emitter renders if sugar"
    (contains_substring human_rich "if f then 1 else 0");
  assert_true "Protoss/H emitter renders + sugar" (contains_substring human_rich "x + acc");
  assert_true "Protoss/H emitter renders list literal"
    (contains_substring human_rich "[1, 2]");
  assert_true "Protoss/H emitter renders field access"
    (contains_substring human_rich ".vip");
  assert_true "Protoss/H emitter renders record literal"
    (contains_substring human_rich "{ name = \"Ada\", vip = true }");
  assert_true "Protoss/H emitter renders record update"
    (contains_substring human_rich "| name = \"Grace\" }");
  assert_true "Protoss/H emitter renders capability signature"
    (contains_substring human_rich "Process { Http.get } String");
  assert_true "Protoss/H emitter renders union declaration"
    (contains_substring human_rich "type Demo.Shop.Status A = Closed | Open A");
  assert_true "Protoss/H emitter renders list patterns"
    (contains_substring human_rich "Cons h t -> h");
  ignore
    (human_projection "Protoss/H emitter Elm-origin program"
       "double : Nat -> Nat\n\
        double x =\n\
       \    x + x\n\n\
        selected : Nat\n\
        selected =\n\
       \    case true of\n\
       \        true -> double 2\n\
       \        false -> 0\n");
  (try
     ignore
       (Surface_syntax.render_program
          (Parser.parse_string "(defpoly id (params A) (-> A A) (lambda (x A) x))"));
     fail "defpoly should have no Protoss/H projection"
   with Surface_syntax.Unrenderable _ -> ());
  (try
     ignore
       (Surface_syntax.render_program
          (Parser.parse_string "(capabilities Clock.read)\n(def now (Process Nat) (Clock.read))"));
     fail "Clock.read should have no Protoss/H projection"
   with Surface_syntax.Unrenderable _ -> ());
  (* Every example fixture that checks in isolation must either round-trip
     through the Protoss/H projection hash-identically or be explicitly
     unrenderable; the minimum count guards against the loop going hollow. *)
  let examples_dir = Filename.dirname basic_protoss_path in
  let human_round_trips = ref 0 in
  Sys.readdir examples_dir |> Array.to_list |> List.sort String.compare
  |> List.iter (fun name ->
         if Filename.check_suffix name ".protoss" then
           let source = Store.read_file (Filename.concat examples_dir name) in
           match (try Some (check source) with Kernel.Error _ | Parser.Error _ -> None) with
           | None -> () (* imports or intentional error fixtures *)
           | Some original -> (
               match
                 try Some (Surface_syntax.render_program (Parser.parse_string source))
                 with Surface_syntax.Unrenderable _ -> None
               with
               | None -> ()
               | Some rendered ->
                   incr human_round_trips;
                   let reparsed = check rendered in
                   assert_equal ("Protoss/H projection hash round-trip: " ^ name)
                     (Kernel.hash_program original) (Kernel.hash_program reparsed);
                   assert_equal ("Protoss/H projection idempotent: " ^ name) rendered
                     (Surface_syntax.render_program (Parser.parse_string rendered))));
  assert_true "Protoss/H projection covers the example fixtures"
    (!human_round_trips >= 20);
  trace_test "Protoss/H emitter";

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
  let bool_wildcard = check "(def out Nat (match true (true 1) (_ 0)))" in
  assert_equal "match Bool wildcard hashes as explicit case" (Kernel.hash_program bool_case)
    (Kernel.hash_program bool_wildcard);
  let bool_wildcard_out, _ = Runtime.normalize_def bool_wildcard "out" in
  assert_equal "match Bool wildcard normalizes" "1"
    (Runtime.value_to_string bool_wildcard_out);
  let bool_wildcard_only = check "(def out Nat (case false (_ 9)))" in
  let bool_wildcard_only_out, _ = Runtime.normalize_def bool_wildcard_only "out" in
  assert_equal "Bool wildcard-only branch normalizes" "9"
    (Runtime.value_to_string bool_wildcard_only_out);
  expect_check_error_contains
    "(def bad Nat (case true (true 1) (true 2) (false 0)))"
    "Bool case duplicate branch: true";
  expect_check_error_contains
    "(def bad Nat (case false (false 0) (true 1) (false 2)))"
    "Bool case duplicate branch: false";
  expect_check_error_contains
    "(def bad Nat (case true (true 1) (false 0) (_ 2)))"
    "Bool case wildcard branch is unreachable";
  expect_check_error_contains
    "(def bad Nat (case true (_ 1) (_ 2)))"
    "Bool case duplicate wildcard branch";
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
  let list_match_wildcard =
    check
      "(def xs (List Nat) (Cons 1 (Cons 2 Nil)))\n\
       (def first Nat (match xs (Cons head tail head) (_ 0)))"
  in
  let list_case_wildcard =
    check
      "(def xs (List Nat) (Cons 1 (Cons 2 Nil)))\n\
       (def first Nat (caseList xs (Cons head tail head) (_ 0)))"
  in
  assert_equal "match List wildcard hashes as caseList"
    (Kernel.hash_program list_match_explicit)
    (Kernel.hash_program list_match_wildcard);
  assert_equal "caseList wildcard hashes as explicit caseList"
    (Kernel.hash_program list_match_explicit)
    (Kernel.hash_program list_case_wildcard);
  let list_match_wildcard_first, _ = Runtime.normalize_def list_match_wildcard "first" in
  assert_equal "match List wildcard normalizes" "1"
    (Runtime.value_to_string list_match_wildcard_first);
  let list_case_cons_wildcard =
    check
      "(def xs (List Nat) (Cons 1 (Cons 2 Nil)))\n\
       (def out Nat (caseList xs (Nil 0) (_ 9)))"
  in
  let list_case_cons_explicit =
    check
      "(def xs (List Nat) (Cons 1 (Cons 2 Nil)))\n\
       (def out Nat (caseList xs (Nil 0) (Cons head tail 9)))"
  in
  assert_equal "caseList Cons wildcard hashes as explicit caseList"
    (Kernel.hash_program list_case_cons_explicit)
    (Kernel.hash_program list_case_cons_wildcard);
  let list_case_cons_wildcard_out, _ =
    Runtime.normalize_def list_case_cons_wildcard "out"
  in
  assert_equal "caseList Cons wildcard normalizes" "9"
    (Runtime.value_to_string list_case_cons_wildcard_out);
  let list_case_wildcard_only =
    check "(def xs (List Nat) (Nil Nat))\n(def out Nat (caseList xs (_ 7)))"
  in
  let list_case_wildcard_only_explicit =
    check
      "(def xs (List Nat) (Nil Nat))\n\
       (def out Nat (caseList xs (Nil 7) (Cons head tail 7)))"
  in
  assert_equal "caseList wildcard-only hashes as explicit caseList"
    (Kernel.hash_program list_case_wildcard_only_explicit)
    (Kernel.hash_program list_case_wildcard_only);
  let list_case_wildcard_only_out, _ =
    Runtime.normalize_def list_case_wildcard_only "out"
  in
  assert_equal "caseList wildcard-only normalizes" "7"
    (Runtime.value_to_string list_case_wildcard_only_out);
  let list_case_wildcard_capture =
    check
      "(def out Nat \
       (let (__match_head0 8) \
       (let (xs (List Nat) (Cons 1 Nil)) \
       (caseList xs (Nil 0) (_ __match_head0)))))"
  in
  let list_case_wildcard_capture_out, _ =
    Runtime.normalize_def list_case_wildcard_capture "out"
  in
  assert_equal "caseList wildcard generated binders do not capture" "8"
    (Runtime.value_to_string list_case_wildcard_capture_out);
  assert_true "match List has no canonical match node"
    (not (contains_substring (Kernel.serialize_checked_program list_match) "match"));
  expect_parse_error_contains
    "(def xs (List Nat) (Cons 1 Nil))\n\
     (def bad Nat (caseList xs (Nil 0) (Cons head tail head) (_ 2)))"
    "caseList wildcard branch is unreachable";
  expect_parse_error_contains
    "(def xs (List Nat) (Cons 1 Nil))\n\
     (def bad Nat (match xs (Nil 0) (Cons head tail head) (_ 2)))"
    "match wildcard branch is unreachable";
  expect_parse_error_contains
    "(def xs (List Nat) (Cons 1 Nil))\n(def bad Nat (caseList xs (_ 0) (_ 1)))"
    "duplicate caseList wildcard branch";
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
  let graph_stats = Canonical_ir.graph_stats graph_json in
  assert_equal "canonical graph stats defs" "1" (string_of_int graph_stats.Canonical_ir.defs);
  assert_equal "canonical graph stats capabilities" "0"
    (string_of_int graph_stats.Canonical_ir.capabilities);
  assert_true "canonical graph stats nodes" (graph_stats.Canonical_ir.nodes >= 3);
  assert_true "canonical graph stats edges" (graph_stats.Canonical_ir.edges > 0);
  assert_true "canonical graph stats describe"
    (contains_substring (Canonical_ir.describe_graph_stats graph_stats) "Graph stats");
  assert_equal "canonical graph version" Kernel.canonical_graph_version
    (json_string_field "version" graph);
  assert_equal "canonical graph current version" "protoss-canon-graph-v2"
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
  let main_graph_def = Canonical_ir.graph_definition graph_json "main" in
  let graph_roots = Canonical_ir.graph_definitions graph_json in
  assert_equal "canonical graph roots count" "1" (string_of_int (List.length graph_roots));
  assert_true "canonical graph roots describe"
    (contains_substring (Canonical_ir.describe_graph_definitions graph_roots) "name=main");
  assert_equal "canonical graph def name" "main" main_graph_def.Canonical_ir.graph_def_name;
  assert_equal "canonical graph def type ref" top_type_ref
    main_graph_def.Canonical_ir.graph_def_type_ref;
  assert_equal "canonical graph def term ref" top_term_ref
    main_graph_def.Canonical_ir.graph_def_term_ref;
  assert_true "canonical graph def describes"
    (contains_substring (Canonical_ir.describe_graph_definition main_graph_def) "Graph def");
  let main_graph_def_by_id =
    Canonical_ir.graph_definition graph_json main_graph_def.Canonical_ir.graph_def_id
  in
  assert_equal "canonical graph def lookup by def id" "main"
    main_graph_def_by_id.Canonical_ir.graph_def_name;
  let top_type_node = Canonical_ir.graph_node graph_json top_type_ref in
  assert_equal "canonical graph node type kind" "Type" top_type_node.Canonical_ir.node_kind;
  assert_true "canonical graph node type describes"
    (contains_substring (Canonical_ir.describe_graph_node top_type_node) "Graph node");
  let top_term_node = Canonical_ir.graph_node graph_json top_term_ref in
  assert_equal "canonical graph node term kind" "Term" top_term_node.Canonical_ir.node_kind;
  assert_true "canonical graph node term edges" (List.length top_term_node.Canonical_ir.node_edge_refs > 0);
  (try
     ignore (Canonical_ir.graph_node graph_json "p2:missing");
     fail "canonical graph missing node should be rejected"
   with Kernel.Error msg ->
     assert_true "canonical graph reports missing node"
       (contains_substring msg "canonical graph node not found"));
  (try
     ignore (Canonical_ir.graph_definition graph_json "missing");
     fail "canonical graph missing definition should be rejected"
   with Kernel.Error msg ->
     assert_true "canonical graph reports missing definition"
       (contains_substring msg "canonical graph definition not found"));
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
  let legacy_graph_json = Kernel.checked_to_graph_json_legacy_v1 formatted_a in
  let legacy_graph = Json.parse legacy_graph_json in
  assert_equal "legacy canonical graph version" Kernel.canonical_graph_legacy_v1
    (json_string_field "version" legacy_graph);
  assert_equal "legacy canonical graph to program roundtrip" program_canonical
    (Canonical_ir.graph_to_program legacy_graph_json);
  assert_equal "legacy canonical graph checked hash" (Kernel.hash_program formatted_a)
    (Kernel.hash_program (Canonical_ir.checked_of_graph legacy_graph_json));
  let migrated_graph_json = Canonical_ir.migrate_graph legacy_graph_json in
  let migrated_graph = Json.parse migrated_graph_json in
  assert_equal "migrated canonical graph version" Kernel.canonical_graph_version
    (json_string_field "version" migrated_graph);
  assert_equal "migrated canonical graph exact current serialization" graph_json
    migrated_graph_json;
  assert_equal "migrated canonical graph checked hash" (Kernel.hash_program formatted_a)
    (Kernel.hash_program (Canonical_ir.checked_of_graph migrated_graph_json));
  let migrated_current_graph_json = Canonical_ir.migrate_graph graph_json in
  assert_equal "current canonical graph migration is stable" graph_json
    migrated_current_graph_json;
  let graph_value, _ = Runtime.normalize_def graph_checked "main" in
  assert_equal "canonical graph eval" "2" (Runtime.value_to_string graph_value);
  let strict_graph = check "(def main Nat (strict (succ 1)))" in
  let strict_graph_json = Canonical_ir.serialize_graph strict_graph in
  assert_true "strict canonical serialization"
    (contains_substring (Kernel.serialize_checked_program strict_graph) "(strict");
  assert_true "strict graph serialization"
    (contains_substring strict_graph_json "\"tag\": \"Strict\"");
  assert_true "strict graph definition describes term"
    (contains_substring
       (Canonical_ir.graph_definition strict_graph_json "main").Canonical_ir.graph_def_term_canonical
       "(strict");
  assert_equal "strict graph to program roundtrip"
    (Kernel.serialize_checked_program strict_graph)
    (Canonical_ir.graph_to_program strict_graph_json);
  let strict_graph_checked = Canonical_ir.checked_of_graph strict_graph_json in
  let strict_graph_value, _ = Runtime.normalize_def strict_graph_checked "main" in
  assert_equal "strict graph eval" "2" (Runtime.value_to_string strict_graph_value);
  let basic_path = find_up (Sys.getcwd ()) "examples/basic.protoss" in
  let basic_invariants = Invariants.check_file basic_path in
  assert_equal "invariants file hash" (Kernel.hash_program (Loader.check_file basic_path))
    basic_invariants.Invariants.program_hash;
  assert_true "invariants file checks graph migration"
    basic_invariants.Invariants.graph_migration;
  assert_true "invariants file describes graph migration"
    (contains_substring (Invariants.describe_file basic_invariants) "graph_migration=ok");
  let invariants_graph_dir = temp_dir "invariants-graph" in
  ensure_dir invariants_graph_dir;
  let invariants_graph_path = Filename.concat invariants_graph_dir "basic.graph.json" in
  write_file invariants_graph_path (Canonical_ir.serialize_graph (Loader.check_file basic_path));
  let graph_invariants = Invariants.check_graph invariants_graph_path in
  assert_equal "invariants graph hash" basic_invariants.program_hash
    graph_invariants.Invariants.program_hash;
  assert_true "invariants graph checks graph migration"
    graph_invariants.Invariants.graph_migration;
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
  let dep_graph_edges = Canonical_ir.graph_dependencies dep_graph_json in
  assert_equal "canonical graph dependency edge count" "1"
    (string_of_int (List.length dep_graph_edges));
  assert_true "canonical graph dependency describes def"
    (contains_substring (Canonical_ir.describe_graph_dependencies dep_graph_edges) "def=three");
  assert_true "canonical graph dependency describes dep"
    (contains_substring (Canonical_ir.describe_graph_dependencies dep_graph_edges) "depends_on=two");
  assert_equal "canonical graph dependency filtered edge count" "1"
    (string_of_int (List.length (Canonical_ir.graph_dependencies_for dep_graph_json "three")));
  assert_equal "canonical graph dependency leaf edge count" "0"
    (string_of_int (List.length (Canonical_ir.graph_dependencies_for dep_graph_json "two")));
  let agent_source = Canonical_ir.agent_graph_source "test" "dep_graph" in
  let dep_graph = Json.parse dep_graph_json in
  let agent_summary =
    Json.parse (Canonical_ir.agent_graph_summary_json ~source:agent_source dep_graph_json)
  in
  assert_equal "agent graph summary format" "protoss-agent-graph-v1"
    (json_string_field "format" agent_summary);
  assert_equal "agent graph summary query" "summary" (json_string_field "query" agent_summary);
  assert_equal "agent graph summary source kind" "test"
    (json_string_field "kind" (json_field "source" agent_summary));
  assert_equal "agent graph summary graph hash" (json_string_field "graphHash" dep_graph)
    (json_string_field "graphHash" agent_summary);
  let agent_stats = json_field "stats" agent_summary in
  assert_equal "agent graph summary definition count" "2"
    (string_of_int (json_nat_field "definitions" agent_stats));
  assert_true "agent graph summary includes three"
    (json_array_field "definitions" agent_summary
    |> List.exists (fun def -> String.equal "three" (json_string_field "name" def)));
  assert_equal "agent graph summary dependency count" "1"
    (string_of_int (List.length (json_array_field "dependencies" agent_summary)));
  let agent_def = Json.parse (Canonical_ir.agent_graph_definition_json dep_graph_json "three") in
  let agent_def_body = json_field "definition" agent_def in
  assert_equal "agent graph def query" "definition" (json_string_field "query" agent_def);
  assert_equal "agent graph def name" "three" (json_string_field "name" agent_def_body);
  assert_equal "agent graph def deps" "two"
    (String.concat "," (json_string_array_field "deps" agent_def_body));
  let dep_three_term_ref = json_string_field "termRef" (graph_def dep_graph "three") in
  let agent_node = Json.parse (Canonical_ir.agent_graph_node_json dep_graph_json dep_three_term_ref) in
  let agent_node_body = json_field "node" agent_node in
  assert_equal "agent graph node query" "node" (json_string_field "query" agent_node);
  assert_equal "agent graph node id" dep_three_term_ref (json_string_field "id" agent_node_body);
  assert_equal "agent graph node kind" "Term" (json_string_field "kind" agent_node_body);
  assert_true "agent graph node edge refs"
    (json_string_array_field "edgeRefs" agent_node_body <> []);
  let agent_deps =
    Json.parse (Canonical_ir.agent_graph_dependencies_json dep_graph_json (Some "three"))
  in
  assert_equal "agent graph deps filter" "three" (json_string_field "id" agent_deps);
  let agent_dep = List.hd (json_array_field "dependencies" agent_deps) in
  assert_equal "agent graph dep target" "two" (json_string_field "dependsOn" agent_dep);
  let agent_explain =
    Json.parse (Canonical_ir.agent_graph_definition_explanation_json dep_graph_json "three")
  in
  assert_equal "agent graph explanation query" "definition-explanation"
    (json_string_field "query" agent_explain);
  assert_equal "agent graph explanation def" "three"
    (json_string_field "name" (json_field "definition" agent_explain));
  assert_equal "agent graph explanation type node" "Type"
    (json_string_field "kind" (json_field "typeNode" agent_explain));
  assert_equal "agent graph explanation term node" "Term"
    (json_string_field "kind" (json_field "termNode" agent_explain));
  assert_equal "agent graph explanation dependency count" "1"
    (string_of_int (List.length (json_array_field "dependencies" agent_explain)));
  assert_true "agent graph explanation notes dependency"
    (List.exists
       (fun note -> contains_substring note "Depends on: two")
       (json_string_array_field "notes" agent_explain));
  let nested_old_model =
    Ast.TRecord
      (Ast.sort_fields
         [
           ("count", Ast.TNat);
           ("prefs", Ast.TRecord (Ast.sort_fields [ ("theme", Ast.TString) ]));
         ])
  in
  let nested_new_model =
    Ast.TRecord
      (Ast.sort_fields
         [
           ("count", Ast.TNat);
           ( "prefs",
             Ast.TRecord
               (Ast.sort_fields [ ("density", Ast.TNat); ("theme", Ast.TString) ]) );
         ])
  in
  let nested_migration_expr, nested_migration_strategies =
    Agent_protocol.migration_expr_source nested_old_model nested_new_model
  in
  assert_true "agent nested migration copies nested field"
    (contains_substring nested_migration_expr
       "(theme (get (get old prefs) theme))");
  assert_true "agent nested migration defaults nested field"
    (contains_substring nested_migration_expr "(density 0)");
  assert_equal "agent nested migration strategies" "copy,default,nested"
    (String.concat "," nested_migration_strategies);
  let nested_migration_checked =
    check
      ("(def old " ^ Ast.string_of_typ nested_old_model
     ^ " (record (count 4) (prefs (record (theme \"dark\")))))\n\
        (def migrate "
      ^ Ast.string_of_typ (Ast.TFun (nested_old_model, nested_new_model))
      ^ " " ^ nested_migration_expr ^ ")\n\
        (def migrated "
      ^ Ast.string_of_typ nested_new_model ^ " (migrate old))")
  in
  let nested_migrated, _ = Runtime.normalize_def nested_migration_checked "migrated" in
  assert_equal "agent nested migration normalizes"
    "{count = 4, prefs = {density = 0, theme = \"dark\"}}"
    (Runtime.value_to_string nested_migrated);
  let mcp_response request =
    match Mcp_server.handle_message request with
    | Some response -> Json.parse response
    | None -> fail "MCP request unexpectedly returned no response"
  in
  let mcp_init =
    mcp_response
      "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"initialize\",\"params\":{\"protocolVersion\":\"2025-11-25\",\"capabilities\":{},\"clientInfo\":{\"name\":\"test\",\"version\":\"1\"}}}"
  in
  let mcp_init_result = json_field "result" mcp_init in
  assert_equal "mcp initialize protocol" "2025-11-25"
    (json_string_field "protocolVersion" mcp_init_result);
  let mcp_tools =
    mcp_response "{\"jsonrpc\":\"2.0\",\"id\":2,\"method\":\"tools/list\",\"params\":{}}"
  in
  let mcp_tool_names =
    json_array_field "tools" (json_field "result" mcp_tools)
    |> List.map (json_string_field "name")
  in
  List.iter
    (fun name ->
      assert_true ("mcp exposes " ^ name) (List.exists (String.equal name) mcp_tool_names))
    [
      "protoss.query";
      "protoss.readNode";
      "protoss.renderView";
      "protoss.proposePatch";
      "protoss.checkPatch";
      "protoss.applyPatch";
      "protoss.runHarness";
      "protoss.explain";
      "protoss.normalize";
      "protoss.diff";
      "protoss.rollback";
    ];
  let mcp_graph_dir = temp_dir "mcp-graph" in
  ensure_dir mcp_graph_dir;
  let mcp_graph_path = Filename.concat mcp_graph_dir "graph.json" in
  write_file mcp_graph_path dep_graph_json;
  let mcp_query =
    mcp_response
      ("{\"jsonrpc\":\"2.0\",\"id\":3,\"method\":\"tools/call\",\"params\":{\"name\":\"protoss.query\",\"arguments\":{\"graphPath\":"
      ^ Ast.quote mcp_graph_path ^ ",\"query\":\"definitions\"}}}")
  in
  let mcp_query_result = json_field "result" mcp_query in
  assert_true "mcp query result is not error" (not (json_bool_field "isError" mcp_query_result));
  let mcp_query_structured = json_field "structuredContent" mcp_query_result in
  assert_equal "mcp query structured format" "protoss-agent-graph-v1"
    (json_string_field "format" mcp_query_structured);
  assert_equal "mcp query structured query" "definitions"
    (json_string_field "query" mcp_query_structured);
  let mcp_harness_root = temp_dir "mcp-harness" in
  ensure_dir (Filename.concat mcp_harness_root "src");
  ensure_dir (Filename.concat mcp_harness_root "harness");
  write_file (Filename.concat mcp_harness_root "protoss.toml")
    "name = \"mcp-harness\"\n\
     version = \"0.1.0\"\n\
     entrypoints = [\"src/main.protoss\"]\n\
     stdlib = \"none\"\n\
     source_dirs = [\"src\"]\n\
     store_dir = \".protoss/store\"\n\
     cache_dir = \".protoss/cache\"\n";
  write_file (Filename.concat mcp_harness_root "src/main.protoss") "(def two Nat 2)\n";
  let mcp_harness_path = Filename.concat mcp_harness_root "harness/smoke.pth" in
  write_file mcp_harness_path "harness two_ok = unit two == 2\n";
  let mcp_harness_manifest = Workspace.parse_manifest mcp_harness_root in
  ignore (Workspace.build mcp_harness_manifest);
  let mcp_harness =
    mcp_response
      ("{\"jsonrpc\":\"2.0\",\"id\":4,\"method\":\"tools/call\",\"params\":{\"name\":\"protoss.runHarness\",\"arguments\":{\"store\":"
      ^ Ast.quote (Workspace.store_root mcp_harness_manifest)
      ^ ",\"harnessPath\":" ^ Ast.quote mcp_harness_path ^ "}}}")
  in
  let mcp_harness_result = json_field "result" mcp_harness in
  assert_true "mcp runHarness result is not error"
    (not (json_bool_field "isError" mcp_harness_result));
  let mcp_harness_structured = json_field "structuredContent" mcp_harness_result in
  assert_equal "mcp runHarness structured format" Harness.format
    (json_string_field "format" mcp_harness_structured);
  assert_equal "mcp runHarness structured status" "pass"
    (json_string_field "status" mcp_harness_structured);
  (* Injection guard: protoss.applyPatch routes through the validated commit
     path (Agent_protocol.commit_patch_json), which requires a harness. A
     well-formed patch submitted without one is refused — check cannot be
     bypassed through MCP. *)
  let mcp_patch_path = Filename.concat mcp_harness_root "add_three.json" in
  write_file mcp_patch_path
    "{\"op\":\"AddDef\",\"name\":\"three\",\"deps\":[\"two\"],\"type\":\"Nat\",\"expr\":[\"succ\",\"two\"]}";
  let mcp_apply_noharness =
    mcp_response
      ("{\"jsonrpc\":\"2.0\",\"id\":5,\"method\":\"tools/call\",\"params\":{\"name\":\"protoss.applyPatch\",\"arguments\":{\"store\":"
      ^ Ast.quote (Workspace.store_root mcp_harness_manifest)
      ^ ",\"patchPath\":" ^ Ast.quote mcp_patch_path ^ "}}}")
  in
  assert_true "mcp applyPatch without a harness is refused (no check bypass)"
    (json_bool_field "isError" (json_field "result" mcp_apply_noharness));
  let duplicate_ref_checked = check "(def a Nat 1)\n(def b Nat 1)\n(def c Nat b)" in
  let duplicate_ref_graph_json = Canonical_ir.serialize_graph duplicate_ref_checked in
  let duplicate_ref_roundtrip =
    Canonical_ir.serialize_graph (Canonical_ir.checked_of_graph duplicate_ref_graph_json)
  in
  assert_equal "canonical graph duplicate DefId deps roundtrip" duplicate_ref_graph_json
    duplicate_ref_roundtrip;
  assert_equal "canonical graph duplicate DefId deps representative" "a"
    (String.concat ","
       (Canonical_ir.graph_definition duplicate_ref_graph_json "c").Canonical_ir.graph_def_deps);
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
  let defrec_nat_termination = Kernel.termination_explanation_text defrec_nat "count" in
  assert_true "termination explanation names definition"
    (contains_substring defrec_nat_termination "definition=count");
  assert_true "termination explanation reports foldNat"
    (contains_substring defrec_nat_termination "foldNat=1");
  assert_true "termination explanation reports structural status"
    (contains_substring defrec_nat_termination "status=structural-fold");
  assert_true "termination explanation reports static type nodes"
    (contains_substring defrec_nat_termination "staticTypeNodes=3");
  assert_true "termination explanation reports static arity"
    (contains_substring defrec_nat_termination "staticArrowArity=1");
  assert_true "termination explanation reports Nat static size"
    (contains_substring defrec_nat_termination "staticSizedArguments=arg0:Nat.value");
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
  let defrecpoly_list =
    check
      "(defrecpoly copy (params A) (-> (List A) (List A)) \
       (list xs) (nil (Nil A)) (cons x acc (Cons A x acc)))\n\
       (def ns (List Nat) (Cons 1 (Cons 2 Nil)))\n\
       (def ss (List String) (Cons \"a\" Nil))\n\
       (def outN (List Nat) (copy ns))\n\
       (def outS (List String) (copy ss))"
  in
  let defrecpoly_list_explicit =
    check
      "(defpoly copy (params A) (-> (List A) (List A)) \
       (lambda (xs (List A)) \
       (foldList xs (Nil A) \
       (lambda (x A) (lambda (acc (List A)) (Cons A x acc))))))\n\
       (def ns (List Nat) (Cons 1 (Cons 2 Nil)))\n\
       (def ss (List String) (Cons \"a\" Nil))\n\
       (def outN (List Nat) (copy ns))\n\
       (def outS (List String) (copy ss))"
  in
  assert_equal "defrecpoly List desugars to polymorphic foldList"
    (Kernel.hash_program defrecpoly_list_explicit)
    (Kernel.hash_program defrecpoly_list);
  let copied_ns, _ = Runtime.normalize_def defrecpoly_list "outN" in
  assert_equal "defrecpoly List Nat normalization" "[1, 2]"
    (Runtime.value_to_string copied_ns);
  let copied_ss, _ = Runtime.normalize_def defrecpoly_list "outS" in
  assert_equal "defrecpoly List String normalization" "[\"a\"]"
    (Runtime.value_to_string copied_ss);
  let defrecpoly_alpha_a =
    check
      "(defrecpoly copy (params A) (-> (List A) (List A)) \
       (list xs) (nil (Nil A)) (cons x acc (Cons A x acc)))"
  in
  let defrecpoly_alpha_b =
    check
      "(defrecpoly copy (params B) (-> (List B) (List B)) \
       (list xs) (nil (Nil B)) (cons x acc (Cons B x acc)))"
  in
  assert_equal "defrecpoly type parameter alpha-stable hash"
    (Kernel.hash_program defrecpoly_alpha_a)
    (Kernel.hash_program defrecpoly_alpha_b);
  expect_parse_error "(defrecpoly bad (params) (-> Nat Nat) (nat n) (zero 0) (step acc acc))";
  expect_check_error
    "(defrecpoly bad (params A) (-> (List A) Nat) \
     (list xs) (nil 0) (cons x acc (bad xs)))";
  let tree_rec_base =
    "(variant Tree (params A) \
     (Leaf A) \
     (Node (Record (left (Tree A)) (right (Tree A)))))\n\
     (def leaf (Tree Nat) (variant Leaf 1))\n\
     (def tree (Tree Nat) \
       (variant Node (record (left leaf) (right (variant Leaf 2)))))\n\
     (def add (-> Nat (-> Nat Nat)) \
       (lambda (a Nat) (lambda (b Nat) (foldNat a b (lambda (x Nat) (succ x))))))\n"
  in
  let defrec_variant =
    check
      (tree_rec_base
     ^ "(defrec sizeRec (-> (Tree Nat) Nat) \
          (variant t) \
          (Leaf n 1) \
          (Node pair ((add (recur (get pair left))) (recur (get pair right)))))\n\
        (def out Nat (sizeRec tree))")
  in
  let defrec_variant_explicit =
    check
      (tree_rec_base
     ^ "(def sizeRec (-> (Tree Nat) Nat) \
          (lambda (t (Tree Nat)) \
            (foldVariant (Tree Nat) Nat t \
              (Leaf n 1) \
              (Node pair ((add (recur (get pair left))) (recur (get pair right)))))))\n\
        (def out Nat (sizeRec tree))")
  in
  assert_equal "defrec Variant desugars to foldVariant"
    (Kernel.hash_program defrec_variant_explicit)
    (Kernel.hash_program defrec_variant);
  let tree_size, _ = Runtime.normalize_def defrec_variant "out" in
  assert_equal "defrec Variant normalization" "2" (Runtime.value_to_string tree_size);
  let defrec_variant_termination =
    Kernel.termination_explanation_text defrec_variant "sizeRec"
  in
  assert_true "termination explanation reports recursive variant static size"
    (contains_substring defrec_variant_termination "staticSizedArguments=arg0:Tree.height");
  let nested_tree_rec =
    check
      ("(variant DeepTree \
          (DeepLeaf Nat) \
          (DeepNode (Record (children (Record (left DeepTree) (right DeepTree))) (label String))))\n\
        (def deepLeaf DeepTree (variant DeepLeaf 1))\n\
        (def deepTree DeepTree \
          (variant DeepNode \
            (record \
              (children (record (left deepLeaf) (right (variant DeepLeaf 2)))) \
              (label \"root\"))))\n\
        (def add (-> Nat (-> Nat Nat)) \
          (lambda (a Nat) (lambda (b Nat) (foldNat a b (lambda (x Nat) (succ x))))))\n\
        (defrec deepSize (-> DeepTree Nat) \
          (variant value) \
          (DeepLeaf n 1) \
          (DeepNode node \
            ((add (recur (get (get node children) left))) \
              (recur (get (get node children) right)))))\n\
        (def deepOut Nat (deepSize deepTree))")
  in
  let nested_tree_size, _ = Runtime.normalize_def nested_tree_rec "deepOut" in
  assert_equal "defrec nested record subterm recursion" "2"
    (Runtime.value_to_string nested_tree_size);
  let forest_rec_base =
    "(variant Forest \
       (Leaf Nat) \
       (Many (List Forest)))\n\
     (def add (-> Nat (-> Nat Nat)) \
       (lambda (a Nat) (lambda (b Nat) (foldNat a b (lambda (x Nat) (succ x))))))\n\
     (def leaf Forest (variant Leaf 1))\n\
     (def forest Forest \
       (variant Many \
         (Cons Forest leaf \
           (Cons Forest (variant Many (Cons Forest leaf Nil)) Nil))))\n"
  in
  let forest_rec =
    check
      (forest_rec_base
     ^ "(defrec sizeForest (-> Forest Nat) \
          (variant value) \
          (Leaf n 1) \
          (Many children \
            (foldList children 0 \
              (lambda (child Forest) \
                (lambda (acc Nat) ((add (recur child)) acc))))))\n\
        (def forestSize Nat (sizeForest forest))")
  in
  let forest_size, _ = Runtime.normalize_def forest_rec "forestSize" in
  assert_equal "defrec Variant list payload recursion" "2"
    (Runtime.value_to_string forest_size);
  let forest_record_item_rec =
    check
      (forest_rec_base
     ^ "(variant ForestBox \
          (LeafBox Nat) \
          (ManyBox (List (Record (child ForestBox)))))\n\
        (def leafBox ForestBox (variant LeafBox 1))\n\
        (def forestRefs ForestBox \
          (variant ManyBox \
            (Cons (Record (child ForestBox)) \
              (record (child leafBox)) \
              (Cons (Record (child ForestBox)) (record (child leafBox)) Nil))))\n\
        (defrec sizeForestRefs (-> ForestBox Nat) \
          (variant value) \
          (LeafBox n 1) \
          (ManyBox refs \
            (foldList refs 0 \
              (lambda (ref (Record (child ForestBox))) \
                (lambda (acc Nat) ((add (recur (get ref child))) acc))))))\n\
        (def forestRefsSize Nat (sizeForestRefs forestRefs))")
  in
  let forest_refs_size, _ = Runtime.normalize_def forest_record_item_rec "forestRefsSize" in
  assert_equal "defrec Variant list record field recursion" "2"
    (Runtime.value_to_string forest_refs_size);
  expect_check_error_contains
    (forest_rec_base
   ^ "(def bad Nat \
        (foldVariant Forest Nat forest \
          (Leaf n 1) \
          (Many children \
            (let (other (Cons Forest leaf Nil)) \
              (foldList other 0 \
                (lambda (child Forest) \
                  (lambda (acc Nat) (recur child))))))))")
    "recur";
  let defrecpoly_variant =
    check
      (tree_rec_base
     ^ "(defrecpoly sizeGeneric (params A) (-> (Tree A) Nat) \
          (variant t) \
          (Leaf value 1) \
          (Node pair ((add (recur (get pair left))) (recur (get pair right)))))\n\
        (def outGeneric Nat (sizeGeneric tree))")
  in
  let defrecpoly_variant_explicit =
    check
      (tree_rec_base
     ^ "(defpoly sizeGeneric (params A) (-> (Tree A) Nat) \
          (lambda (t (Tree A)) \
            (foldVariant (Tree A) Nat t \
              (Leaf value 1) \
              (Node pair ((add (recur (get pair left))) (recur (get pair right)))))))\n\
        (def outGeneric Nat (sizeGeneric tree))")
  in
  assert_equal "defrecpoly Variant desugars to polymorphic foldVariant"
    (Kernel.hash_program defrecpoly_variant_explicit)
    (Kernel.hash_program defrecpoly_variant);
  let generic_tree_size, _ = Runtime.normalize_def defrecpoly_variant "outGeneric" in
  assert_equal "defrecpoly Variant normalization" "2"
    (Runtime.value_to_string generic_tree_size);
  expect_parse_error
    (tree_rec_base ^ "(defrec bad (-> (Tree Nat) Nat) (variant t))");
  expect_check_error
    (tree_rec_base
   ^ "(defrec bad (-> (Tree Nat) Nat) \
        (variant t) \
        (Leaf n 1) \
        (Node pair (recur tree)))");
  expect_parse_error "(defrec bad Nat (nat n) (zero 0) (step acc acc))";
  expect_parse_error "(defrec bad (-> Nat Nat) (zero 0) (step acc acc))";
  expect_check_error
    "(defrec bad (-> Nat Nat) (nat n) (zero 0) (step acc (bad acc)))";

  let productive_stream =
    check
      "(def nats (Stream Nat) \
       (coiter Nat Nat 1 \
       (lambda (n Nat) (record (head n) (state (succ n))))))\n\
       (def first Nat (streamHead nats))\n\
       (def second Nat (streamHead (streamTail nats)))\n\
       (def firstThree (List Nat) (streamTake 3 nats))"
  in
  let first, _ = Runtime.normalize_def productive_stream "first" in
  assert_equal "productive stream head normalizes" "1" (Runtime.value_to_string first);
  let second, _ = Runtime.normalize_def productive_stream "second" in
  assert_equal "productive stream tail head normalizes" "2"
    (Runtime.value_to_string second);
  let first_three, _ = Runtime.normalize_def productive_stream "firstThree" in
  assert_equal "productive stream take normalizes" "[1, 2, 3]"
    (Runtime.value_to_string first_three);
  assert_true "productive stream appears in canonical program"
    (contains_substring (Kernel.serialize_checked_program productive_stream) "(coiter");
  expect_check_error
    "(def bad (Stream Nat) \
       (coiter Nat Nat 0 \
       (lambda (n Nat) (record (head n) (next (succ n))))))";

  let productive_automaton =
    check
      "(def counter (Automaton Nat Nat) \
       (automaton Nat Nat 0 \
       (lambda (state Nat) (record (output state) (state (succ state))))))\n\
       (def outputs (List Nat) (automatonRun 4 counter))"
  in
  let outputs, _ = Runtime.normalize_def productive_automaton "outputs" in
  assert_equal "productive automaton run normalizes" "[0, 1, 2, 3]"
    (Runtime.value_to_string outputs);
  assert_true "productive automaton appears in canonical program"
    (contains_substring (Kernel.serialize_checked_program productive_automaton) "(automaton");

  let image_view =
    check
      "(def hero (View (Variant (Open Unit))) \
       (image \"https://example.com/hero.jpg\" \"Hero\"))"
  in
  let hero, _ = Runtime.normalize_def image_view "hero" in
  assert_equal "image view normalization"
    "(image \"https://example.com/hero.jpg\" \"Hero\")"
    (Runtime.value_to_string hero);

  (* Raw HTML escape hatch: node/attr/on primitives and the Attr type. *)
  let node_sexp =
    check
      "(def page (View (Variant (Click Unit))) \
       (node \"div\" \
       (Cons (Attr (Variant (Click Unit))) (attr \"class\" \"card\") \
       (Cons (Attr (Variant (Click Unit))) (on \"click\" (variant (Variant (Click Unit)) Click unit)) \
       (Nil (Attr (Variant (Click Unit)))))) \
       (Cons (View (Variant (Click Unit))) (text \"hi\") \
       (Nil (View (Variant (Click Unit)))))))"
  in
  let node_elm =
    check
      "page : View (Variant (Click Unit))\npage =\n  node \"div\" [ attr \"class\" \"card\", on \"click\" (variant (Variant (Click Unit)) Click unit) ] [ text \"hi\" ]\n"
  in
  assert_equal "node Elm-like surface hashes as S-expression surface"
    (Kernel.hash_program node_sexp) (Kernel.hash_program node_elm);
  let node_page, _ = Runtime.normalize_def node_sexp "page" in
  assert_equal "node view normalization"
    "(node \"div\" [(attr \"class\" \"card\"), (on \"click\" Click unit)] [(text \"hi\")])"
    (Runtime.value_to_string node_page);
  assert_true "node is a canonical primitive (not desugared away)"
    (contains_substring (Kernel.serialize_checked_program node_sexp) "node");
  (* children must be List (View msg): an Attr where a View child is expected is rejected *)
  expect_check_error
    "(def page (View (Variant (Click Unit))) \
     (node \"div\" (Nil (Attr (Variant (Click Unit)))) \
     (Cons (Attr (Variant (Click Unit))) (attr \"x\" \"y\") (Nil (Attr (Variant (Click Unit)))))))";
  (* on must produce the node's own msg type: a foreign variant is rejected *)
  expect_check_error
    "(def page (View (Variant (Click Unit))) \
     (node \"div\" \
     (Cons (Attr (Variant (Click Unit))) (on \"click\" (variant (Variant (Other Unit)) Other unit)) \
     (Nil (Attr (Variant (Click Unit))))) \
     (Nil (View (Variant (Click Unit))))))";

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
  let tuple_sugar =
    check
      "(def pair (Tuple Nat String) (tuple 3 \"Ada\"))\n\
       (def out Nat (match pair ((tuple count name) count)))"
  in
  let tuple_explicit =
    check
      "(def pair (Record (_1 Nat) (_2 String)) (record (_1 3) (_2 \"Ada\")))\n\
       (def out Nat (letRecord pair ((_1 count) (_2 name)) count))"
  in
  assert_equal "tuple sugar hashes as canonical record"
    (Kernel.hash_program tuple_explicit)
    (Kernel.hash_program tuple_sugar);
  let tuple_out, _ = Runtime.normalize_def tuple_sugar "out" in
  assert_equal "tuple match normalizes" "3" (Runtime.value_to_string tuple_out);
  assert_true "tuple is surface-only canonical syntax"
    (not (contains_substring (Kernel.serialize_checked_program tuple_sugar) "Tuple"));
  assert_true "tuple match has no canonical match node"
    (not (contains_substring (Kernel.serialize_checked_program tuple_sugar) "match"));
  expect_parse_error "(def bad (Tuple Nat) (tuple 1))";
  expect_parse_error "(def bad Nat (match (tuple 1 2) ((tuple a a) a)))";
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
  let record_update =
    check
      "(def p (Record (active Bool) (name String)) (record (active true) (name \"Ada\")))\n\
       (def updated (Record (active Bool) (name String)) (recordUpdate p (name \"Grace\")))\n\
       (def out Bool (get updated active))"
  in
  let record_update_out, _ = Runtime.normalize_def record_update "out" in
  assert_equal "recordUpdate preserves fields" "true"
    (Runtime.value_to_string record_update_out);
  assert_true "recordUpdate is surface-only canonical syntax"
    (not (contains_substring (Kernel.serialize_checked_program record_update) "recordUpdate"));
  expect_parse_error
    "(def p (Record (name String)) (record (name \"Ada\")))\n\
     (def bad (Record (name String)) (recordUpdate p (name \"Grace\") (name \"Ada\")))";
  expect_check_error
    "(def p (Record (name String)) (record (name \"Ada\")))\n\
     (def bad (Record (name String)) (recordUpdate p (missing \"Grace\")))";
  expect_check_error "(def bad Nat (recordUpdate 1 (count 2)))";

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
  let maybe_wildcard =
    check
      "(type Maybe (A) (Variant (None Unit) (Some A)))\n\
       (def value (Maybe Nat) (variant Some 4))\n\
       (def out Nat (match value (Some n n) (_ 0)))"
  in
  assert_equal "match Variant wildcard hashes as explicit case"
    (Kernel.hash_program maybe_unit_branch_shorthand)
    (Kernel.hash_program maybe_wildcard);
  let maybe_wildcard_out, _ = Runtime.normalize_def maybe_wildcard "out" in
  assert_equal "match Variant wildcard normalizes" "4"
    (Runtime.value_to_string maybe_wildcard_out);
  let maybe_wildcard_only =
    check
      "(type Maybe (A) (Variant (None Unit) (Some A)))\n\
       (def value (Maybe Nat) (variant None unit))\n\
       (def out Nat (case value (_ 7)))"
  in
  let maybe_wildcard_only_out, _ = Runtime.normalize_def maybe_wildcard_only "out" in
  assert_equal "Variant wildcard-only branch normalizes" "7"
    (Runtime.value_to_string maybe_wildcard_only_out);
  let variant_record_payload_match =
    check
      "(variant LeadEvent (Lead (Record (name String) (status String))))\n\
       (def event LeadEvent (variant Lead (record (name \"Ada\") (status \"open\"))))\n\
       (def out String (match event (Lead (record name (status s)) s)))"
  in
  let variant_record_payload_explicit =
    check
      "(variant LeadEvent (Lead (Record (name String) (status String))))\n\
       (def event LeadEvent (variant Lead (record (name \"Ada\") (status \"open\"))))\n\
       (def out String (case event (Lead payload (letRecord payload (name (status s)) s))))"
  in
  assert_equal "match Variant record payload hashes as case plus letRecord"
    (Kernel.hash_program variant_record_payload_explicit)
    (Kernel.hash_program variant_record_payload_match);
  let record_payload_out, _ = Runtime.normalize_def variant_record_payload_match "out" in
  assert_equal "match Variant record payload normalizes" "\"open\""
    (Runtime.value_to_string record_payload_out);
  assert_true "match Variant record payload has no canonical match node"
    (not
       (contains_substring
          (Kernel.serialize_checked_program variant_record_payload_match)
          "match"));
  let variant_tuple_payload_match =
    check
      "(variant PairMsg (Pair (Tuple Nat String)))\n\
       (def msg PairMsg (variant Pair (tuple 7 \"score\")))\n\
       (def out Nat (match msg (Pair (tuple n label) n)))"
  in
  let variant_tuple_payload_explicit =
    check
      "(variant PairMsg (Pair (Record (_1 Nat) (_2 String))))\n\
       (def msg PairMsg (variant Pair (record (_1 7) (_2 \"score\"))))\n\
       (def out Nat (case msg (Pair payload (letRecord payload ((_1 n) (_2 label)) n))))"
  in
  assert_equal "match Variant tuple payload hashes as canonical record payload"
    (Kernel.hash_program variant_tuple_payload_explicit)
    (Kernel.hash_program variant_tuple_payload_match);
  let tuple_payload_out, _ = Runtime.normalize_def variant_tuple_payload_match "out" in
  assert_equal "match Variant tuple payload normalizes" "7"
    (Runtime.value_to_string tuple_payload_out);
  expect_parse_error
    "(variant PairMsg (Pair (Tuple Nat Nat)))\n\
     (def msg PairMsg (variant Pair (tuple 1 2)))\n\
     (def out Nat (match msg (Pair (tuple a a) a)))";
  let maybe_out, _ = Runtime.normalize_def maybe_alias "out" in
  assert_equal "parametric type alias runtime" "4" (Runtime.value_to_string maybe_out);
  let maybe_short_out, _ = Runtime.normalize_def maybe_unit_branch_shorthand "out" in
  assert_equal "unit variant branch shorthand runtime" "4" (Runtime.value_to_string maybe_short_out);
  expect_check_error_contains
    "(variant Maybe (params A) (None Unit) (Some A))\n\
     (def value (Maybe Nat) (variant Some 1))\n\
     (def bad Nat (case value (None 0) (Some n n) (Some m m)))"
    "Variant case duplicate branch: Some";
  expect_check_error_contains
    "(variant Maybe (params A) (None Unit) (Some A))\n\
     (def value (Maybe Nat) (variant Some 1))\n\
     (def bad Nat (case value (None 0) (Some n n) (_ 2)))"
    "Variant case wildcard branch is unreachable";
  expect_check_error_contains
    "(variant Maybe (params A) (None Unit) (Some A))\n\
     (def value (Maybe Nat) (variant Some 1))\n\
     (def bad Nat (case value (_ 0) (_ 1)))"
    "Variant case duplicate wildcard branch";
  expect_check_error_contains
    "(variant Maybe (params A) (None Unit) (Some A))\n\
     (def value (Maybe Nat) (variant Some 1))\n\
     (def bad Nat (case value (Some n n) (_ _)))"
    "unknown name: _";
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
  let fold_variant_wildcard =
    check
      "(variant Maybe (params A) (None Unit) (Some A))\n\
       (def value (Maybe Nat) (variant None unit))\n\
       (def out Nat (foldVariant (Maybe Nat) Nat value (Some n n) (_ 0)))"
  in
  assert_equal "foldVariant wildcard hashes as explicit branches"
    (Kernel.hash_program fold_variant_unit_explicit)
    (Kernel.hash_program fold_variant_wildcard);
  let fold_variant_wildcard_out, _ = Runtime.normalize_def fold_variant_wildcard "out" in
  assert_equal "foldVariant wildcard normalizes" "0"
    (Runtime.value_to_string fold_variant_wildcard_out);
  expect_check_error_contains
    "(variant Maybe (params A) (None Unit) (Some A))\n\
     (def value (Maybe Nat) (variant Some 1))\n\
     (def bad Nat (foldVariant (Maybe Nat) Nat value (None 0) (Some n n) (Some m m)))"
    "foldVariant duplicate branch: Some";
  expect_check_error_contains
    "(variant Maybe (params A) (None Unit) (Some A))\n\
     (def value (Maybe Nat) (variant Some 1))\n\
     (def bad Nat (foldVariant (Maybe Nat) Nat value (None 0) (Some n n) (_ 2)))"
    "foldVariant wildcard branch is unreachable";
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
  let poly_spine_contextual =
    check
      "(defpoly countPair (params A) \
       (-> (List A) (-> (List A) Nat)) \
       (lambda (xs (List A)) (lambda (ys (List A)) 0)))\n\
       (def countedSpine Nat ((countPair (Cons 1 Nil)) Nil))"
  in
  let poly_spine_contextual_explicit =
    check
      "(defpoly countPair (params A) \
       (-> (List A) (-> (List A) Nat)) \
       (lambda (xs (List A)) (lambda (ys (List A)) 0)))\n\
       (def countedSpine Nat \
       (((inst countPair Nat) (Cons Nat 1 (Nil Nat))) (Nil Nat)))"
  in
  assert_equal "defpoly partial spine infers Cons head list item"
    (Kernel.hash_program poly_spine_contextual_explicit)
    (Kernel.hash_program poly_spine_contextual);
  let counted_spine, _ = Runtime.normalize_def poly_spine_contextual "countedSpine" in
  assert_equal "defpoly partial spine normalizes" "0"
    (Runtime.value_to_string counted_spine);
  expect_check_error
    "(defpoly countPair (params A) \
       (-> (List A) (-> (List A) Nat)) \
       (lambda (xs (List A)) (lambda (ys (List A)) 0)))\n\
     (def bad Nat ((countPair (Cons 1 Nil)) (Cons true Nil)))";
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
  let primitive_nf =
    check
      "(def natText String (prim.Nat.toString 42))\n\
       (def cat String ((prim.String.concat \"a\") \"b\"))\n\
       (def eq Bool ((prim.Nat.eq 2) 2))\n\
       (def textLen Nat (prim.String.length \"abcd\"))\n\
       (def textSlice String (((prim.String.slice \"abcd\") 1) 2))\n\
       (def textSliceEmpty String (((prim.String.slice \"abcd\") 9) 2))\n"
  in
  assert_equal "kernel Nat.toString normalizer" "\"42\""
    (Kernel.cterm_to_string (Kernel.normalize_checked_def primitive_nf "natText"));
  assert_equal "kernel String.concat normalizer" "\"ab\""
    (Kernel.cterm_to_string (Kernel.normalize_checked_def primitive_nf "cat"));
  assert_equal "kernel Nat.eq normalizer" "true"
    (Kernel.cterm_to_string (Kernel.normalize_checked_def primitive_nf "eq"));
  assert_equal "kernel String.length normalizer" "4"
    (Kernel.cterm_to_string (Kernel.normalize_checked_def primitive_nf "textLen"));
  assert_equal "kernel String.slice normalizer" "\"bc\""
    (Kernel.cterm_to_string (Kernel.normalize_checked_def primitive_nf "textSlice"));
  assert_equal "kernel String.slice out of range normalizer" "\"\""
    (Kernel.cterm_to_string (Kernel.normalize_checked_def primitive_nf "textSliceEmpty"));

  assert_equal "deterministic hash" (Kernel.hash_program norm) (Kernel.hash_program norm);
  let diff = check "(def two Nat (succ 2))" in
  assert_true "different terms must hash differently" (Kernel.hash_program norm <> Kernel.hash_program diff);

  let lazy_unused =
    check
      "(def main Nat \
       (let (unused Nat (foldNat 1000 0 (lambda (acc Nat) (succ acc)))) 0))"
  in
  let lazy_unused_value, lazy_unused_trace =
    Runtime.normalize_def ~trace_cache:true lazy_unused "main"
  in
  assert_equal "lazy let unused result" "0" (Runtime.value_to_string lazy_unused_value);
  assert_true "lazy let creates thunk"
    (List.exists (String.equal "thunk let") lazy_unused_trace);
  assert_true "lazy let does not force unused RHS"
    (not (List.exists (String.equal "force let") lazy_unused_trace));
  let strict_unused =
    check
      "(def inc (-> Nat Nat) (lambda (x Nat) (succ x)))\n\
       (def main Nat (let (unused Nat (strict (inc 41))) 0))"
  in
  let strict_unused_value, strict_unused_trace =
    Runtime.normalize_def ~trace_cache:true strict_unused "main"
  in
  assert_equal "strict let unused result" "0" (Runtime.value_to_string strict_unused_value);
  assert_true "strict let forces unused RHS"
    (List.exists (String.equal "strict let") strict_unused_trace);
  assert_true "strict let bypasses lazy thunk"
    (not (List.exists (String.equal "thunk let") strict_unused_trace));
  let lazy_shared =
    check "(def main Nat (let (x Nat (succ 41)) ((prim.Nat.add x) x)))"
  in
  let lazy_shared_value, lazy_shared_trace =
    Runtime.normalize_def ~trace_cache:true lazy_shared "main"
  in
  let force_count =
    List.fold_left
      (fun count line -> if String.equal line "force let" then count + 1 else count)
      0 lazy_shared_trace
  in
  assert_equal "lazy let shared value" "84" (Runtime.value_to_string lazy_shared_value);
  assert_equal "lazy let shared thunk forces once" "1" (string_of_int force_count);

  let memo =
    check "(def inc (-> Nat Nat) (lambda (x Nat) (succ x)))\n\
           (def b Nat (let (x (inc 41)) (let (y (inc 41)) ((prim.Nat.add x) y))))"
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
  let eval_key_b = Runtime.eval_key_for_def memo "b" in
  let eval_key_b_alt_policy =
    Runtime.eval_key_for_def ~cache_scope:"alternate-runtime-policy" memo "b"
  in
  let eval_key_b_cap_scope = Runtime.eval_key_for_def ~cap_scope:[ "Human.ask" ] memo "b" in
  assert_equal "eval key explicit shape"
    (Kernel.hash_string
       ("protoss.eval.v1\ndef-id=p2:def\nargs-hash=p2:args\nruntime-policy=policy"))
    (Runtime.eval_key ~def_id:"p2:def" ~args_hash:"p2:args" ~runtime_policy:"policy");
  assert_equal "process eval key explicit shape"
    (Kernel.hash_string
       ("protoss.process.eval.v1\ndef-id=p2:def\nworld-ref=p2:world\ncap-scope=Human.ask\nruntime-policy=policy"))
    (Runtime.process_eval_key ~def_id:"p2:def" ~world_ref:"p2:world"
       ~cap_scope:[ "Human.ask" ] ~runtime_policy:"policy");
  assert_true "eval key uses content hash prefix"
    (contains_substring eval_key_b "p2:");
  assert_true "eval key partitions by runtime policy"
    (not (String.equal eval_key_b eval_key_b_alt_policy));
  assert_true "eval key partitions by capability scope"
    (not (String.equal eval_key_b eval_key_b_cap_scope));
  assert_true "runtime policy records stdlib fast paths"
    (contains_substring
       (Runtime.eval_runtime_policy ~stdlib_fast_paths:true
          ~cache_scope:"alternate-runtime-policy" memo)
       "stdlib-fast-paths=true");
  assert_true "runtime policy records capability scope"
    (contains_substring (Runtime.eval_runtime_policy ~cap_scope:[ "Human.ask" ] memo)
       "cap-scope=Human.ask");
  let process_eval_key_world_a =
    Runtime.process_eval_key_for_def ~world_ref:"p2:world-a" ~cap_scope:[ "Human.ask" ] memo "b"
  in
  let process_eval_key_world_b =
    Runtime.process_eval_key_for_def ~world_ref:"p2:world-b" ~cap_scope:[ "Human.ask" ] memo "b"
  in
  let process_eval_key_cap_scope =
    Runtime.process_eval_key_for_def ~world_ref:"p2:world-a" ~cap_scope:[ "Clock.read" ] memo "b"
  in
  assert_true "process eval key partitions by WorldRef"
    (not (String.equal process_eval_key_world_a process_eval_key_world_b));
  assert_true "process eval key partitions by CapScope"
    (not (String.equal process_eval_key_world_a process_eval_key_cap_scope));
  let _, _ = Runtime.eval_entry ~trace_cache:true ~cache_dir memo "b" in
  let _, persistent_trace = Runtime.eval_entry ~trace_cache:true ~cache_dir memo "b" in
  let _, _ =
    Runtime.eval_entry ~trace_cache:true ~cache_dir
      ~cache_scope:"alternate-runtime-policy" memo "b"
  in
  let hits, misses, entries = Runtime.persistent_cache_stats cache_dir in
  assert_true "persistent cache should have entries" (entries > 0);
  assert_true "persistent cache should record misses" (misses > 0);
  assert_true "persistent cache should record hits" (hits > 0);
  assert_true "persistent cache should contain eval key file"
    (Sys.file_exists (Filename.concat cache_dir (eval_key_b ^ ".cache")));
  assert_true "persistent cache should contain alternate policy eval key file"
    (Sys.file_exists (Filename.concat cache_dir (eval_key_b_alt_policy ^ ".cache")));
  assert_true "persistent cache trace should contain eval-key disk hit"
    (List.exists
       (fun line -> String.equal line ("cache hit eval " ^ eval_key_b))
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
  if Sys.getenv_opt "PROTOSS_RUN_STDLIB_TESTS" = Some "1" then (
  let stdlib_generics = Loader.check_file stdlib_generics_path in
  let result_errors_path = find_up (Sys.getcwd ()) "examples/result_errors.protoss" in
  let result_errors = Loader.check_file result_errors_path in
  let ada_age, _ = Runtime.normalize_def result_errors "adaAge" in
  assert_equal "Result business error example keeps Ok value" "18"
    (Runtime.value_to_string ada_age);
  let teen_default_age, _ = Runtime.normalize_def result_errors "teenDefaultAge" in
  assert_equal "Result business error example defaults Err value" "0"
    (Runtime.value_to_string teen_default_age);
  let teen_rejected, _ = Runtime.normalize_def result_errors "teenRejected" in
  assert_equal "Result business error example exposes Err branch" "true"
    (Runtime.value_to_string teen_rejected);
  let bumped, _ = Runtime.normalize_def stdlib_generics "bumped" in
  assert_equal "stdlib generic List.map" "[2, 3]" (Runtime.value_to_string bumped);
  let greeting, _ = Runtime.normalize_def stdlib_generics "greeting" in
  assert_equal "stdlib String.append" "\"Ada Lovelace\"" (Runtime.value_to_string greeting);
  let empty_label, _ = Runtime.normalize_def stdlib_generics "emptyLabel" in
  assert_equal "stdlib String.isEmpty" "true" (Runtime.value_to_string empty_label);
  let non_empty_label, _ = Runtime.normalize_def stdlib_generics "nonEmptyLabel" in
  assert_equal "stdlib String.nonEmpty" "true" (Runtime.value_to_string non_empty_label);
  let joined_labels, _ = Runtime.normalize_def stdlib_generics "joinedLabels" in
  assert_equal "stdlib String.join" "\"item,item\"" (Runtime.value_to_string joined_labels);
  let greeting_length, _ = Runtime.normalize_def stdlib_generics "greetingLength" in
  assert_equal "stdlib String.length" "12" (Runtime.value_to_string greeting_length);
  let greeting_prefix, _ = Runtime.normalize_def stdlib_generics "greetingPrefix" in
  assert_equal "stdlib String.slice" "\"Ada\"" (Runtime.value_to_string greeting_prefix);
  let greeting_starts_with, _ = Runtime.normalize_def stdlib_generics "greetingStartsWith" in
  assert_equal "stdlib String.startsWith" "true"
    (Runtime.value_to_string greeting_starts_with);
  let greeting_take, _ = Runtime.normalize_def stdlib_generics "greetingTake" in
  assert_equal "stdlib String.take" "\"Ada \"" (Runtime.value_to_string greeting_take);
  let greeting_drop, _ = Runtime.normalize_def stdlib_generics "greetingDrop" in
  assert_equal "stdlib String.drop" "\"Lovelace\"" (Runtime.value_to_string greeting_drop);
  let greeting_char, _ = Runtime.normalize_def stdlib_generics "greetingChar" in
  assert_equal "stdlib String.charAt hit" "Some \"d\"" (Runtime.value_to_string greeting_char);
  let greeting_char_missing, _ =
    Runtime.normalize_def stdlib_generics "greetingCharMissing"
  in
  assert_equal "stdlib String.charAt miss" "None unit"
    (Runtime.value_to_string greeting_char_missing);
  let digit_char, _ = Runtime.normalize_def stdlib_generics "digitChar" in
  assert_equal "stdlib String.isDigit hit" "true" (Runtime.value_to_string digit_char);
  let non_digit_char, _ = Runtime.normalize_def stdlib_generics "nonDigitChar" in
  assert_equal "stdlib String.isDigit miss" "false" (Runtime.value_to_string non_digit_char);
  let whitespace_char, _ = Runtime.normalize_def stdlib_generics "whitespaceChar" in
  assert_equal "stdlib String.isWhitespace" "true"
    (Runtime.value_to_string whitespace_char);
  let delimiter_char, _ = Runtime.normalize_def stdlib_generics "delimiterChar" in
  assert_equal "stdlib String.isDelimiter" "true" (Runtime.value_to_string delimiter_char);
  let atom_char, _ = Runtime.normalize_def stdlib_generics "atomChar" in
  assert_equal "stdlib String.isAtomChar hit" "true" (Runtime.value_to_string atom_char);
  let atom_char_rejected, _ = Runtime.normalize_def stdlib_generics "atomCharRejected" in
  assert_equal "stdlib String.isAtomChar reject" "false"
    (Runtime.value_to_string atom_char_rejected);
  let cursor_start_char, _ = Runtime.normalize_def stdlib_generics "cursorStartChar" in
  assert_equal "stdlib TextCursor.current start" "Some \"A\""
    (Runtime.value_to_string cursor_start_char);
  let cursor_next_char, _ = Runtime.normalize_def stdlib_generics "cursorNextChar" in
  assert_equal "stdlib TextCursor.current next" "Some \"d\""
    (Runtime.value_to_string cursor_next_char);
  let cursor_remaining, _ = Runtime.normalize_def stdlib_generics "cursorRemaining" in
  assert_equal "stdlib TextCursor.remaining" "\"da Lovelace\""
    (Runtime.value_to_string cursor_remaining);
  let cursor_peek, _ = Runtime.normalize_def stdlib_generics "cursorPeek" in
  assert_equal "stdlib TextCursor.peekIs" "true" (Runtime.value_to_string cursor_peek);
  let cursor_done, _ = Runtime.normalize_def stdlib_generics "cursorDone" in
  assert_equal "stdlib TextCursor.isDone" "true" (Runtime.value_to_string cursor_done);
  let pred_zero, _ = Runtime.normalize_def stdlib_generics "predZero" in
  assert_equal "stdlib Nat.pred zero" "0" (Runtime.value_to_string pred_zero);
  let pred_three, _ = Runtime.normalize_def stdlib_generics "predThree" in
  assert_equal "stdlib Nat.pred" "2" (Runtime.value_to_string pred_three);
  let subtract_floor, _ = Runtime.normalize_def stdlib_generics "subtractFloor" in
  assert_equal "stdlib Nat.sub floor" "0" (Runtime.value_to_string subtract_floor);
  let subtract_value, _ = Runtime.normalize_def stdlib_generics "subtractValue" in
  assert_equal "stdlib Nat.sub" "3" (Runtime.value_to_string subtract_value);
  let nat_less, _ = Runtime.normalize_def stdlib_generics "natLess" in
  assert_equal "stdlib Nat.lt" "true" (Runtime.value_to_string nat_less);
  let nat_not_less, _ = Runtime.normalize_def stdlib_generics "natNotLess" in
  assert_equal "stdlib Nat.lt equal" "false" (Runtime.value_to_string nat_not_less);
  let nat_gte, _ = Runtime.normalize_def stdlib_generics "natGte" in
  assert_equal "stdlib Nat.gte" "true" (Runtime.value_to_string nat_gte);
  let nat_text, _ = Runtime.normalize_def stdlib_generics "natText" in
  assert_equal "stdlib Nat.toString" "\"42\"" (Runtime.value_to_string nat_text);
  let source_span_text, _ = Runtime.normalize_def stdlib_generics "sourceSpanText" in
  assert_equal "stdlib SourceSpan.render" "\"2:7\""
    (Runtime.value_to_string source_span_text);
  let diagnostic_text, _ = Runtime.normalize_def stdlib_generics "diagnosticText" in
  assert_equal "stdlib Diagnostic.render" "\"2:7: unexpected token\""
    (Runtime.value_to_string diagnostic_text);
  let len, _ = Runtime.normalize_def stdlib_generics "len" in
  assert_equal "stdlib generic List.length" "2" (Runtime.value_to_string len);
  let appended, _ = Runtime.normalize_def stdlib_generics "appended" in
  assert_equal "stdlib generic List.append" "[1, 2, 3]" (Runtime.value_to_string appended);
  let concatenated, _ = Runtime.normalize_def stdlib_generics "concatenated" in
  assert_equal "stdlib generic List.concat" "[1, 2, 3]"
    (Runtime.value_to_string concatenated);
  let folded, _ = Runtime.normalize_def stdlib_generics "folded" in
  assert_equal "stdlib generic List.fold" "6" (Runtime.value_to_string folded);
  let flat_mapped, _ = Runtime.normalize_def stdlib_generics "flatMapped" in
  assert_equal "stdlib generic List.flatMap" "[1, 2, 2, 3]"
    (Runtime.value_to_string flat_mapped);
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
  let option_default, _ = Runtime.normalize_def stdlib_generics "optionDefault" in
  assert_equal "stdlib generic Option.map/default" "\"known\""
    (Runtime.value_to_string option_default);
  let option_has_age, _ = Runtime.normalize_def stdlib_generics "optionHasAge" in
  assert_equal "stdlib generic Option.isSome" "true" (Runtime.value_to_string option_has_age);
  let option_missing_age, _ = Runtime.normalize_def stdlib_generics "optionMissingAge" in
  assert_equal "stdlib generic Option.isNone" "true"
    (Runtime.value_to_string option_missing_age);
  let option_next, _ = Runtime.normalize_def stdlib_generics "optionNext" in
  assert_equal "stdlib generic Option.andThen" "Some 42"
    (Runtime.value_to_string option_next);
  let option_pair, _ = Runtime.normalize_def stdlib_generics "optionPair" in
  assert_equal "stdlib generic Option.map2" "Some 42"
    (Runtime.value_to_string option_pair);
  let option_result, _ = Runtime.normalize_def stdlib_generics "optionResult" in
  assert_equal "stdlib generic Option.toResult" "Ok 41"
    (Runtime.value_to_string option_result);
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
  let package_resolved_local, _ =
    Runtime.normalize_def stdlib_generics "packageResolvedLocal"
  in
  assert_equal "stdlib PackageRegistry local resolver"
    "Ok {path = \"../workspace\", selector = \"workspace-a@0.4.0\", source = \"local\"}"
    (Runtime.value_to_string package_resolved_local);
  let package_resolved_global, _ =
    Runtime.normalize_def stdlib_generics "packageResolvedGlobal"
  in
  assert_equal "stdlib PackageRegistry global resolver"
    "Ok {path = \"/registry/workspace-a\", selector = \"workspace-a@NoNetworkExceptDeclared\", source = \"global\"}"
    (Runtime.value_to_string package_resolved_global);
  let package_resolved_missing, _ =
    Runtime.normalize_def stdlib_generics "packageResolvedMissing"
  in
  assert_equal "stdlib PackageRegistry missing resolver"
    "Err \"missing package selector: missing@1.0.0\""
    (Runtime.value_to_string package_resolved_missing);
  let map_age, _ = Runtime.normalize_def stdlib_generics "mapAge" in
  assert_equal "stdlib generic Map.get" "Some 41" (Runtime.value_to_string map_age);
  let map_has_count, _ = Runtime.normalize_def stdlib_generics "mapHasCount" in
  assert_equal "stdlib generic Map.contains" "true"
    (Runtime.value_to_string map_has_count);
  let map_keys, _ = Runtime.normalize_def stdlib_generics "mapKeys" in
  assert_equal "stdlib generic Map.keys" "[\"age\", \"count\"]"
    (Runtime.value_to_string map_keys);
  let map_values, _ = Runtime.normalize_def stdlib_generics "mapValues" in
  assert_equal "stdlib generic Map.values" "[41, 2]"
    (Runtime.value_to_string map_values);
  let map_removed_keys, _ = Runtime.normalize_def stdlib_generics "mapRemovedKeys" in
  assert_equal "stdlib generic Map.remove" "[\"age\"]"
    (Runtime.value_to_string map_removed_keys);
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
  let sexp_is_atom, _ = Runtime.normalize_def stdlib_generics "sexpIsAtom" in
  assert_equal "stdlib Sexp.isAtom" "true" (Runtime.value_to_string sexp_is_atom);
  let sexp_is_list, _ = Runtime.normalize_def stdlib_generics "sexpIsList" in
  assert_equal "stdlib Sexp.isList" "true" (Runtime.value_to_string sexp_is_list);
  let sexp_atom_result, _ = Runtime.normalize_def stdlib_generics "sexpAtomResult" in
  assert_equal "stdlib Sexp.expectAtom" "Ok \"def\""
    (Runtime.value_to_string sexp_atom_result);
  let sexp_list_result, _ = Runtime.normalize_def stdlib_generics "sexpListResult" in
  assert_equal "stdlib Sexp.expectList" "Ok [SAtom \"def\", SString \"main\"]"
    (Runtime.value_to_string sexp_list_result);
  let sexp_string_miss, _ = Runtime.normalize_def stdlib_generics "sexpStringMiss" in
  assert_equal "stdlib Sexp.expectString miss" "Err \"expected Sexp string\""
    (Runtime.value_to_string sexp_string_miss);
  let sexp_flat_atom, _ = Runtime.normalize_def stdlib_generics "sexpFlatAtom" in
  assert_equal "stdlib Sexp.renderFlat atom" "\"def\""
    (Runtime.value_to_string sexp_flat_atom);
  let sexp_flat_string, _ = Runtime.normalize_def stdlib_generics "sexpFlatString" in
  assert_equal "stdlib Sexp.renderFlat string" "\"\\\"main\\\"\""
    (Runtime.value_to_string sexp_flat_string);
  let sexp_flat_form, _ = Runtime.normalize_def stdlib_generics "sexpFlatForm" in
  assert_equal "stdlib Sexp.renderFlat list" "\"(def \\\"main\\\")\""
    (Runtime.value_to_string sexp_flat_form);
  let sexp_flat_nested, _ = Runtime.normalize_def stdlib_generics "sexpFlatNested" in
  assert_equal "stdlib Sexp.renderFlat nested placeholder" "\"((...) def)\""
    (Runtime.value_to_string sexp_flat_nested);
  let sexp_render_form, _ = Runtime.normalize_def stdlib_generics "sexpRenderForm" in
  assert_equal "stdlib Sexp.render list" "\"(def \\\"main\\\")\""
    (Runtime.value_to_string sexp_render_form);
  let sexp_render_nested, _ = Runtime.normalize_def stdlib_generics "sexpRenderNested" in
  assert_equal "stdlib Sexp.render nested list" "\"((def \\\"main\\\") def)\""
    (Runtime.value_to_string sexp_render_nested);
  let sexp_tokens, _ = Runtime.normalize_def stdlib_generics "sexpTokens" in
  assert_equal "stdlib Sexp.lexTokens"
    "Ok [SLParen unit, SAtomToken \"def\", SAtomToken \"main\", SStringToken \"Ada\", SRParen unit]"
    (Runtime.value_to_string sexp_tokens);
  let sexp_tokens_with_comment, _ =
    Runtime.normalize_def stdlib_generics "sexpTokensWithComment"
  in
  assert_equal "stdlib Sexp.lexTokens comment"
    "Ok [SLParen unit, SAtomToken \"def\", SAtomToken \"main\", SAtomToken \"1\", SRParen unit, SLParen unit, SAtomToken \"next\", SRParen unit]"
    (Runtime.value_to_string sexp_tokens_with_comment);
  let sexp_tokens_unterminated, _ =
    Runtime.normalize_def stdlib_generics "sexpTokensUnterminated"
  in
  assert_equal "stdlib Sexp.lexTokens unterminated string" "Err \"unterminated string\""
    (Runtime.value_to_string sexp_tokens_unterminated);
  let sexp_parsed, _ = Runtime.normalize_def stdlib_generics "sexpParsed" in
  assert_equal "stdlib Sexp.parseText"
    "Ok [SList [SAtom \"def\", SAtom \"main\", SString \"Ada\"]]"
    (Runtime.value_to_string sexp_parsed);
  let sexp_parsed_nested, _ = Runtime.normalize_def stdlib_generics "sexpParsedNested" in
  assert_equal "stdlib Sexp.parseText nested"
    "Ok [SList [SAtom \"outer\", SList [SAtom \"inner\", SString \"Ada\"], SAtom \"tail\"]]"
    (Runtime.value_to_string sexp_parsed_nested);
  let sexp_parsed_unexpected_close, _ =
    Runtime.normalize_def stdlib_generics "sexpParsedUnexpectedClose"
  in
  assert_equal "stdlib Sexp.parseText unexpected close" "Err \"unexpected )\""
    (Runtime.value_to_string sexp_parsed_unexpected_close);
  let sexp_parsed_unterminated_list, _ =
    Runtime.normalize_def stdlib_generics "sexpParsedUnterminatedList"
  in
  assert_equal "stdlib Sexp.parseText unterminated list" "Err \"unterminated list\""
    (Runtime.value_to_string sexp_parsed_unterminated_list);
  let protoss_parsed_def, _ = Runtime.normalize_def stdlib_generics "protossParsedDef" in
  assert_equal "stdlib Protoss.parseText def"
    "Ok [PDDef {expr = PEString \"Ada\", name = \"greeting\", typ = PTName \"String\"}]"
    (Runtime.value_to_string protoss_parsed_def);
  let protoss_parsed_function_def, _ =
    Runtime.normalize_def stdlib_generics "protossParsedFunctionDef"
  in
  assert_equal "stdlib Protoss.parseText function def"
    "Ok [PDDef {expr = PELambda {body = PEApply {args = [PEVar \"x\"], fn = PEVar \"succ\"}, param = {name = \"x\", typ = PTName \"Nat\"}}, name = \"inc\", typ = PTFun {first = PTName \"Nat\", second = PTName \"Nat\"}}]"
    (Runtime.value_to_string protoss_parsed_function_def);
  let protoss_parsed_type_apply, _ =
    Runtime.normalize_def stdlib_generics "protossParsedTypeApply"
  in
  assert_equal "stdlib Protoss.parseText type application"
    "Ok [PDDef {expr = PEVar \"value\", name = \"xs\", typ = PTApply {args = [PTName \"Nat\"], name = \"List\"}}]"
    (Runtime.value_to_string protoss_parsed_type_apply);
  let protoss_parsed_unit_def, _ =
    Runtime.normalize_def stdlib_generics "protossParsedUnitDef"
  in
  assert_equal "stdlib Protoss.parseText unit def"
    "Ok [PDDef {expr = PEUnit unit, name = \"main\", typ = PTName \"Unit\"}]"
    (Runtime.value_to_string protoss_parsed_unit_def);
  let protoss_parsed_bool_def, _ =
    Runtime.normalize_def stdlib_generics "protossParsedBoolDef"
  in
  assert_equal "stdlib Protoss.parseText bool def"
    "Ok [PDDef {expr = PEBool true, name = \"ok\", typ = PTName \"Bool\"}]"
    (Runtime.value_to_string protoss_parsed_bool_def);
  let protoss_parsed_record_expr, _ =
    Runtime.normalize_def stdlib_generics "protossParsedRecordExpr"
  in
  assert_equal "stdlib Protoss.parseText record expr"
    "Ok [PDDef {expr = PERecord [{expr = PEString \"Ada\", name = \"name\"}, {expr = PEBool true, name = \"active\"}], name = \"person\", typ = PTName \"Person\"}]"
    (Runtime.value_to_string protoss_parsed_record_expr);
  let protoss_parsed_variant_expr, _ =
    Runtime.normalize_def stdlib_generics "protossParsedVariantExpr"
  in
  assert_equal "stdlib Protoss.parseText variant expr"
    "Ok [PDDef {expr = PEVariant {constructor = \"Some\", payload = PEVar \"4\", typeHint = Some PTApply {args = [PTName \"Nat\"], name = \"Maybe\"}}, name = \"value\", typ = PTApply {args = [PTName \"Nat\"], name = \"Maybe\"}}]"
    (Runtime.value_to_string protoss_parsed_variant_expr);
  let protoss_parsed_let_expr, _ =
    Runtime.normalize_def stdlib_generics "protossParsedLetExpr"
  in
  assert_equal "stdlib Protoss.parseText let expr"
    "Ok [PDDef {expr = PELet {body = PEApply {args = [PEVar \"x\"], fn = PEVar \"succ\"}, name = \"x\", typ = Some PTName \"Nat\", value = PEVar \"1\"}, name = \"local\", typ = PTName \"Nat\"}]"
    (Runtime.value_to_string protoss_parsed_let_expr);
  let protoss_parsed_let_inferred_expr, _ =
    Runtime.normalize_def stdlib_generics "protossParsedLetInferredExpr"
  in
  assert_equal "stdlib Protoss.parseText inferred let expr"
    "Ok [PDDef {expr = PELet {body = PEVar \"x\", name = \"x\", typ = None unit, value = PEVar \"1\"}, name = \"local\", typ = PTName \"Nat\"}]"
    (Runtime.value_to_string protoss_parsed_let_inferred_expr);
  let protoss_parsed_case_expr, _ =
    Runtime.normalize_def stdlib_generics "protossParsedCaseExpr"
  in
  assert_equal "stdlib Protoss.parseText case expr"
    "Ok [PDDef {expr = PECase {branches = [{binder = None unit, body = PEVar \"1\", constructor = \"true\"}, {binder = None unit, body = PEVar \"0\", constructor = \"false\"}], scrutinee = PEVar \"flag\"}, name = \"out\", typ = PTName \"Nat\"}]"
    (Runtime.value_to_string protoss_parsed_case_expr);
  let protoss_parsed_case_variant_expr, _ =
    Runtime.normalize_def stdlib_generics "protossParsedCaseVariantExpr"
  in
  assert_equal "stdlib Protoss.parseText variant case expr"
    "Ok [PDDef {expr = PECase {branches = [{binder = None unit, body = PEVar \"0\", constructor = \"None\"}, {binder = Some \"x\", body = PEVar \"x\", constructor = \"Some\"}], scrutinee = PEVar \"maybe\"}, name = \"out\", typ = PTName \"Nat\"}]"
    (Runtime.value_to_string protoss_parsed_case_variant_expr);
  let protoss_parsed_fold_nat_expr, _ =
    Runtime.normalize_def stdlib_generics "protossParsedFoldNatExpr"
  in
  assert_equal "stdlib Protoss.parseText foldNat expr"
    "Ok [PDDef {expr = PEFoldNat {step = PELambda {body = PEApply {args = [PEVar \"acc\"], fn = PEVar \"succ\"}, param = {name = \"acc\", typ = PTName \"Nat\"}}, target = PEVar \"n\", zero = PEVar \"0\"}, name = \"two\", typ = PTName \"Nat\"}]"
    (Runtime.value_to_string protoss_parsed_fold_nat_expr);
  let protoss_parsed_fold_variant_expr, _ =
    Runtime.normalize_def stdlib_generics "protossParsedFoldVariantExpr"
  in
  assert_equal "stdlib Protoss.parseText foldVariant expr"
    "Ok [PDDef {expr = PEFoldVariant {branches = [{binder = Some \"x\", body = PEVar \"1\", constructor = \"Leaf\"}, {binder = Some \"pair\", body = PEVar \"2\", constructor = \"Node\"}], result = PTName \"Nat\", scrutinee = PEVar \"tree\", target = PTName \"Tree\"}, name = \"size\", typ = PTName \"Nat\"}]"
    (Runtime.value_to_string protoss_parsed_fold_variant_expr);
  let protoss_parsed_recur_expr, _ =
    Runtime.normalize_def stdlib_generics "protossParsedRecurExpr"
  in
  assert_equal "stdlib Protoss.parseText recur expr"
    "Ok [PDDef {expr = PERecur PEVar \"child\", name = \"step\", typ = PTName \"Nat\"}]"
    (Runtime.value_to_string protoss_parsed_recur_expr);
  let protoss_parsed_get_expr, _ =
    Runtime.normalize_def stdlib_generics "protossParsedGetExpr"
  in
  assert_equal "stdlib Protoss.parseText get expr"
    "Ok [PDDef {expr = PEField {field = \"name\", target = PEVar \"user\"}, name = \"userName\", typ = PTName \"String\"}]"
    (Runtime.value_to_string protoss_parsed_get_expr);
  let protoss_parsed_inst_expr, _ =
    Runtime.normalize_def stdlib_generics "protossParsedInstExpr"
  in
  assert_equal "stdlib Protoss.parseText inst expr"
    "Ok [PDDef {expr = PEInst {name = \"id\", typeArgs = [PTName \"Nat\"]}, name = \"idNat\", typ = PTName \"Id\"}]"
    (Runtime.value_to_string protoss_parsed_inst_expr);
  let protoss_parsed_fold_list_expr, _ =
    Runtime.normalize_def stdlib_generics "protossParsedFoldListExpr"
  in
  assert_equal "stdlib Protoss.parseText foldList expr"
    "Ok [PDDef {expr = PEFoldList {step = PELambda {body = PELambda {body = PEApply {args = [PEVar \"acc\"], fn = PEVar \"succ\"}, param = {name = \"acc\", typ = PTName \"Nat\"}}, param = {name = \"x\", typ = PTName \"Nat\"}}, target = PEVar \"xs\", zero = PEVar \"0\"}, name = \"len\", typ = PTName \"Nat\"}]"
    (Runtime.value_to_string protoss_parsed_fold_list_expr);
  let protoss_parsed_case_list_expr, _ =
    Runtime.normalize_def stdlib_generics "protossParsedCaseListExpr"
  in
  assert_equal "stdlib Protoss.parseText caseList expr"
    "Ok [PDDef {expr = PECaseList {consBody = PEVar \"head\", head = \"head\", nilBody = PEVar \"0\", tail = \"tail\", target = PEVar \"xs\"}, name = \"headOrZero\", typ = PTName \"Nat\"}]"
    (Runtime.value_to_string protoss_parsed_case_list_expr);
  let protoss_parsed_nil_expr, _ =
    Runtime.normalize_def stdlib_generics "protossParsedNilExpr"
  in
  assert_equal "stdlib Protoss.parseText Nil expr"
    "Ok [PDDef {expr = PENil {arg = None unit, typ = None unit}, name = \"xs\", typ = PTApply {args = [PTName \"Nat\"], name = \"List\"}}]"
    (Runtime.value_to_string protoss_parsed_nil_expr);
  let protoss_parsed_typed_nil_expr, _ =
    Runtime.normalize_def stdlib_generics "protossParsedTypedNilExpr"
  in
  assert_equal "stdlib Protoss.parseText typed Nil expr"
    "Ok [PDDef {expr = PENil {arg = Some PEVar \"Nat\", typ = Some PTName \"Nat\"}, name = \"xs\", typ = PTApply {args = [PTName \"Nat\"], name = \"List\"}}]"
    (Runtime.value_to_string protoss_parsed_typed_nil_expr);
  let protoss_parsed_cons_expr, _ =
    Runtime.normalize_def stdlib_generics "protossParsedConsExpr"
  in
  assert_equal "stdlib Protoss.parseText Cons expr"
    "Ok [PDDef {expr = PECons {head = PEVar \"1\", tail = PENil {arg = None unit, typ = None unit}, typ = Some PTName \"Nat\"}, name = \"xs\", typ = PTApply {args = [PTName \"Nat\"], name = \"List\"}}]"
    (Runtime.value_to_string protoss_parsed_cons_expr);
  let protoss_parsed_cons_inferred_expr, _ =
    Runtime.normalize_def stdlib_generics "protossParsedConsInferredExpr"
  in
  assert_equal "stdlib Protoss.parseText inferred Cons expr"
    "Ok [PDDef {expr = PECons {head = PEVar \"1\", tail = PENil {arg = None unit, typ = None unit}, typ = None unit}, name = \"xs\", typ = PTApply {args = [PTName \"Nat\"], name = \"List\"}}]"
    (Runtime.value_to_string protoss_parsed_cons_inferred_expr);
  let protoss_parsed_done_expr, _ =
    Runtime.normalize_def stdlib_generics "protossParsedDoneExpr"
  in
  assert_equal "stdlib Protoss.parseText done expr"
    "Ok [PDDef {expr = PEDone PEString \"ok\", name = \"p\", typ = PTApply {args = [PTName \"String\"], name = \"Process\"}}]"
    (Runtime.value_to_string protoss_parsed_done_expr);
  let protoss_parsed_bind_expr, _ =
    Runtime.normalize_def stdlib_generics "protossParsedBindExpr"
  in
  assert_equal "stdlib Protoss.parseText bind expr"
    "Ok [PDDef {expr = PEBind {body = PEDone PEVar \"answer\", param = \"answer\", process = PERequest PRAskHuman \"Name?\", typ = PTName \"String\"}, name = \"p\", typ = PTApply {args = [PTName \"String\"], name = \"Process\"}}]"
    (Runtime.value_to_string protoss_parsed_bind_expr);
  let protoss_parsed_request_expr, _ =
    Runtime.normalize_def stdlib_generics "protossParsedRequestExpr"
  in
  assert_equal "stdlib Protoss.parseText request expr"
    "Ok [PDDef {expr = PERequest PRAskHuman \"Name?\", name = \"p\", typ = PTApply {args = [PTName \"String\"], name = \"Process\"}}]"
    (Runtime.value_to_string protoss_parsed_request_expr);
  let protoss_parsed_clock_expr, _ =
    Runtime.normalize_def stdlib_generics "protossParsedClockExpr"
  in
  assert_equal "stdlib Protoss.parseText clock expr"
    "Ok [PDDef {expr = PERequest PRClockRead unit, name = \"now\", typ = PTApply {args = [PTName \"String\"], name = \"Process\"}}]"
    (Runtime.value_to_string protoss_parsed_clock_expr);
  let protoss_parsed_bad_expression, _ =
    Runtime.normalize_def stdlib_generics "protossParsedBadExpression"
  in
  assert_equal "stdlib Protoss.parseText bad expression" "Err \"expected expression\""
    (Runtime.value_to_string protoss_parsed_bad_expression);
  let protoss_parsed_bad_record_expr, _ =
    Runtime.normalize_def stdlib_generics "protossParsedBadRecordExpr"
  in
  assert_equal "stdlib Protoss.parseText bad record expr"
    "Err \"expected record field form\""
    (Runtime.value_to_string protoss_parsed_bad_record_expr);
  let protoss_parsed_bad_let_expr, _ =
    Runtime.normalize_def stdlib_generics "protossParsedBadLetExpr"
  in
  assert_equal "stdlib Protoss.parseText bad let expr" "Err \"expected let binding\""
    (Runtime.value_to_string protoss_parsed_bad_let_expr);
  let protoss_parsed_bad_case_expr, _ =
    Runtime.normalize_def stdlib_generics "protossParsedBadCaseExpr"
  in
  assert_equal "stdlib Protoss.parseText bad case expr"
    "Err \"expected case branch form\""
    (Runtime.value_to_string protoss_parsed_bad_case_expr);
  let protoss_parsed_bad_fold_nat_expr, _ =
    Runtime.normalize_def stdlib_generics "protossParsedBadFoldNatExpr"
  in
  assert_equal "stdlib Protoss.parseText bad foldNat expr"
    "Err \"expected foldNat step\""
    (Runtime.value_to_string protoss_parsed_bad_fold_nat_expr);
  let protoss_parsed_bad_fold_variant_expr, _ =
    Runtime.normalize_def stdlib_generics "protossParsedBadFoldVariantExpr"
  in
  assert_equal "stdlib Protoss.parseText bad foldVariant expr"
    "Err \"expected foldVariant branch\""
    (Runtime.value_to_string protoss_parsed_bad_fold_variant_expr);
  let protoss_parsed_bad_recur_expr, _ =
    Runtime.normalize_def stdlib_generics "protossParsedBadRecurExpr"
  in
  assert_equal "stdlib Protoss.parseText bad recur expr" "Err \"expected recur value\""
    (Runtime.value_to_string protoss_parsed_bad_recur_expr);
  let protoss_parsed_bad_get_expr, _ =
    Runtime.normalize_def stdlib_generics "protossParsedBadGetExpr"
  in
  assert_equal "stdlib Protoss.parseText bad get expr" "Err \"expected get field\""
    (Runtime.value_to_string protoss_parsed_bad_get_expr);
  let protoss_parsed_bad_case_list_expr, _ =
    Runtime.normalize_def stdlib_generics "protossParsedBadCaseListExpr"
  in
  assert_equal "stdlib Protoss.parseText bad caseList expr"
    "Err \"expected caseList Cons branch\""
    (Runtime.value_to_string protoss_parsed_bad_case_list_expr);
  let protoss_parsed_bad_nil_expr, _ =
    Runtime.normalize_def stdlib_generics "protossParsedBadNilExpr"
  in
  assert_equal "stdlib Protoss.parseText bad Nil expr" "Err \"too many Nil arguments\""
    (Runtime.value_to_string protoss_parsed_bad_nil_expr);
  let protoss_parsed_bad_cons_expr, _ =
    Runtime.normalize_def stdlib_generics "protossParsedBadConsExpr"
  in
  assert_equal "stdlib Protoss.parseText bad Cons expr" "Err \"too many Cons arguments\""
    (Runtime.value_to_string protoss_parsed_bad_cons_expr);
  let protoss_parsed_bad_bind_expr, _ =
    Runtime.normalize_def stdlib_generics "protossParsedBadBindExpr"
  in
  assert_equal "stdlib Protoss.parseText bad bind expr" "Err \"expected bind lambda\""
    (Runtime.value_to_string protoss_parsed_bad_bind_expr);
  let protoss_parsed_bad_request_expr, _ =
    Runtime.normalize_def stdlib_generics "protossParsedBadRequestExpr"
  in
  assert_equal "stdlib Protoss.parseText bad request expr"
    "Err \"expected string literal\""
    (Runtime.value_to_string protoss_parsed_bad_request_expr);
  let protoss_parsed_bad_tag, _ =
    Runtime.normalize_def stdlib_generics "protossParsedBadTag"
  in
  assert_equal "stdlib Protoss.parseText bad tag" "Err \"expected declaration tag\""
    (Runtime.value_to_string protoss_parsed_bad_tag);
  let protoss_parsed_bad_function_type, _ =
    Runtime.normalize_def stdlib_generics "protossParsedBadFunctionType"
  in
  assert_equal "stdlib Protoss.parseText bad function type"
    "Err \"expected function result type\""
    (Runtime.value_to_string protoss_parsed_bad_function_type);
  let protoss_parsed_type_alias, _ =
    Runtime.normalize_def stdlib_generics "protossParsedTypeAlias"
  in
  assert_equal "stdlib Protoss.parseText type alias"
    "Ok [PDType {body = PTApply {args = [PTApply {args = [PTName \"Unit\"], name = \"None\"}, PTApply {args = [PTName \"A\"], name = \"Some\"}], name = \"Variant\"}, name = \"Maybe\", typeParams = [\"A\"]}]"
    (Runtime.value_to_string protoss_parsed_type_alias);
  let protoss_parsed_record, _ = Runtime.normalize_def stdlib_generics "protossParsedRecord" in
  assert_equal "stdlib Protoss.parseText record"
    "Ok [PDRecord {fields = [{name = \"first\", typ = PTName \"A\"}, {name = \"second\", typ = PTName \"B\"}], name = \"Pair\", typeParams = [\"A\", \"B\"]}]"
    (Runtime.value_to_string protoss_parsed_record);
  let protoss_parsed_variant, _ =
    Runtime.normalize_def stdlib_generics "protossParsedVariant"
  in
  assert_equal "stdlib Protoss.parseText variant"
    "Ok [PDVariant {cases = [{name = \"Leaf\", payload = PTName \"A\"}, {name = \"Node\", payload = PTApply {args = [PTName \"A\"], name = \"Tree\"}}], name = \"Tree\", typeParams = [\"A\"]}]"
    (Runtime.value_to_string protoss_parsed_variant);
  let protoss_parsed_module_file, _ =
    Runtime.normalize_def stdlib_generics "protossParsedModuleFile"
  in
  assert_equal "stdlib Protoss.parseText module file"
    "Ok [PDModule \"Demo.Math\", PDImport \"prelude.protoss\", PDExport [\"Number\", \"double\"], PDCapabilities [\"Human.ask\", \"Clock.read\"]]"
    (Runtime.value_to_string protoss_parsed_module_file);
  let protoss_parsed_bad_import, _ =
    Runtime.normalize_def stdlib_generics "protossParsedBadImport"
  in
  assert_equal "stdlib Protoss.parseText bad import" "Err \"expected import path\""
    (Runtime.value_to_string protoss_parsed_bad_import);
  let protoss_parsed_alias, _ = Runtime.normalize_def stdlib_generics "protossParsedAlias" in
  assert_equal "stdlib Protoss.parseText alias"
    "Ok [PDType {body = PTName \"Nat\", name = \"Count\", typeParams = []}]"
    (Runtime.value_to_string protoss_parsed_alias);
  let protoss_parsed_defpoly, _ =
    Runtime.normalize_def stdlib_generics "protossParsedDefPoly"
  in
  assert_equal "stdlib Protoss.parseText defpoly"
    "Ok [PDDefPoly {expr = PELambda {body = PEVar \"x\", param = {name = \"x\", typ = PTName \"A\"}}, name = \"id\", typ = PTFun {first = PTName \"A\", second = PTName \"A\"}, typeParams = [\"A\"]}]"
    (Runtime.value_to_string protoss_parsed_defpoly);
  let protoss_parsed_defcap, _ =
    Runtime.normalize_def stdlib_generics "protossParsedDefCap"
  in
  assert_equal "stdlib Protoss.parseText defcap"
    "Ok [PDDefCap {capabilities = [\"Human.ask\"], expr = PERequest PRAskHuman \"Name?\", name = \"askName\", typ = PTApply {args = [PTName \"String\"], name = \"Process\"}}]"
    (Runtime.value_to_string protoss_parsed_defcap);
  let protoss_parsed_defpolycap, _ =
    Runtime.normalize_def stdlib_generics "protossParsedDefPolyCap"
  in
  assert_equal "stdlib Protoss.parseText defpolycap"
    "Ok [PDDefPolyCap {capabilities = [], expr = PELambda {body = PEVar \"x\", param = {name = \"x\", typ = PTName \"A\"}}, name = \"id\", typ = PTFun {first = PTName \"A\", second = PTName \"A\"}, typeParams = [\"A\"]}]"
    (Runtime.value_to_string protoss_parsed_defpolycap);
  let protoss_parsed_bad_defcap, _ =
    Runtime.normalize_def stdlib_generics "protossParsedBadDefCap"
  in
  assert_equal "stdlib Protoss.parseText bad defcap"
    "Err \"invalid definition capabilities\""
    (Runtime.value_to_string protoss_parsed_bad_defcap);
  let protoss_parsed_defrec_nat, _ =
    Runtime.normalize_def stdlib_generics "protossParsedDefRecNat"
  in
  assert_equal "stdlib Protoss.parseText defrec Nat"
    "Ok [PDDefRec {body = PDRNat {acc = \"acc\", param = \"n\", step = PEApply {args = [PEVar \"acc\"], fn = PEVar \"succ\"}, zero = PEVar \"0\"}, name = \"count\", typ = PTFun {first = PTName \"Nat\", second = PTName \"Nat\"}, typeParams = []}]"
    (Runtime.value_to_string protoss_parsed_defrec_nat);
  let protoss_parsed_defrec_variant, _ =
    Runtime.normalize_def stdlib_generics "protossParsedDefRecVariant"
  in
  assert_equal "stdlib Protoss.parseText defrec Variant"
    "Ok [PDDefRec {body = PDRVariant {branches = [{binder = Some \"n\", body = PEVar \"1\", constructor = \"Leaf\"}, {binder = Some \"pair\", body = PERecur PEField {field = \"left\", target = PEVar \"pair\"}, constructor = \"Node\"}], param = \"tree\"}, name = \"size\", typ = PTFun {first = PTName \"Tree\", second = PTName \"Nat\"}, typeParams = []}]"
    (Runtime.value_to_string protoss_parsed_defrec_variant);
  let protoss_parsed_defrecpoly_list, _ =
    Runtime.normalize_def stdlib_generics "protossParsedDefRecPolyList"
  in
  assert_equal "stdlib Protoss.parseText defrecpoly List"
    "Ok [PDDefRec {body = PDRList {acc = \"acc\", item = \"item\", nil = PENil {arg = None unit, typ = None unit}, param = \"xs\", step = PECons {head = PEVar \"item\", tail = PEVar \"acc\", typ = None unit}}, name = \"copy\", typ = PTFun {first = PTApply {args = [PTName \"A\"], name = \"List\"}, second = PTApply {args = [PTName \"A\"], name = \"List\"}}, typeParams = [\"A\"]}]"
    (Runtime.value_to_string protoss_parsed_defrecpoly_list);
  let protoss_parsed_bad_defrec, _ =
    Runtime.normalize_def stdlib_generics "protossParsedBadDefRec"
  in
  assert_equal "stdlib Protoss.parseText bad defrec" "Err \"expected step clause\""
    (Runtime.value_to_string protoss_parsed_bad_defrec);
  let protoss_parsed_bad_field, _ =
    Runtime.normalize_def stdlib_generics "protossParsedBadField"
  in
  assert_equal "stdlib Protoss.parseText bad field" "Err \"expected field type\""
    (Runtime.value_to_string protoss_parsed_bad_field);
  let protoss_parsed_bad_params, _ =
    Runtime.normalize_def stdlib_generics "protossParsedBadParams"
  in
  assert_equal "stdlib Protoss.parseText bad params" "Err \"expected parameter name\""
    (Runtime.value_to_string protoss_parsed_bad_params);
  let protoss_formatted_def, _ =
    Runtime.normalize_def stdlib_generics "protossFormattedDef"
  in
  assert_equal "stdlib Protoss.formatText def" "Ok \"(def greeting String \\\"Ada\\\")\""
    (Runtime.value_to_string protoss_formatted_def);
  let protoss_formatted_multiple, _ =
    Runtime.normalize_def stdlib_generics "protossFormattedMultiple"
  in
  assert_equal "stdlib Protoss.formatText multiple"
    "Ok \"(def one Nat 1)\\n(def two Nat (succ one))\""
    (Runtime.value_to_string protoss_formatted_multiple);
  let protoss_formatted_invalid, _ =
    Runtime.normalize_def stdlib_generics "protossFormattedInvalid"
  in
  assert_equal "stdlib Protoss.formatText invalid" "Err \"expected declaration tag\""
    (Runtime.value_to_string protoss_formatted_invalid);
  let protoss_formatted_case_expr, _ =
    Runtime.normalize_def stdlib_generics "protossFormattedCaseExpr"
  in
  assert_equal "stdlib Protoss.formatText case expr"
    "Ok \"(def local Nat (let (x Nat 1) (case flag (true (succ x)) (false 0))))\""
    (Runtime.value_to_string protoss_formatted_case_expr);
  let protoss_formatted_record_expr, _ =
    Runtime.normalize_def stdlib_generics "protossFormattedRecordExpr"
  in
  assert_equal "stdlib Protoss.formatText record expr"
    "Ok \"(def person Person (record (name \\\"Ada\\\") (active true)))\""
    (Runtime.value_to_string protoss_formatted_record_expr);
  let protoss_formatted_fold_variant_expr, _ =
    Runtime.normalize_def stdlib_generics "protossFormattedFoldVariantExpr"
  in
  assert_equal "stdlib Protoss.formatText foldVariant expr"
    "Ok \"(def size Nat (foldVariant Tree Nat tree (Leaf x 1) (Node pair (recur (get pair left)))))\""
    (Runtime.value_to_string protoss_formatted_fold_variant_expr);
  let protoss_formatted_named_decls, _ =
    Runtime.normalize_def stdlib_generics "protossFormattedNamedDecls"
  in
  assert_equal "stdlib Protoss.formatText named decls"
    "Ok \"(record Pair (params A B) (first A) (second B))\\n(variant Tree (params A) (Leaf A) (Node (Tree A)))\""
    (Runtime.value_to_string protoss_formatted_named_decls);
  let protoss_formatted_module_file, _ =
    Runtime.normalize_def stdlib_generics "protossFormattedModuleFile"
  in
  assert_equal "stdlib Protoss.formatText module file"
    "Ok \"(module Demo.Math)\\n(import \\\"prelude.protoss\\\")\\n(export Number double)\\n(capabilities Human.ask Clock.read)\""
    (Runtime.value_to_string protoss_formatted_module_file);
  let protoss_formatted_poly_caps, _ =
    Runtime.normalize_def stdlib_generics "protossFormattedPolyCaps"
  in
  assert_equal "stdlib Protoss.formatText poly caps"
    "Ok \"(type Count Nat)\\n(defpoly id (params A) (-> A A) (lambda (x A) x))\\n(defcap askName (capabilities Human.ask) (Process String) (Human.ask \\\"Name?\\\"))\\n(defpolycap pure (params A) (capabilities) (-> A A) (lambda (x A) x))\""
    (Runtime.value_to_string protoss_formatted_poly_caps);
  let protoss_formatted_defrec, _ =
    Runtime.normalize_def stdlib_generics "protossFormattedDefRec"
  in
  assert_equal "stdlib Protoss.formatText defrec"
    "Ok \"(defrec count (-> Nat Nat) (nat n) (zero 0) (step acc (succ acc)))\\n(defrecpoly copy (params A) (-> (List A) (List A)) (list xs) (nil Nil) (cons item acc (Cons item acc)))\""
    (Runtime.value_to_string protoss_formatted_defrec);
  let protoss_term_names_def, _ =
    Runtime.normalize_def stdlib_generics "protossTermNamesDef"
  in
  assert_equal "stdlib Protoss.declTermNames def" "[\"Nat.add\", \"base\"]"
    (Runtime.value_to_string protoss_term_names_def);
  let protoss_type_names_def, _ =
    Runtime.normalize_def stdlib_generics "protossTypeNamesDef"
  in
  assert_equal "stdlib Protoss.declTypeNames def" "[\"Pair\"]"
    (Runtime.value_to_string protoss_type_names_def);
  let protoss_term_names_poly, _ =
    Runtime.normalize_def stdlib_generics "protossTermNamesPoly"
  in
  assert_equal "stdlib Protoss.declTermNames defpoly" "[]"
    (Runtime.value_to_string protoss_term_names_poly);
  let protoss_type_names_poly, _ =
    Runtime.normalize_def stdlib_generics "protossTypeNamesPoly"
  in
  assert_equal "stdlib Protoss.declTypeNames defpoly" "[\"Maybe\"]"
    (Runtime.value_to_string protoss_type_names_poly);
  let protoss_term_names_defrec, _ =
    Runtime.normalize_def stdlib_generics "protossTermNamesDefRec"
  in
  assert_equal "stdlib Protoss.declTermNames defrec"
    "[\"base\", \"Nat.add\", \"stepBase\"]"
    (Runtime.value_to_string protoss_term_names_defrec);
  let protoss_type_names_variant_decl, _ =
    Runtime.normalize_def stdlib_generics "protossTypeNamesVariantDecl"
  in
  assert_equal "stdlib Protoss.declTypeNames variant" "[\"Tree\"]"
    (Runtime.value_to_string protoss_type_names_variant_decl);
  let protoss_resolve_valid_missing_terms, _ =
    Runtime.normalize_def stdlib_generics "protossResolveValidMissingTerms"
  in
  assert_equal "stdlib Protoss.resolveText valid terms" "[]"
    (Runtime.value_to_string protoss_resolve_valid_missing_terms);
  let protoss_resolve_valid_missing_types, _ =
    Runtime.normalize_def stdlib_generics "protossResolveValidMissingTypes"
  in
  assert_equal "stdlib Protoss.resolveText valid types" "[]"
    (Runtime.value_to_string protoss_resolve_valid_missing_types);
  let protoss_resolve_valid_missing_exports, _ =
    Runtime.normalize_def stdlib_generics "protossResolveValidMissingExports"
  in
  assert_equal "stdlib Protoss.resolveText valid exports" "[]"
    (Runtime.value_to_string protoss_resolve_valid_missing_exports);
  let protoss_resolve_missing_terms, _ =
    Runtime.normalize_def stdlib_generics "protossResolveMissingTerms"
  in
  assert_equal "stdlib Protoss.resolveText missing terms" "[\"unknown\"]"
    (Runtime.value_to_string protoss_resolve_missing_terms);
  let protoss_resolve_missing_types, _ =
    Runtime.normalize_def stdlib_generics "protossResolveMissingTypes"
  in
  assert_equal "stdlib Protoss.resolveText missing types" "[\"MissingType\"]"
    (Runtime.value_to_string protoss_resolve_missing_types);
  let protoss_resolve_missing_exports, _ =
    Runtime.normalize_def stdlib_generics "protossResolveMissingExports"
  in
  assert_equal "stdlib Protoss.resolveText missing exports" "[\"nope\"]"
    (Runtime.value_to_string protoss_resolve_missing_exports);
  let protoss_resolve_duplicate_terms, _ =
    Runtime.normalize_def stdlib_generics "protossResolveDuplicateTerms"
  in
  assert_equal "stdlib Protoss.resolveText duplicate terms" "[\"a\"]"
    (Runtime.value_to_string protoss_resolve_duplicate_terms);
  let protoss_resolve_duplicate_types, _ =
    Runtime.normalize_def stdlib_generics "protossResolveDuplicateTypes"
  in
  assert_equal "stdlib Protoss.resolveText duplicate types" "[\"Box\"]"
    (Runtime.value_to_string protoss_resolve_duplicate_types);
  let protoss_resolve_external_missing_terms, _ =
    Runtime.normalize_def stdlib_generics "protossResolveExternalMissingTerms"
  in
  assert_equal "stdlib Protoss.resolveTextWith external terms" "[]"
    (Runtime.value_to_string protoss_resolve_external_missing_terms);
  let protoss_resolve_external_missing_types, _ =
    Runtime.normalize_def stdlib_generics "protossResolveExternalMissingTypes"
  in
  assert_equal "stdlib Protoss.resolveTextWith external types" "[]"
    (Runtime.value_to_string protoss_resolve_external_missing_types);
  let protoss_dep_order_valid, _ =
    Runtime.normalize_def stdlib_generics "protossDepOrderValid"
  in
  assert_equal "stdlib Protoss.termDependencyOrderText valid"
    "PDepOrderOk [\"a\", \"b\", \"c\"]"
    (Runtime.value_to_string protoss_dep_order_valid);
  let protoss_dep_order_cycle, _ =
    Runtime.normalize_def stdlib_generics "protossDepOrderCycle"
  in
  assert_equal "stdlib Protoss.termDependencyOrderText cycle"
    "PDepOrderCycle [\"a\", \"b\"]"
    (Runtime.value_to_string protoss_dep_order_cycle);
  let protoss_dep_order_external, _ =
    Runtime.normalize_def stdlib_generics "protossDepOrderExternal"
  in
  assert_equal "stdlib Protoss.termDependencyOrderText external"
    "PDepOrderOk [\"local\"]"
    (Runtime.value_to_string protoss_dep_order_external);
  let protoss_dep_nodes, _ = Runtime.normalize_def stdlib_generics "protossDepNodes" in
  assert_equal "stdlib Protoss.declsTermDepNodes"
    "[{deps = [\"b\"], name = \"c\"}, {deps = [\"a\"], name = \"b\"}, {deps = [], name = \"a\"}]"
    (Runtime.value_to_string protoss_dep_nodes);
  let protoss_type_dep_order_valid, _ =
    Runtime.normalize_def stdlib_generics "protossTypeDepOrderValid"
  in
  assert_equal "stdlib Protoss.typeDependencyOrderText valid"
    "PDepOrderOk [\"Id\", \"Box\", \"UseBox\"]"
    (Runtime.value_to_string protoss_type_dep_order_valid);
  let protoss_type_dep_order_cycle, _ =
    Runtime.normalize_def stdlib_generics "protossTypeDepOrderCycle"
  in
  assert_equal "stdlib Protoss.typeDependencyOrderText cycle"
    "PDepOrderCycle [\"A\", \"B\"]"
    (Runtime.value_to_string protoss_type_dep_order_cycle);
  let protoss_type_dep_order_recursive_variant, _ =
    Runtime.normalize_def stdlib_generics "protossTypeDepOrderRecursiveVariant"
  in
  assert_equal "stdlib Protoss.typeDependencyOrderText recursive variant"
    "PDepOrderOk [\"Tree\"]"
    (Runtime.value_to_string protoss_type_dep_order_recursive_variant);
  let protoss_type_dep_nodes, _ =
    Runtime.normalize_def stdlib_generics "protossTypeDepNodes"
  in
  assert_equal "stdlib Protoss.declsTypeDepNodes"
    "[{deps = [\"Box\"], name = \"UseBox\"}, {deps = [\"Id\"], name = \"Box\"}, {deps = [], name = \"Id\"}]"
    (Runtime.value_to_string protoss_type_dep_nodes);
  let protoss_type_env_entries, _ =
    Runtime.normalize_def stdlib_generics "protossTypeEnvValidEntries"
  in
  assert_equal "stdlib Protoss.checkTypeEnvText entries"
    "[{arity = 0, deps = [\"Box\"], kind = \"variant\", name = \"UseBox\"}, {arity = 1, deps = [], kind = \"record\", name = \"Box\"}, {arity = 0, deps = [], kind = \"alias\", name = \"Id\"}]"
    (Runtime.value_to_string protoss_type_env_entries);
  let protoss_type_env_order, _ =
    Runtime.normalize_def stdlib_generics "protossTypeEnvValidOrder"
  in
  assert_equal "stdlib Protoss.checkTypeEnvText order"
    "PDepOrderOk [\"Box\", \"Id\", \"UseBox\"]"
    (Runtime.value_to_string protoss_type_env_order);
  let protoss_type_env_valid_arity_issues, _ =
    Runtime.normalize_def stdlib_generics "protossTypeEnvValidArityIssues"
  in
  assert_equal "stdlib Protoss.checkTypeEnvText valid arity"
    "[]"
    (Runtime.value_to_string protoss_type_env_valid_arity_issues);
  let protoss_type_env_duplicate_type, _ =
    Runtime.normalize_def stdlib_generics "protossTypeEnvDuplicateType"
  in
  assert_equal "stdlib Protoss.checkTypeEnvText duplicate type"
    "Err \"duplicate type: Box\""
    (Runtime.value_to_string protoss_type_env_duplicate_type);
  let protoss_type_env_missing_type, _ =
    Runtime.normalize_def stdlib_generics "protossTypeEnvMissingType"
  in
  assert_equal "stdlib Protoss.checkTypeEnvText missing type"
    "Err \"missing type: Missing\""
    (Runtime.value_to_string protoss_type_env_missing_type);
  let protoss_type_env_duplicate_param, _ =
    Runtime.normalize_def stdlib_generics "protossTypeEnvDuplicateParam"
  in
  assert_equal "stdlib Protoss.checkTypeEnvText duplicate param"
    "Err \"duplicate type parameter: Box.A\""
    (Runtime.value_to_string protoss_type_env_duplicate_param);
  let protoss_type_env_duplicate_record_field, _ =
    Runtime.normalize_def stdlib_generics "protossTypeEnvDuplicateRecordField"
  in
  assert_equal "stdlib Protoss.checkTypeEnvText duplicate record field"
    "Err \"duplicate record field: Box.value\""
    (Runtime.value_to_string protoss_type_env_duplicate_record_field);
  let protoss_type_env_duplicate_variant_case, _ =
    Runtime.normalize_def stdlib_generics "protossTypeEnvDuplicateVariantCase"
  in
  assert_equal "stdlib Protoss.checkTypeEnvText duplicate variant case"
    "Err \"duplicate variant case: Maybe.Some\""
    (Runtime.value_to_string protoss_type_env_duplicate_variant_case);
  let protoss_type_env_cycle, _ =
    Runtime.normalize_def stdlib_generics "protossTypeEnvCycle"
  in
  assert_equal "stdlib Protoss.checkTypeEnvText cycle"
    "Err \"cyclic type dependency: A,B\""
    (Runtime.value_to_string protoss_type_env_cycle);
  let protoss_type_env_wrong_arity_named, _ =
    Runtime.normalize_def stdlib_generics "protossTypeEnvWrongArityNamed"
  in
  assert_equal "stdlib Protoss.checkTypeEnvText named arity"
    "Err \"wrong type arity: Box expected 1 got 2\""
    (Runtime.value_to_string protoss_type_env_wrong_arity_named);
  let protoss_type_env_wrong_arity_builtin, _ =
    Runtime.normalize_def stdlib_generics "protossTypeEnvWrongArityBuiltin"
  in
  assert_equal "stdlib Protoss.checkTypeEnvText builtin arity"
    "Err \"wrong type arity: List expected 1 got 0\""
    (Runtime.value_to_string protoss_type_env_wrong_arity_builtin);
  let protoss_type_env_wrong_arity_param, _ =
    Runtime.normalize_def stdlib_generics "protossTypeEnvWrongArityParam"
  in
  assert_equal "stdlib Protoss.checkTypeEnvText param arity"
    "Err \"wrong type arity: A expected 0 got 1\""
    (Runtime.value_to_string protoss_type_env_wrong_arity_param);
  let protoss_type_env_arity_issues, _ =
    Runtime.normalize_def stdlib_generics "protossTypeEnvArityIssues"
  in
  assert_equal "stdlib Protoss.typeEnvReportText arity issues"
    "[{actual = 2, expected = 1, name = \"Box\"}]"
    (Runtime.value_to_string protoss_type_env_arity_issues);
  let protoss_capability_valid_declared, _ =
    Runtime.normalize_def stdlib_generics "protossCapabilityValidDeclared"
  in
  assert_equal "stdlib Protoss.checkCapabilityText declared"
    "[\"Human.ask\", \"Clock.read\"]"
    (Runtime.value_to_string protoss_capability_valid_declared);
  let protoss_capability_valid_used, _ =
    Runtime.normalize_def stdlib_generics "protossCapabilityValidUsed"
  in
  assert_equal "stdlib Protoss.checkCapabilityText used"
    "[\"Clock.read\", \"Human.ask\"]"
    (Runtime.value_to_string protoss_capability_valid_used);
  let protoss_capability_valid_scoped, _ =
    Runtime.normalize_def stdlib_generics "protossCapabilityValidScoped"
  in
  assert_equal "stdlib Protoss.checkCapabilityText scoped" "[\"Human.ask\"]"
    (Runtime.value_to_string protoss_capability_valid_scoped);
  let protoss_capability_valid_missing, _ =
    Runtime.normalize_def stdlib_generics "protossCapabilityValidMissing"
  in
  assert_equal "stdlib Protoss.checkCapabilityText valid missing" "[]"
    (Runtime.value_to_string protoss_capability_valid_missing);
  let protoss_capability_missing_declaration, _ =
    Runtime.normalize_def stdlib_generics "protossCapabilityMissingDeclaration"
  in
  assert_equal "stdlib Protoss.checkCapabilityText missing declaration"
    "Err \"missing capability declaration: Human.ask\""
    (Runtime.value_to_string protoss_capability_missing_declaration);
  let protoss_capability_unknown, _ =
    Runtime.normalize_def stdlib_generics "protossCapabilityUnknown"
  in
  assert_equal "stdlib Protoss.checkCapabilityText unknown"
    "Err \"unknown capability: Space.laser\""
    (Runtime.value_to_string protoss_capability_unknown);
  let protoss_capability_duplicate_declaration, _ =
    Runtime.normalize_def stdlib_generics "protossCapabilityDuplicateDeclaration"
  in
  assert_equal "stdlib Protoss.checkCapabilityText duplicate declaration"
    "Err \"duplicate capability declaration: Human.ask\""
    (Runtime.value_to_string protoss_capability_duplicate_declaration);
  let protoss_capability_duplicate_scope, _ =
    Runtime.normalize_def stdlib_generics "protossCapabilityDuplicateScope"
  in
  assert_equal "stdlib Protoss.checkCapabilityText duplicate scope"
    "Err \"duplicate scoped capability: ask.Human.ask\""
    (Runtime.value_to_string protoss_capability_duplicate_scope);
  let protoss_static_valid_missing_terms, _ =
    Runtime.normalize_def stdlib_generics "protossStaticValidMissingTerms"
  in
  assert_equal "stdlib Protoss.checkStaticText valid terms" "[]"
    (Runtime.value_to_string protoss_static_valid_missing_terms);
  let protoss_static_valid_term_order, _ =
    Runtime.normalize_def stdlib_generics "protossStaticValidTermOrder"
  in
  assert_equal "stdlib Protoss.checkStaticText term order"
    "PDepOrderOk [\"base\", \"ask\", \"askAgain\"]"
    (Runtime.value_to_string protoss_static_valid_term_order);
  let protoss_static_valid_type_order, _ =
    Runtime.normalize_def stdlib_generics "protossStaticValidTypeOrder"
  in
  assert_equal "stdlib Protoss.checkStaticText type order" "PDepOrderOk [\"Box\"]"
    (Runtime.value_to_string protoss_static_valid_type_order);
  let protoss_static_valid_capabilities, _ =
    Runtime.normalize_def stdlib_generics "protossStaticValidCapabilities"
  in
  assert_equal "stdlib Protoss.checkStaticText capabilities" "[\"Human.ask\"]"
    (Runtime.value_to_string protoss_static_valid_capabilities);
  let protoss_static_missing_term, _ =
    Runtime.normalize_def stdlib_generics "protossStaticMissingTerm"
  in
  assert_equal "stdlib Protoss.checkStaticText missing term"
    "Err \"missing term: unknown\""
    (Runtime.value_to_string protoss_static_missing_term);
  let protoss_static_term_cycle, _ =
    Runtime.normalize_def stdlib_generics "protossStaticTermCycle"
  in
  assert_equal "stdlib Protoss.checkStaticText term cycle"
    "Err \"cyclic term dependency: a,b\""
    (Runtime.value_to_string protoss_static_term_cycle);
  let protoss_static_type_arity, _ =
    Runtime.normalize_def stdlib_generics "protossStaticTypeArity"
  in
  assert_equal "stdlib Protoss.checkStaticText type arity"
    "Err \"wrong type arity: Box expected 1 got 2\""
    (Runtime.value_to_string protoss_static_type_arity);
  let protoss_static_missing_capability, _ =
    Runtime.normalize_def stdlib_generics "protossStaticMissingCapability"
  in
  assert_equal "stdlib Protoss.checkStaticText missing capability"
    "Err \"missing capability declaration: Human.ask\""
    (Runtime.value_to_string protoss_static_missing_capability);
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
  let json_rendered_profile, _ = Runtime.normalize_def stdlib_generics "jsonRenderedProfile" in
  assert_equal "stdlib Json.render object" "\"{\\\"name\\\":\\\"Ada\\\",\\\"age\\\":41}\""
    (Runtime.value_to_string json_rendered_profile);
  let json_rendered_array, _ = Runtime.normalize_def stdlib_generics "jsonRenderedArray" in
  assert_equal "stdlib Json.render array" "\"[true,null,3]\""
    (Runtime.value_to_string json_rendered_array);
  let json_rendered_escaped, _ =
    Runtime.normalize_def stdlib_generics "jsonRenderedEscaped"
  in
  assert_equal "stdlib Json.render escaped string"
    "\"\\\"Ada\\\\n\\\\\\\"Lovelace\\\\\\\"\\\\\\\\lab\\\"\""
    (Runtime.value_to_string json_rendered_escaped);
  let json_lexed_object, _ = Runtime.normalize_def stdlib_generics "jsonLexedObject" in
  assert_equal "stdlib Json.lexTokens object"
    "Ok [JTLBrace unit, JTString \"name\", JTColon unit, JTString \"Ada\", JTComma unit, JTString \"age\", JTColon unit, JTNat 41, JTRBrace unit]"
    (Runtime.value_to_string json_lexed_object);
  let json_parsed_object, _ = Runtime.normalize_def stdlib_generics "jsonParsedObject" in
  assert_equal "stdlib Json.parseText object"
    "Ok JObject [{first = \"name\", second = JString \"Ada\"}, {first = \"age\", second = JNat 41}]"
    (Runtime.value_to_string json_parsed_object);
  let json_parsed_array, _ = Runtime.normalize_def stdlib_generics "jsonParsedArray" in
  assert_equal "stdlib Json.parseText array" "Ok JArray [JBool true, JNull unit, JNat 3]"
    (Runtime.value_to_string json_parsed_array);
  let json_parsed_nested, _ = Runtime.normalize_def stdlib_generics "jsonParsedNested" in
  assert_equal "stdlib Json.parseText nested"
    "Ok JObject [{first = \"items\", second = JArray [JObject [{first = \"ok\", second = JBool true}], JNull unit]}]"
    (Runtime.value_to_string json_parsed_nested);
  let json_parsed_escaped, _ = Runtime.normalize_def stdlib_generics "jsonParsedEscaped" in
  assert_equal "stdlib Json.parseText escaped string"
    "Ok JString \"Ada\\n\\\"Lovelace\\\"\\\\lab\""
    (Runtime.value_to_string json_parsed_escaped);
  let json_parsed_bad_trailing_comma, _ =
    Runtime.normalize_def stdlib_generics "jsonParsedBadTrailingComma"
  in
  assert_equal "stdlib Json.parseText trailing comma" "Err \"expected JSON value\""
    (Runtime.value_to_string json_parsed_bad_trailing_comma);
  let json_parsed_bad_object_value, _ =
    Runtime.normalize_def stdlib_generics "jsonParsedBadObjectValue"
  in
  assert_equal "stdlib Json.parseText missing object value" "Err \"expected JSON value\""
    (Runtime.value_to_string json_parsed_bad_object_value);
  let json_parsed_bad_keyword, _ =
    Runtime.normalize_def stdlib_generics "jsonParsedBadKeyword"
  in
  assert_equal "stdlib Json.parseText bad keyword" "Err \"invalid JSON keyword\""
    (Runtime.value_to_string json_parsed_bad_keyword);
  let json_parsed_rendered_object, _ =
    Runtime.normalize_def stdlib_generics "jsonParsedRenderedObject"
  in
  assert_equal "stdlib Json.parseText render round-trip"
    "\"{\\\"name\\\":\\\"Ada\\\",\\\"age\\\":41}\""
    (Runtime.value_to_string json_parsed_rendered_object)
  ) else
    print_endline "stdlib interpreter tests skipped (set PROTOSS_RUN_STDLIB_TESTS=1)";
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
  let human_module_math = Filename.concat module_root "human_math.protoss" in
  let human_module_app = Filename.concat module_root "human_app.protoss" in
  let human_module_bad = Filename.concat module_root "human_bad.protoss" in
  write_file human_module_math
    ("module Demo.HumanMath exposing (Number, double)\n\
      import " ^ Ast.quote stdlib_path ^ " exposing (..)\n\n\
      type alias Number = Nat\n\n\
      hidden : Number\n\
      hidden = 2\n\n\
      double : Number -> Number\n\
      double x = x + hidden\n");
  write_file human_module_app
    "import \"human_math.protoss\" exposing (double)\n\n\
     result : Demo.HumanMath.Number\n\
     result = Demo.HumanMath.double 4\n";
  let human_module_checked = Loader.check_file human_module_app in
  let human_module_result, _ = Runtime.normalize_def human_module_checked "result" in
  assert_equal "human module exposing import" "6"
    (Runtime.value_to_string human_module_result);
  write_file human_module_bad
    "import \"human_math.protoss\" exposing (hidden)\n\n\
     leak : Demo.HumanMath.Number\n\
     leak = Demo.HumanMath.hidden\n";
  (try
     ignore (Loader.check_file human_module_bad);
     fail "human import exposing should not bypass module exports"
   with Loader.Error msg ->
     assert_true "human import exposing private module export error" (String.contains msg 'e'));
  write_file module_bad
    "(import \"math.protoss\")\n(def leak Demo.Math.Number Demo.Math.hidden)\n";
  (try
     ignore (Loader.check_file module_bad);
     fail "private module definition should not be importable"
   with Loader.Error msg -> assert_true "private module export error" (String.contains msg 'e'));
  let module_bad_type = Filename.concat module_root "bad_type.protoss" in
  write_file module_bad_type "(module Demo.Bad)\n(def hidden Nat\n  true)\n";
  (try
     ignore (Loader.check_file module_bad_type);
     fail "qualified module type error should point at source definition body"
   with Loader.Error msg ->
     assert_true "qualified module type error has expression line"
       (contains_substring msg (module_bad_type ^ ":3:3: definition Demo.Bad.hidden")));
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
  let bad_nested_type_file = Filename.concat import_root "bad_nested_type.protoss" in
  write_file bad_nested_type_file "(def bad Nat\n  (succ true))\n";
  (try
     ignore (Loader.check_file bad_nested_type_file);
     fail "loader nested type error should point at bad argument"
   with Loader.Error msg ->
     assert_true "loader nested type error has expression line"
       (contains_substring msg (bad_nested_type_file ^ ":2:9: definition bad")));
  let bad_unknown_file = Filename.concat import_root "bad_unknown.protoss" in
  write_file bad_unknown_file "(def bad Nat\n  (let (x 1)\n    miss))\n";
  (try
     ignore (Loader.check_file bad_unknown_file);
     fail "loader unknown name should point at missing name"
   with Loader.Error msg ->
     assert_true "loader unknown name has expression line"
       (contains_substring msg (bad_unknown_file ^ ":3:5: definition bad")));
  let bad_syntax_file = Filename.concat import_root "bad_syntax.protoss" in
  write_file bad_syntax_file "(def bad Nat\n  (succ 1)\n";
  (try
     ignore (Loader.check_file bad_syntax_file);
     fail "loader syntax error should be localized"
   with Loader.Error msg ->
     assert_true "loader syntax error has file line column"
       (contains_substring msg (bad_syntax_file ^ ":1:1: unterminated list")));
  let bad_defcap_file = Filename.concat import_root "bad_defcap.protoss" in
  write_file bad_defcap_file
    "; ignored comment mentioning (defcap fake ...)\n\n\
     (capabilities Human.ask)\n\
     (defcap askName (capabilities) (Process String) (Human.ask \"Name?\"))\n";
  (try
     ignore (Loader.check_file bad_defcap_file);
     fail "loader defcap error should be localized"
   with Loader.Error msg ->
     assert_true "loader defcap error has exact line"
       (contains_substring msg (bad_defcap_file ^ ":4:1: definition askName")));
  let bad_defrec_file = Filename.concat import_root "bad_defrec.protoss" in
  write_file bad_defrec_file
    "\n(defrec count (-> Nat Nat) (nat n) (zero 0) (step acc true))\n";
  (try
     ignore (Loader.check_file bad_defrec_file);
     fail "loader defrec error should be localized"
   with Loader.Error msg ->
     assert_true "loader defrec error has exact line"
       (contains_substring msg (bad_defrec_file ^ ":2:55: definition count")));
  let bad_alias_file = Filename.concat import_root "bad_alias.protoss" in
  write_file bad_alias_file
    "; ignored comment mentioning (type Loop Nat)\n\n(type Loop Loop)\n(def bad Loop 0)\n";
  (try
     ignore (Loader.check_file bad_alias_file);
     fail "loader recursive alias error should be localized"
   with Loader.Error msg ->
     assert_true "loader recursive alias error has exact line"
       (contains_substring msg
          (bad_alias_file
         ^ ":3:1: recursive type alias must be guarded by a Variant constructor: Loop")));
  let duplicate_alias_file = Filename.concat import_root "duplicate_alias.protoss" in
  write_file duplicate_alias_file "(type A Nat)\n\n(type A Bool)\n(def x A 0)\n";
  (try
     ignore (Loader.check_file duplicate_alias_file);
     fail "loader duplicate alias error should be localized"
   with Loader.Error msg ->
     assert_true "loader duplicate alias error points at duplicate"
       (contains_substring msg (duplicate_alias_file ^ ":3:1: duplicate type alias: A")));
  let cyclic_alias_file = Filename.concat import_root "cyclic_alias.protoss" in
  write_file cyclic_alias_file "\n(type A B)\n(type B A)\n(def bad A 0)\n";
  (try
     ignore (Loader.check_file cyclic_alias_file);
     fail "loader cyclic alias error should be localized"
   with Loader.Error msg ->
     assert_true "loader cyclic alias error has exact line"
       (contains_substring msg (cyclic_alias_file ^ ":2:1: cyclic type alias: A -> B -> A")));
  let bad_alias_arity_file = Filename.concat import_root "bad_alias_arity.protoss" in
  write_file bad_alias_arity_file
    "(type Pair (A B) (Record (left A) (right B)))\n\
     (def bad (Pair Nat) (record (left 0) (right 1)))\n";
  (try
     ignore (Loader.check_file bad_alias_arity_file);
     fail "loader alias arity error should be localized"
   with Loader.Error msg ->
     assert_true "loader alias arity error has exact line"
       (contains_substring msg
          (bad_alias_arity_file ^ ":1:1: type alias Pair expects 2 argument(s), got 1")));
  let duplicate_alias_param_file = Filename.concat import_root "duplicate_alias_param.protoss" in
  write_file duplicate_alias_param_file "(type Box (A A) A)\n(def bad (Box Nat) 0)\n";
  (try
     ignore (Loader.check_file duplicate_alias_param_file);
     fail "loader duplicate alias param error should be localized"
   with Loader.Error msg ->
     assert_true "loader duplicate alias param error has exact line"
       (contains_substring msg
          (duplicate_alias_param_file ^ ":1:1: duplicate type parameter A in alias Box")));

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
  let human_process =
    check "capabilities Human.ask\naskName : Process String\naskName = Human.ask \"Name?\"\n"
  in
  let human_pv, _ = Runtime.eval_entry human_process "askName" in
  assert_true "human capabilities declaration should allow process request"
    (match human_pv with
    | Runtime.VProcessRequest { Runtime.req = Ast.AskHuman "Name?"; _ } -> true
    | _ -> false);
  expect_check_error "askName : Process String\naskName = Human.ask \"Name?\"\n";
  let human_scoped_process =
    check
      "capabilities Human.ask\n\
       askScoped : Process { Human.ask } String\n\
       askScoped = Human.ask \"Name?\"\n"
  in
  assert_equal "human Process capability scope" "Human.ask"
    (String.concat "," (checked_def human_scoped_process "askScoped").Kernel.capabilities);
  assert_equal "human Process capability type visible"
    "(Process (capabilities Human.ask) String)"
    (Ast.string_of_typ (checked_def human_scoped_process "askScoped").Kernel.def.typ);
  let human_scoped_pv, _ = Runtime.eval_entry human_scoped_process "askScoped" in
  assert_true "human Process capability type should allow process request"
    (match human_scoped_pv with
    | Runtime.VProcessRequest { Runtime.req = Ast.AskHuman "Name?"; _ } -> true
    | _ -> false);
  let sexp_scoped_process =
    check
      "(capabilities Human.ask)\n\
       (def askScoped (Process (capabilities Human.ask) String) (Human.ask \"Name?\"))"
  in
  assert_equal "sexp Process capability type visible"
    "(Process (capabilities Human.ask) String)"
    (Ast.string_of_typ (checked_def sexp_scoped_process "askScoped").Kernel.def.typ);
  expect_check_error
    "capabilities Human.ask\nbad : Process { } String\nbad = Human.ask \"Name?\"\n";
  expect_check_error
    "(capabilities Human.ask)\n\
     (def bad (Process (capabilities) String) (Human.ask \"Name?\"))";
  let scoped_process_graph = Json.parse (Canonical_ir.serialize_graph human_scoped_process) in
  assert_equal "Process type graph exposes capabilities" "Human.ask"
    (String.concat ","
       (json_string_array_field "capabilities"
          (json_field "type" (graph_def scoped_process_graph "askScoped"))));
  let effect_sensors_path = find_up (Sys.getcwd ()) "examples/effect_sensors.protoss" in
  let effect_sensors = Loader.check_file effect_sensors_path in
  let assert_sensor_request name expected_req expected_scope =
    let value, _ = Runtime.eval_entry effect_sensors name in
    match value with
    | Runtime.VProcessRequest suspended ->
        assert_true ("sensor fixture request " ^ name)
          (match (suspended.Runtime.req, expected_req) with
          | Ast.ReadClock, Ast.ReadClock -> true
          | Ast.HttpGet a, Ast.HttpGet b -> String.equal a b
          | Ast.ServerRequest (ar, ap), Ast.ServerRequest (br, bp) ->
              String.equal ar br && String.equal ap bp
          | _ -> false);
        assert_equal ("sensor fixture scope " ^ name) expected_scope
          (String.concat "," suspended.Runtime.cap_scope);
        assert_equal ("sensor fixture response type " ^ name) "\"ok\""
          (Runtime.value_to_string (Runtime.response_value suspended.req "String:ok"))
    | other -> fail ("sensor fixture should suspend " ^ name ^ ", got " ^ Runtime.value_to_string other)
  in
  assert_sensor_request "readTime" Ast.ReadClock "Clock.read";
  assert_sensor_request "fetchStatus"
    (Ast.HttpGet "https://example.invalid/status")
    "Http.get";
  assert_sensor_request "askSensor"
    (Ast.ServerRequest ("/sensor/read", "{\"sensor\":\"temperature\"}"))
    "Server.request";
  let secret_leak_risk_path = find_up (Sys.getcwd ()) "examples/secret_leak_risk.protoss" in
  let secret_leak_risk = Loader.check_file secret_leak_risk_path in
  assert_true "SecretLeakRisk detects local storage plus outbound request"
    (List.exists
       (fun line ->
         contains_substring line "SecretLeakRisk"
         && contains_substring line "Local.storage"
         && contains_substring line "Http.get")
       (Kernel.secret_leak_risks secret_leak_risk));
  let secret_ref_type = Ast.TSecretRef ("user", Ast.TString) in
  assert_equal "SecretRef type canonical" "(SecretRef user String)"
    (Kernel.type_to_canonical secret_ref_type);
  let secret_ref_program =
    check "(type ApiToken (SecretRef user String))\n(def main Nat 0)"
  in
  assert_equal "SecretRef type alias parses" "(SecretRef user String)"
    (Ast.string_of_typ (List.hd secret_ref_program.Kernel.program.type_aliases).Ast.type_body);
  let sealed_a =
    Json.parse
      (Secrets.seal_json ~scope:"user" ~typ:Ast.TString ~handle:"token-handle"
         ~value:"raw-secret-a")
  in
  let sealed_b =
    Json.parse
      (Secrets.seal_json ~scope:"user" ~typ:Ast.TString ~handle:"token-handle"
         ~value:"raw-secret-b")
  in
  assert_equal "sealed secret hashes handle not value"
    (json_string_field "handleRef" sealed_a)
    (json_string_field "handleRef" sealed_b);
  assert_true "sealed secret never stores raw value"
    (not
       (contains_substring
          (Secrets.seal_json ~scope:"user" ~typ:Ast.TString ~handle:"token-handle"
             ~value:"raw-secret-a")
          "raw-secret-a"));
  assert_true "sealed secret JSON marks value un-hashed"
    (not (json_bool_field "valueHashed" sealed_a));
  let defcap_process =
    check
      "(capabilities Human.ask)\n\
       (def askName (Process String) (Human.ask \"Name?\"))\n\
       (defcap askAgain (capabilities Human.ask) (Process String) askName)\n\
       (defcap pure (capabilities) Nat 0)"
  in
  let plain_defcap_process =
    check
      "(capabilities Human.ask)\n\
       (def askName (Process String) (Human.ask \"Name?\"))\n\
       (def askAgain (Process String) askName)\n\
       (def pure Nat 0)"
  in
  assert_equal "defcap does not affect canonical hash"
    (Kernel.hash_program plain_defcap_process)
    (Kernel.hash_program defcap_process);
  assert_equal "defcap inherited scope" "Human.ask"
    (String.concat "," (checked_def defcap_process "askAgain").Kernel.capabilities);
  assert_equal "defcap empty pure scope" ""
    (String.concat "," (checked_def defcap_process "pure").Kernel.capabilities);
  let defpolycap_pure =
    check
      "(defpolycap id (params A) (capabilities) (-> A A) (lambda (x A) x))\n\
       (def out Nat (id 4))"
  in
  let defpolycap_out, _ = Runtime.normalize_def defpolycap_pure "out" in
  assert_equal "defpolycap pure normalizes" "4"
    (Runtime.value_to_string defpolycap_out);
  expect_check_error
    "(capabilities Human.ask)\n\
     (defcap askName (capabilities) (Process String) (Human.ask \"Name?\"))";
  expect_check_error
    "(capabilities Human.ask)\n\
     (defcap pure (capabilities Human.ask) Nat 0)";
  expect_check_error
    "(defcap pure (capabilities Human.ask) Nat 0)";
  let process_graph_json = Canonical_ir.serialize_graph process in
  let process_graph = Json.parse process_graph_json in
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
  let graph_capabilities = Canonical_ir.graph_capabilities process_graph_json in
  assert_equal "process graph inspected capability count" "1"
    (string_of_int (List.length graph_capabilities));
  assert_true "process graph inspected capability describes request"
    (contains_substring (Canonical_ir.describe_graph_capabilities graph_capabilities)
       "request=AskHuman");
  let inspected_human_capability = Canonical_ir.graph_capability process_graph_json "Human.ask" in
  assert_equal "process graph inspected capability ref" human_capability_ref
    inspected_human_capability.Canonical_ir.graph_cap_ref;
  let inspected_human_request =
    match inspected_human_capability.Canonical_ir.graph_cap_requests with
    | [ request ] -> request
    | _ -> fail "Human.ask should have one request signature"
  in
  assert_equal "process graph inspected capability request ref" human_signature_ref
    inspected_human_request.Canonical_ir.graph_cap_req_ref;
  assert_equal "process graph inspected capability payload type" "(Record (prompt String))"
    (Ast.string_of_typ inspected_human_request.Canonical_ir.graph_cap_req_payload_type);
  assert_equal "process graph inspected capability response type" "String"
    (Ast.string_of_typ inspected_human_request.Canonical_ir.graph_cap_req_response_type);
  let graph_capability_scopes = Canonical_ir.graph_capability_scopes process_graph_json in
  assert_equal "process graph capability scope count" "1"
    (string_of_int (List.length graph_capability_scopes));
  assert_true "process graph capability scope describes def"
    (contains_substring (Canonical_ir.describe_graph_capability_scopes graph_capability_scopes)
       "def=askName");
  assert_true "process graph capability scope describes capability"
    (contains_substring (Canonical_ir.describe_graph_capability_scopes graph_capability_scopes)
       "capability=Human.ask");
  assert_equal "process graph capability scope filter by name" "1"
    (string_of_int
       (List.length (Canonical_ir.graph_capability_scopes_for process_graph_json "Human.ask")));
  assert_equal "process graph capability scope filter by ref" "1"
    (string_of_int
       (List.length
          (Canonical_ir.graph_capability_scopes_for process_graph_json human_capability_ref)));
  let process_agent_capability =
    Json.parse (Canonical_ir.agent_graph_capability_json process_graph_json "Human.ask")
  in
  let process_agent_capability_body = json_field "capability" process_agent_capability in
  assert_equal "process agent graph capability query" "capability"
    (json_string_field "query" process_agent_capability);
  assert_equal "process agent graph capability ref" human_capability_ref
    (json_string_field "ref" process_agent_capability_body);
  let process_agent_request =
    List.hd (json_array_field "requests" process_agent_capability_body)
  in
  assert_equal "process agent graph request tag" "AskHuman"
    (json_string_field "tag" process_agent_request);
  assert_equal "process agent graph request payload" "(Record (prompt String))"
    (json_string_field "payloadTypeCanonical" process_agent_request);
  let process_agent_scopes =
    Json.parse
      (Canonical_ir.agent_graph_capability_scopes_json process_graph_json (Some "Human.ask"))
  in
  assert_equal "process agent graph scope query" "capability-scopes"
    (json_string_field "query" process_agent_scopes);
  assert_equal "process agent graph scope count" "1"
    (string_of_int (List.length (json_array_field "capabilityScopes" process_agent_scopes)));
  let process_host_contract_json = Canonical_ir.graph_host_contract process_graph_json in
  let process_host_contract = Json.parse process_host_contract_json in
  let process_agent_host =
    Json.parse (Canonical_ir.agent_graph_host_contract_json process_graph_json)
  in
  let process_agent_host_contract = json_field "hostContract" process_agent_host in
  assert_equal "process agent graph host query" "host-contract"
    (json_string_field "query" process_agent_host);
  assert_equal "process agent graph host format" "protoss-host-contract-v1"
    (json_string_field "format" process_agent_host_contract);
  assert_equal "process host contract format" "protoss-host-contract-v1"
    (json_string_field "format" process_host_contract);
  assert_equal "process host contract codec version" Canonical_ir.host_codec_version
    (json_string_field "hostCodecVersion" process_host_contract);
  assert_equal "process host contract graph hash" (json_string_field "graphHash" process_graph)
    (json_string_field "graphHash" process_host_contract);
  assert_true "process host contract hash"
    (String.length (json_string_field "contractHash" process_host_contract) > 3);
  assert_equal "process host contract deterministic" process_host_contract_json
    (Canonical_ir.graph_host_contract process_graph_json);
  assert_equal "process host contract check" "Host contract OK\n"
    (Canonical_ir.check_graph_host_contract process_graph_json process_host_contract_json);
  let process_host_caps = json_array_field "capabilities" process_host_contract in
  assert_equal "process host contract capability count" "1"
    (string_of_int (List.length process_host_caps));
  let process_host_cap = List.hd process_host_caps in
  assert_equal "process host contract capability ref" human_capability_ref
    (json_string_field "capabilityRef" process_host_cap);
  let process_host_requests = json_array_field "requests" process_host_cap in
  let process_host_request = List.hd process_host_requests in
  assert_equal "process host contract request ref" human_signature_ref
    (json_string_field "requestSignatureRef" process_host_request);
  assert_equal "process host contract payload type" "(Record (prompt String))"
    (json_string_field "payloadTypeCanonical" process_host_request);
  let process_host_request_codec = json_field "requestCodec" process_host_request in
  let process_payload_type = Ast.TRecord [ ("prompt", Ast.TString) ] in
  let process_payload_type_canonical = Kernel.type_to_canonical process_payload_type in
  assert_equal "process host contract request codec format" Canonical_ir.host_codec_version
    (json_string_field "format" process_host_request_codec);
  assert_equal "process host contract request codec type" process_payload_type_canonical
    (json_string_field "typeCanonical" process_host_request_codec);
  assert_equal "process host contract request codec type hash"
    (Kernel.hash_string process_payload_type_canonical)
    (json_string_field "typeHash" process_host_request_codec);
  assert_equal "process host contract request codec ref"
    (Canonical_ir.host_codec_ref process_payload_type)
    (json_string_field "codecRef" process_host_request_codec);
  assert_equal "process host contract response type" "String"
    (json_string_field "responseTypeCanonical" process_host_request);
  let process_host_response_codec = json_field "responseCodec" process_host_request in
  let process_response_type_canonical = Kernel.type_to_canonical Ast.TString in
  assert_equal "process host contract response codec format" Canonical_ir.host_codec_version
    (json_string_field "format" process_host_response_codec);
  assert_equal "process host contract response codec type" process_response_type_canonical
    (json_string_field "typeCanonical" process_host_response_codec);
  assert_equal "process host contract response codec type hash"
    (Kernel.hash_string process_response_type_canonical)
    (json_string_field "typeHash" process_host_response_codec);
  assert_equal "process host contract response codec ref"
    (Canonical_ir.host_codec_ref Ast.TString)
    (json_string_field "codecRef" process_host_response_codec);
  let process_host_scopes = json_array_field "capabilityScopes" process_host_contract in
  assert_equal "process host contract scope count" "1"
    (string_of_int (List.length process_host_scopes));
  let process_host_scope = List.hd process_host_scopes in
  assert_equal "process host contract scope def" "askName"
    (json_string_field "def" process_host_scope);
  assert_equal "process host contract scope capability" "Human.ask"
    (json_string_field "capability" process_host_scope);
  assert_equal "process host contract scope ref"
    (Kernel.capability_scope_ref [ "Human.ask" ])
    (json_string_field "scopeRef" process_host_scope);
  (try
     ignore
       (Canonical_ir.check_graph_host_contract process_graph_json
          (replace_once process_host_contract_json "Human.ask" "Clock.read"));
     fail "drifted host contract should be rejected"
   with Kernel.Error msg ->
     assert_true "drifted host contract mismatch"
       (contains_substring msg "host contract mismatch"));
  (try
     ignore
       (Canonical_ir.check_graph_host_contract process_graph_json
          (replace_once process_host_contract_json
             (Canonical_ir.host_codec_ref process_payload_type)
             "p2:bad-codec"));
     fail "drifted host contract codec should be rejected"
   with Kernel.Error msg ->
     assert_true "drifted host contract codec mismatch"
       (contains_substring msg "host contract mismatch"));
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
  assert_equal "process graph def capability scope ref"
    (Kernel.capability_scope_ref [ "Human.ask" ])
    (json_string_field "capabilityScopeRef" (graph_def process_graph "askName"));
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
             "\"capabilityScopeRef\": " "\"capabilityScopeRefMissing\": "));
     fail "canonical graph should reject missing capability scope ref"
   with Kernel.Error msg ->
     assert_true "canonical graph rejects missing capability scope ref"
       (contains_substring msg "canonical graph missing field: capabilityScopeRef"));
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
  (try
     ignore
       (Canonical_ir.parse_graph
          (replace_once (Canonical_ir.serialize_graph process)
             ("\"capabilityScopeRef\": "
             ^ Ast.quote (Kernel.capability_scope_ref [ "Human.ask" ]))
             "\"capabilityScopeRef\": \"p2:bad\""));
     fail "canonical graph should reject corrupt capability scope ref"
   with Kernel.Error msg ->
     assert_true "canonical graph rejects corrupt capability scope ref"
       (contains_substring msg "canonical graph capabilityScopeRef mismatch: askName"));

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
  assert_equal "graph def capability scope ref"
    (Kernel.capability_scope_ref [ "Human.ask" ])
    (json_string_field "capabilityScopeRef" (graph_def scoped_graph "wrapped"));
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
     let human_scope_ref = Kernel.capability_scope_ref [ "Human.ask" ] in
     let clock_scope_ref = Kernel.capability_scope_ref [ "Clock.read" ] in
     ignore
       (Canonical_ir.parse_graph
          (replace_once (Canonical_ir.serialize_graph scoped_process)
             ("\"capabilityScope\": [\"Human.ask\"], \"capabilityScopeRefs\": ["
            ^ Ast.quote human_capability_ref ^ "], \"capabilityScopeRef\": "
            ^ Ast.quote human_scope_ref)
             ("\"capabilityScope\": [\"Clock.read\"], \"capabilityScopeRefs\": ["
            ^ Ast.quote clock_capability_ref ^ "], \"capabilityScopeRef\": "
            ^ Ast.quote clock_scope_ref)));
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
  let multi_cap_scoped_process =
    check
      "(capabilities Human.ask Clock.read)\n\
       (def both (Process (capabilities Clock.read Human.ask) String)\n\
       \  (bind (Clock.read) (lambda (now String) (Human.ask \"Name?\"))))"
  in
  assert_equal "scoped Process bind union type visible"
    "(Process (capabilities Clock.read Human.ask) String)"
    (Ast.string_of_typ (checked_def multi_cap_scoped_process "both").Kernel.def.typ);
  expect_check_error
    "(capabilities Human.ask Clock.read)\n\
     (def bad (Process (capabilities Human.ask) String)\n\
     \  (bind (Clock.read) (lambda (now String) (Human.ask \"Name?\"))))";
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
  let multi_cap_scope_ref = Kernel.capability_scope_ref [ "Clock.read"; "Human.ask" ] in
  assert_equal "graph multi capability scope ref" multi_cap_scope_ref
    (json_string_field "capabilityScopeRef" (graph_def multi_cap_graph "both"));
  assert_equal "graph capability scope filter by scope ref" "2"
    (string_of_int
       (List.length (Canonical_ir.graph_capability_scopes_for multi_cap_graph_json multi_cap_scope_ref)));
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
  assert_equal "invariants ledger cap scope ref"
    (Kernel.capability_scope_ref [ "Human.ask" ])
    ledger_invariants.Invariants.cap_scope_ref;
  assert_equal "invariants ledger request tag" "AskHuman"
    ledger_invariants.Invariants.request_tag;
  assert_equal "invariants ledger request signature ref" human_signature_ref
    ledger_invariants.Invariants.request_signature_ref;
  assert_equal "invariants ledger response type" "String"
    ledger_invariants.Invariants.response_type;
  let invariant_request_codec_ref =
    Canonical_ir.host_codec_ref (Ast.TRecord [ ("prompt", Ast.TString) ])
  in
  let invariant_response_codec_ref = Canonical_ir.host_codec_ref Ast.TString in
  assert_equal "invariants ledger host codec version" Canonical_ir.host_codec_version
    ledger_invariants.Invariants.host_codec_version;
  assert_equal "invariants ledger request codec ref" invariant_request_codec_ref
    ledger_invariants.Invariants.request_codec_ref;
  assert_equal "invariants ledger response codec ref" invariant_response_codec_ref
    ledger_invariants.Invariants.response_codec_ref;
  let old_sign_key = Sys.getenv_opt "PROTOSS_LEDGER_SIGN_KEY" in
  let old_verify_key = Sys.getenv_opt "PROTOSS_LEDGER_VERIFY_KEY" in
  let old_key_id = Sys.getenv_opt "PROTOSS_LEDGER_SIGN_KEY_ID" in
  let restore_env name = function
    | Some value -> Unix.putenv name value
    | None -> Unix.putenv name ""
  in
  Fun.protect
    ~finally:(fun () ->
      restore_env "PROTOSS_LEDGER_SIGN_KEY" old_sign_key;
      restore_env "PROTOSS_LEDGER_VERIFY_KEY" old_verify_key;
      restore_env "PROTOSS_LEDGER_SIGN_KEY_ID" old_key_id)
    (fun () ->
      Unix.putenv "PROTOSS_LEDGER_SIGN_KEY" "test-ledger-key";
      Unix.putenv "PROTOSS_LEDGER_VERIFY_KEY" "test-ledger-key";
      Unix.putenv "PROTOSS_LEDGER_SIGN_KEY_ID" "test-key";
      let signed_ledger = temp_dir "signed-ledger" in
      let signed_world = Ledger.init signed_ledger in
      let signed_req = Ast.AskHuman "signed" in
      let _signed_value, signed_suspended, signed_request_id, signed_continuation_id =
        ledger_suspension signed_req [ "Human.ask" ]
      in
      let signed_event, _ =
        Ledger.record_request signed_ledger signed_world signed_req signed_suspended
          signed_request_id signed_continuation_id [ "Human.ask" ]
      in
      let signed_content = Ledger.inspect_event signed_ledger signed_event in
      assert_true "ledger signed event records algorithm"
        (contains_substring signed_content "signature-algorithm=sha256-shared-key");
      assert_true "ledger signed event records key id"
        (contains_substring signed_content "signature-key-id=test-key");
      let bad_signed_content =
        replace_once signed_content "signature=p2:" "signature=p2:bad"
      in
      let bad_signed_event = Hashcons.hash ("event:" ^ bad_signed_content) in
      Store.write_file_atomic (Ledger.event_path signed_ledger bad_signed_event)
        bad_signed_content;
      (try
         ignore (Ledger.inspect_event signed_ledger bad_signed_event);
         fail "ledger signed event with bad signature should be rejected"
       with Failure msg ->
         assert_true "ledger signed event rejects signature mismatch"
           (contains_substring msg "signature mismatch")));
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
  let patch_ok_review = Patch.review_text patch_ok in
  assert_true "patch review header" (contains_substring patch_ok_review "Patch review");
  assert_true "patch review operation" (contains_substring patch_ok_review "op 1: AddDef");
  assert_true "patch review name" (contains_substring patch_ok_review "name: two");
  assert_true "patch review type" (contains_substring patch_ok_review "type: Nat");
  let agent_protocol = Json.parse (Agent_protocol.protocol_json ()) in
  assert_equal "agent protocol format" "protoss-agent-protocol-v1"
    (json_string_field "format" agent_protocol);
  assert_equal "agent protocol pipeline" "AI,PatchCandidate,Validator,Harness,Commit"
    (String.concat "," (json_string_array_field "pipeline" agent_protocol));
  let agent_guard_direct =
    Json.parse (Agent_protocol.guard_write_json (Filename.concat store "program.canon"))
  in
  assert_true "agent guard rejects canonical write"
    (not (json_bool_field "allowed" agent_guard_direct));
  assert_equal "agent guard direct decision" "deny"
    (json_string_field "decision" agent_guard_direct);
  let agent_guard_patch = Json.parse (Agent_protocol.guard_write_json patch_ok) in
  assert_true "agent guard allows patch candidate" (json_bool_field "allowed" agent_guard_patch);
  let agent_no_harness_store = temp_dir "agent-commit-no-harness" in
  let agent_no_harness_before = snapshot agent_no_harness_store in
  (try
     ignore (Agent_protocol.commit_patch_json agent_no_harness_store patch_ok);
     fail "agent commit should require an attached harness"
   with Kernel.Error msg ->
     assert_true "agent commit rejects missing harness"
       (contains_substring msg "HARNESS001"
       && contains_substring msg "requires at least one attached harness"));
  assert_true "agent missing harness commit mutates nothing"
    (snapshot agent_no_harness_store = agent_no_harness_before);
  let agent_commit_harness =
    patch_file "protoss-agent-commit.pth" "harness two_ok = unit two == 2\n"
  in
  let agent_commit_store = temp_dir "agent-commit" in
  let agent_commit =
    Json.parse
      (Agent_protocol.commit_patch_json ~harnesses:[ agent_commit_harness ]
         agent_commit_store patch_ok)
  in
  assert_equal "agent commit format" "protoss-agent-commit-v1"
    (json_string_field "format" agent_commit);
  assert_equal "agent commit protocol" "protoss-agent-protocol-v1"
    (json_string_field "protocol" agent_commit);
  assert_equal "agent commit stage" "Commit" (json_string_field "stage" agent_commit);
  assert_equal "agent commit harness status" "pass"
    (json_string_field "harnessStatus" agent_commit);
  assert_equal "agent commit harness count" "1"
    (string_of_int (json_nat_field "harnessCount" agent_commit));
  let agent_commit_harness_report = List.hd (json_array_field "harnessReports" agent_commit) in
  assert_equal "agent commit embeds harness report" "pass"
    (json_string_field "status" agent_commit_harness_report);
  assert_true "agent commit denies direct canonical writes"
    (not (json_bool_field "directCanonicalWrites" agent_commit));
  assert_true "agent commit patch ref" (contains_substring (json_string_field "patchRef" agent_commit) "p2:");
  let agent_commit_guard = json_field "canonicalWriteGuard" agent_commit in
  assert_true "agent commit embeds denied canonical guard"
    (not (json_bool_field "allowed" agent_commit_guard));
  let agent_committed = Store.load_program agent_commit_store |> Kernel.check_program in
  let agent_committed_two, _ = Runtime.normalize_def agent_committed "two" in
  assert_equal "agent commit applies validated patch" "2"
    (Runtime.value_to_string agent_committed_two);
  let agent_failing_harness_store = temp_dir "agent-commit-failing-harness" in
  let agent_failing_harness_before = snapshot agent_failing_harness_store in
  let agent_failing_harness =
    patch_file "protoss-agent-commit-fail.pth" "harness two_bad = unit two == 3\n"
  in
  (try
     ignore
       (Agent_protocol.commit_patch_json ~harnesses:[ agent_failing_harness ]
          agent_failing_harness_store patch_ok);
     fail "agent commit should reject a failing harness"
   with Kernel.Error msg ->
     assert_true "agent commit rejects failing harness"
       (contains_substring msg "HARNESS001"
       && contains_substring msg "attached harness failed"
       && contains_substring msg "two_bad"));
  assert_true "agent failing harness commit mutates nothing"
    (snapshot agent_failing_harness_store = agent_failing_harness_before);
  let agent_test_synthesis = Json.parse (Agent_protocol.synthesize_tests_json agent_committed) in
  assert_equal "agent test synthesis format" "protoss-agent-test-synthesis-v1"
    (json_string_field "format" agent_test_synthesis);
  let agent_test_suggestions = json_array_field "suggestions" agent_test_synthesis in
  assert_true "agent test synthesis suggests normalization harness"
    (List.exists
       (fun suggestion ->
         String.equal (json_string_field "name" suggestion) "two"
         && String.equal (json_string_field "kind" suggestion) "normalization"
         && contains_substring (json_string_field "harnessTemplate" suggestion)
              "harness two_normalizes = unit two == <expected>")
       agent_test_suggestions);
  let harness_checked =
    check
      "(def two Nat 2)\n\
       (def main Nat 0)\n\
       (def sample Nat 2)\n\
       (def prop (-> Nat Bool) (lambda (n Nat) true))\n\
       (def boolProp Bool true)\n\
       (def invariantOk Bool true)\n\
       (def migrationOk Bool true)\n\
       (def securityOk Bool true)\n\
       (def evalOk Bool true)"
  in
  let harness_source =
    "harness twoExample = example two\n\
     harness twoUnit = unit two == 2\n\
     harness propBool = property boolProp\n\
     harness propGenerated = property prop with sample\n\
     harness sampleGenerator = generator sample\n\
     harness twoBenchmark = benchmark two\n\
     harness invariantOk = invariant invariantOk == true\n\
     harness migrationOk = migration migrationOk == true\n\
     harness scenarioMain = scenario main\n\
     harness securityOk = security securityOk == true\n\
     harness diagnosticPrompt = diagnostic inspect two\n\
     harness aiEvalOk = ai-eval evalOk == true\n"
  in
  let harnesses = Harness.parse harness_source in
  assert_equal "harness parser declarations" "12"
    (string_of_int (List.length harnesses));
  assert_true "harness canonical bytes include format"
    (contains_substring (Harness.canonical_bytes harness_source)
       Harness.canonical_format);
  assert_equal "harness file ref is canonical"
    (Kernel.hash_string (Harness.canonical_bytes harness_source))
    (Harness.file_ref harness_source);
  let harness_report =
    Json.parse (Harness.run_json harness_checked ~source:"inline.pth" harness_source)
  in
  assert_equal "harness report format" Harness.format
    (json_string_field "format" harness_report);
  assert_equal "harness report status" "pass"
    (json_string_field "status" harness_report);
  assert_equal "harness report count" "12"
    (string_of_int (json_nat_field "harnessCount" harness_report));
  let harness_results = json_array_field "harnesses" harness_report in
  let harness_result name =
    match
      List.find_opt
        (fun result -> String.equal (json_string_field "name" result) name)
        harness_results
    with
    | Some result -> result
    | None -> fail ("missing harness result: " ^ name)
  in
  let harness_example = List.hd harness_results in
  assert_equal "harness result id"
    (Harness.harness_id (List.hd harnesses))
    (json_string_field "harnessId" harness_example);
  assert_true "harness example passes" (json_bool_field "passed" harness_example);
  let harness_unit = List.nth harness_results 1 in
  assert_equal "harness unit actual" "2" (json_string_field "actual" harness_unit);
  assert_equal "harness unit expected" "2"
    (json_string_field "expected" harness_unit);
  assert_true "harness property with generator passes"
    (json_bool_field "passed" (harness_result "propGenerated"));
  assert_equal "harness property diagnostic names generator" "generator=sample sample=2"
    (json_string_field "diagnostic" (harness_result "propGenerated"));
  assert_equal "harness generator actual" "2"
    (json_string_field "actual" (harness_result "sampleGenerator"));
  assert_true "harness benchmark passes"
    (json_bool_field "passed" (harness_result "twoBenchmark"));
  assert_true "harness invariant passes"
    (json_bool_field "passed" (harness_result "invariantOk"));
  assert_true "harness migration contract passes"
    (json_bool_field "passed" (harness_result "migrationOk"));
  assert_true "harness world scenario passes"
    (json_bool_field "passed" (harness_result "scenarioMain"));
  assert_true "harness security policy passes"
    (json_bool_field "passed" (harness_result "securityOk"));
  assert_equal "harness diagnostic prompt actual" "inspect two"
    (json_string_field "actual" (harness_result "diagnosticPrompt"));
  assert_true "harness ai evaluation passes"
    (json_bool_field "passed" (harness_result "aiEvalOk"));
  let failing_harness =
    Json.parse
      (Harness.run_json harness_checked ~source:"inline.pth"
         "harness bad = unit two == 3\n")
  in
  assert_equal "harness failing status" "fail"
    (json_string_field "status" failing_harness);
  let failing_result = List.hd (json_array_field "harnesses" failing_harness) in
  assert_true "harness failing unit is marked"
    (not (json_bool_field "passed" failing_result));
  assert_equal "harness failing actual" "2" (json_string_field "actual" failing_result);
  assert_equal "harness failing expected" "3"
    (json_string_field "expected" failing_result);

  let factor_store = temp_dir "factor-identical" in
  let factor_seed =
    patch_file "protoss-factor-seed.json"
      "{ \"ops\": [\n\
       \  { \"op\":\"AddDef\", \"name\":\"a\", \"deps\":[], \"type\":\"Nat\", \"expr\":1 },\n\
       \  { \"op\":\"AddDef\", \"name\":\"b\", \"deps\":[], \"type\":\"Nat\", \"expr\":1 },\n\
       \  { \"op\":\"AddDef\", \"name\":\"c\", \"deps\":[\"b\"], \"type\":\"Nat\", \"expr\":\"b\" },\n\
       \  { \"op\":\"AddDef\", \"name\":\"d\", \"deps\":[], \"type\":\"Nat\", \"expr\":1 }\n\
       ] }"
  in
  ignore (Patch.apply factor_store factor_seed);
  let factor_checked = Store.load_program factor_store |> Kernel.check_program in
  let factor_report = Json.parse (Agent_protocol.factor_identical_json factor_checked) in
  assert_equal "agent factor identical format" "protoss-factor-identical-v1"
    (json_string_field "format" factor_report);
  assert_equal "agent factor identical duplicate groups" "1"
    (string_of_int (json_nat_field "duplicateGroups" factor_report));
  assert_equal "agent factor identical safe delete count" "1"
    (string_of_int (json_nat_field "safeDeleteCount" factor_report));
  assert_equal "agent factor identical blocked count" "1"
    (string_of_int (json_nat_field "blockedCount" factor_report));
  let factor_group = List.hd (json_array_field "groups" factor_report) in
  assert_equal "agent factor identical representative" "a"
    (json_string_field "representative" factor_group);
  assert_equal "agent factor identical names" "a,b,d"
    (String.concat "," (json_string_array_field "names" factor_group));
  let factor_safe = List.hd (json_array_field "safeDeletes" factor_group) in
  assert_equal "agent factor identical safe delete" "d"
    (json_string_field "name" factor_safe);
  let factor_blocked = List.hd (json_array_field "blocked" factor_group) in
  assert_equal "agent factor identical blocked duplicate" "b"
    (json_string_field "name" factor_blocked);
  assert_equal "agent factor identical blocked dependent" "c"
    (String.concat "," (json_string_array_field "dependents" factor_blocked));
  let factor_patch_candidate = json_field "patchCandidate" factor_report in
  let factor_patch_op = List.hd (json_array_field "ops" factor_patch_candidate) in
  assert_equal "agent factor identical patch op" "DeleteDef"
    (json_string_field "op" factor_patch_op);
  assert_equal "agent factor identical patch target" "d"
    (json_string_field "name" factor_patch_op);
  let factor_patch =
    patch_file "protoss-factor-identical.json"
      (Agent_protocol.factor_identical_patch_json factor_checked)
  in
  ignore (Patch.check factor_store factor_patch);
  ignore (Patch.apply factor_store factor_patch);
  let factor_after_names =
    (Store.load_program factor_store).defs
    |> List.map (fun (d : Ast.def) -> d.name)
    |> List.sort String.compare
  in
  assert_equal "agent factor identical keeps dependent duplicate" "a,b,c"
    (String.concat "," factor_after_names);

  let compare_store = temp_dir "candidate-comparison" in
  let compare_left =
    patch_file "protoss-compare-left.json"
      "{ \"op\":\"AddDef\", \"name\":\"one\", \"deps\":[], \"type\":\"Nat\", \"expr\":1 }"
  in
  let compare_right =
    patch_file "protoss-compare-right.json"
      "{ \"op\":\"AddDef\", \"name\":\"bad\", \"deps\":[], \"type\":\"Nat\", \"expr\":true }"
  in
  let compare_before = snapshot compare_store in
  let comparison =
    Json.parse (Agent_protocol.compare_candidates_json compare_store compare_left compare_right)
  in
  assert_equal "agent candidate comparison format"
    "protoss-agent-candidate-comparison-v1"
    (json_string_field "format" comparison);
  assert_equal "agent candidate comparison recommendation" "left"
    (json_string_field "recommendation" comparison);
  assert_equal "agent candidate comparison reason" "left-valid-right-invalid"
    (json_string_field "reason" comparison);
  assert_true "agent candidate comparison does not require harness"
    (not (json_bool_field "requiresHarness" comparison));
  let compared = json_array_field "candidates" comparison in
  let compared_left = List.nth compared 0 in
  let compared_right = List.nth compared 1 in
  assert_true "agent candidate comparison left valid"
    (json_bool_field "valid" compared_left);
  assert_true "agent candidate comparison right invalid"
    (not (json_bool_field "valid" compared_right));
  assert_true "agent candidate comparison invalid diagnostic"
    (contains_substring (json_string_field "diagnostic" compared_right) "definition bad");
  assert_true "agent candidate comparison check is read-only"
    (snapshot compare_store = compare_before);

  let text_diff_store = temp_dir "text-diff" in
  let add_text_diff =
    patch_file "protoss-add-text.diff" "--- a/app.protoss\n+++ b/app.protoss\n@@\n+(def one Nat 1)\n"
  in
  let add_text_patch = Patch.from_text_diff text_diff_store add_text_diff in
  let add_text_patch_json = Json.parse add_text_patch in
  assert_equal "patch text diff AddDef op" "AddDef"
    (json_string_field "op" add_text_patch_json);
  assert_equal "patch text diff AddDef name" "one"
    (json_string_field "name" add_text_patch_json);
  let add_text_patch_file = patch_file "protoss-add-text.json" add_text_patch in
  ignore (Patch.check text_diff_store add_text_patch_file);
  ignore (Patch.apply text_diff_store add_text_patch_file);
  let text_diff_added = Store.load_program text_diff_store |> Kernel.check_program in
  let text_diff_one, _ = Runtime.normalize_def text_diff_added "one" in
  assert_equal "patch text diff AddDef applies" "1"
    (Runtime.value_to_string text_diff_one);
  let replace_text_diff =
    patch_file "protoss-replace-text.diff"
      "--- a/app.protoss\n+++ b/app.protoss\n@@\n-(def one Nat 1)\n+(def one Nat 2)\n"
  in
  let replace_text_patch = Patch.from_text_diff text_diff_store replace_text_diff in
  let replace_text_patch_json = Json.parse replace_text_patch in
  assert_equal "patch text diff ReplaceDef op" "ReplaceDef"
    (json_string_field "op" replace_text_patch_json);
  let replace_text_patch_file = patch_file "protoss-replace-text.json" replace_text_patch in
  ignore (Patch.apply text_diff_store replace_text_patch_file);
  let text_diff_replaced = Store.load_program text_diff_store |> Kernel.check_program in
  let text_diff_replaced_one, _ = Runtime.normalize_def text_diff_replaced "one" in
  assert_equal "patch text diff ReplaceDef applies" "2"
    (Runtime.value_to_string text_diff_replaced_one);
  let ambiguous_text_diff =
    patch_file "protoss-ambiguous-text.diff"
      "--- a/app.protoss\n+++ b/app.protoss\n@@\n+(def a Nat 1)\n+(def b Nat 2)\n"
  in
  (try
     ignore (Patch.from_text_diff text_diff_store ambiguous_text_diff);
     fail "ambiguous text diff should be rejected"
   with Patch.Error msg ->
     assert_true "patch text diff ambiguity names intent"
       (contains_substring msg "ambiguous textual modification"));

  let patch_audit_path store ref =
    Filename.concat (Filename.concat store "patches") (ref ^ ".patch")
  in
  let patch_latest_path store = Filename.concat (Filename.concat store "patches") "latest" in
  let patch_provenance_dir store = Filename.concat store "provenance" in
  let patch_root_path store ref =
    Filename.concat (Filename.concat (patch_provenance_dir store) "roots") (ref ^ ".root")
  in
  let patch_latest_root_path store = Filename.concat (patch_provenance_dir store) "latest-root" in
  let patch_latest_provenance_path store =
    Filename.concat (patch_provenance_dir store) "latest-patch"
  in
  let patch_provenance_path store ref =
    Filename.concat (Filename.concat (patch_provenance_dir store) "patches")
      (ref ^ ".provenance")
  in
  let patch_ok_ref = Patch.apply store patch_ok in
  assert_true "valid patch writes object" (count_objects store > 0);
  let patch_gc_clean = Store.gc store in
  assert_true "store gc keeps live patch object" (patch_gc_clean.Store.unreachable = []);
  let garbage_object = Store.put_object store "test" "garbage" in
  let patch_gc_dirty = Store.gc store in
  assert_true "store gc reports unreachable object"
    (List.exists (String.equal garbage_object) patch_gc_dirty.Store.unreachable);
  let patch_gc_sweep = Store.gc ~delete:true store in
  assert_true "store gc deletes unreachable object"
    (List.exists (String.equal garbage_object) patch_gc_sweep.Store.deleted);
  assert_true "store gc leaves deleted object absent"
    (not (Sys.file_exists (Store.object_path store garbage_object)));
  assert_true "valid patch writes audit ref" (Sys.file_exists (patch_audit_path store patch_ok_ref));
  assert_equal "valid patch latest pointer" patch_ok_ref
    (String.trim (Store.read_file (patch_latest_path store)));
  let patch_ok_audit = Store.read_file (patch_audit_path store patch_ok_ref) in
  assert_true "patch audit records format"
    (contains_substring patch_ok_audit "protoss-patch-audit-v1");
  assert_true "patch audit records source hash" (contains_substring patch_ok_audit "source-hash=p2:");
  assert_true "patch audit records program hash" (contains_substring patch_ok_audit "program-hash=p2:");
  assert_true "patch audit records root ref" (contains_substring patch_ok_audit "root-ref=p2:");
  assert_true "first patch audit has no previous root"
    (contains_substring patch_ok_audit "previous-root=none");
  assert_true "patch audit records operation"
    (contains_substring patch_ok_audit "op=1 kind=AddDef name=two target=two");
  assert_true "first patch audit has no previous ref"
    (contains_substring patch_ok_audit "previous-ref=none");
  let verified_patch_ok = Patch.verify_audit store in
  assert_equal "patch audit verify latest ref" patch_ok_ref verified_patch_ok.Patch.audit_ref;
  assert_equal "patch audit verify ops" "1" (string_of_int verified_patch_ok.Patch.ops);
  assert_true "patch audit verify root ref"
    (contains_substring verified_patch_ok.Patch.root_ref "p2:");
  assert_equal "patch audit verify first previous root" "none"
    (match verified_patch_ok.Patch.previous_root with Some root -> root | None -> "none");
  assert_true "patch audit verify source hash"
    (contains_substring verified_patch_ok.Patch.source_hash "p2:");
  assert_true "patch root state file exists"
    (Sys.file_exists (patch_root_path store verified_patch_ok.Patch.root_ref));
  assert_equal "patch latest root pointer" verified_patch_ok.Patch.root_ref
    (String.trim (Store.read_file (patch_latest_root_path store)));
  let patch_provenance_ref = String.trim (Store.read_file (patch_latest_provenance_path store)) in
  let patch_provenance = Store.read_file (patch_provenance_path store patch_provenance_ref) in
  assert_true "patch provenance links audit and root"
    (contains_substring patch_provenance ("patch-ref=" ^ patch_ok_ref)
    && contains_substring patch_provenance ("root-ref=" ^ verified_patch_ok.Patch.root_ref));
  let patch_world_ledger = Filename.concat (patch_provenance_dir store) "world-ledger" in
  let patch_world =
    String.trim (Store.read_file (Filename.concat (patch_provenance_dir store) "latest-world"))
  in
  let patch_world_replay = Ledger.replay patch_world_ledger patch_world in
  assert_true "patch provenance links to world ledger"
    (contains_substring patch_world_replay "kind=patch-provenance"
    && contains_substring patch_world_replay ("patch-ref=" ^ patch_ok_ref)
    && contains_substring patch_world_replay ("root-ref=" ^ verified_patch_ok.Patch.root_ref)
    && contains_substring patch_world_replay ("patch-provenance-ref=" ^ patch_provenance_ref));
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
  assert_true "second patch audit links previous root"
    (contains_substring chain_audit ("previous-root=" ^ verified_patch_ok.Patch.root_ref));
  assert_equal "patch audit latest pointer moves" chain_ref
    (String.trim (Store.read_file (patch_latest_path store)));
  assert_equal "patch audit chain latest matches store" chain_ref
    (Patch.verify_latest_matches_store store).Patch.audit_ref;
  let stale_root_pointer_store = temp_dir "patch-audit-stale-root" in
  let stale_root_ref = Patch.apply stale_root_pointer_store patch_ok in
  ignore stale_root_ref;
  Store.write_file_atomic (patch_latest_root_path stale_root_pointer_store) "p2:bad-root\n";
  (try
     ignore (Patch.inspect_audit stale_root_pointer_store);
     fail "stale patch root pointer should reject latest audit"
   with Patch.Error msg ->
     assert_true "stale patch root pointer detects mismatch"
       (contains_substring msg "patch latest root mismatch"));
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

  let patch_bad_source =
    patch_file "protoss-bad-source.json"
      "{ \"op\":\"AddDef\", \"name\":\"badSource\", \"deps\":[], \"type\":\"Nat\", \
       \"expr\":{\"source\":\"(succ true)\"} }"
  in
  let bad_source_before = snapshot store in
  (try
     ignore (Patch.apply store patch_bad_source);
     fail "invalid patch source should be rejected"
   with Patch.Error msg ->
     assert_true "invalid patch source points to embedded expression"
       (contains_substring msg
          (patch_bad_source ^ ": patch op #1 AddDef badSource field expr source 1:7"));
     assert_true "invalid patch source keeps kernel definition context"
       (contains_substring msg "definition badSource"));
  assert_true "invalid source patch must not modify store"
    (snapshot store = bad_source_before);
  assert_true "invalid source patch must not modify ledger"
    (count_files ledger = ledger_before);

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

  let patch_adt_store = temp_dir "patch-adt" in
  let patch_adt_seed =
    patch_file "protoss-patch-adt-seed.json"
      "{ \"ops\": [\
       { \"op\":\"AddDef\", \"name\":\"person\", \"deps\":[], \
       \"type\":{\"source\":\"(Record (name String))\"}, \
       \"expr\":{\"source\":\"(record (name \\\"Ada\\\"))\"} },\
       { \"op\":\"AddDef\", \"name\":\"two\", \"deps\":[], \"type\":\"Nat\", \"expr\":2 },\
       { \"op\":\"AddDef\", \"name\":\"three\", \"deps\":[\"two\"], \"type\":\"Nat\", \"expr\":[\"succ\",\"two\"] },\
       { \"op\":\"AddDef\", \"name\":\"four\", \"deps\":[\"three\"], \"type\":\"Nat\", \"expr\":[\"succ\",\"three\"] }\
       ] }"
  in
  ignore (Patch.apply patch_adt_store patch_adt_seed);
  let patch_add_field =
    patch_file "protoss-patch-add-field.json"
      "{ \"op\":\"AddField\", \"name\":\"person\", \"field\":\"age\", \
       \"fieldType\":\"Nat\", \"expr\":41, \"deps\":[] }"
  in
  ignore (Patch.apply patch_adt_store patch_add_field);
  let patch_add_field_checked = Store.load_program patch_adt_store |> Kernel.check_program in
  let person_age, _ = Runtime.normalize_def patch_add_field_checked "person" in
  assert_equal "patch ADT AddField updates record" "{age = 41, name = \"Ada\"}"
    (Runtime.value_to_string person_age);
  let patch_remove_field =
    patch_file "protoss-patch-remove-field.json"
      "{ \"op\":\"RemoveField\", \"name\":\"person\", \"field\":\"age\", \"deps\":[] }"
  in
  ignore (Patch.apply patch_adt_store patch_remove_field);
  let patch_remove_field_checked = Store.load_program patch_adt_store |> Kernel.check_program in
  let person_no_age, _ = Runtime.normalize_def patch_remove_field_checked "person" in
  assert_equal "patch ADT RemoveField updates record" "{name = \"Ada\"}"
    (Runtime.value_to_string person_no_age);
  let patch_inline =
    patch_file "protoss-patch-inline.json"
      "{ \"op\":\"Inline\", \"name\":\"four\", \"inline\":\"three\", \"deps\":[\"two\"] }"
  in
  ignore (Patch.apply patch_adt_store patch_inline);
  let patch_inline_checked = Store.load_program patch_adt_store |> Kernel.check_program in
  let four_inlined, _ = Runtime.normalize_def patch_inline_checked "four" in
  assert_equal "patch ADT Inline preserves value" "4" (Runtime.value_to_string four_inlined);
  let patch_extract =
    patch_file "protoss-patch-extract.json"
      "{ \"op\":\"Extract\", \"name\":\"nextTwo\", \"from\":\"four\", \"deps\":[\"two\"], \
       \"type\":\"Nat\", \"expr\":{\"source\":\"(succ two)\"} }"
  in
  ignore (Patch.apply patch_adt_store patch_extract);
  let patch_extract_checked = Store.load_program patch_adt_store |> Kernel.check_program in
  let extracted, _ = Runtime.normalize_def patch_extract_checked "nextTwo" in
  assert_equal "patch ADT Extract creates def" "3" (Runtime.value_to_string extracted);
  let four_after_extract, _ = Runtime.normalize_def patch_extract_checked "four" in
  assert_equal "patch ADT Extract rewrites source" "4"
    (Runtime.value_to_string four_after_extract);
  let patch_add_capability =
    patch_file "protoss-patch-add-capability.json"
      "{ \"op\":\"AddCapability\", \"name\":\"Human.ask\", \"deps\":[], \
       \"capabilities\":[\"Human.ask\"] }"
  in
  let add_cap_ref = Patch.apply patch_adt_store patch_add_capability in
  let add_cap_audit =
    Store.read_file (Filename.concat (Filename.concat patch_adt_store "patches") (add_cap_ref ^ ".patch"))
  in
  assert_true "patch ADT AddCapability records audit"
    (contains_substring add_cap_audit "kind=AddCapability");
  assert_true "patch ADT AddCapability writes capability set"
    (contains_substring (Store.read_file (Filename.concat patch_adt_store "capabilities"))
       "Human.ask");
  let patch_add_harness =
    patch_file "protoss-patch-add-harness.json"
      "{ \"op\":\"AddHarness\", \"name\":\"twoHarness\", \"deps\":[], \
       \"source\":\"harness twoHarness = unit two == 2\\n\" }"
  in
  let add_harness_ref = Patch.apply patch_adt_store patch_add_harness in
  let add_harness_audit =
    Store.read_file (Filename.concat (Filename.concat patch_adt_store "patches") (add_harness_ref ^ ".patch"))
  in
  assert_true "patch ADT AddHarness records audit"
    (contains_substring add_harness_audit "kind=AddHarness");
  let migrate_store = temp_dir "patch-adt-migrate" in
  let migrate_patch =
    patch_file "protoss-patch-migrate-type.json"
      "{ \"op\":\"MigrateType\", \"name\":\"migrate_v1_v2\", \"deps\":[], \
       \"type\":{\"source\":\"(-> Nat Nat)\"}, \
       \"expr\":{\"source\":\"(lambda (old Nat) old)\"} }"
  in
  ignore (Patch.apply migrate_store migrate_patch);
  let migrate_checked = Store.load_program migrate_store |> Kernel.check_program in
  let migrate_value, _ = Runtime.eval_entry migrate_checked "migrate_v1_v2" in
  let migrated = Runtime.apply migrate_checked migrate_value (Runtime.VNat 5) in
  assert_equal "patch ADT MigrateType adds migration" "5"
    (Runtime.value_to_string migrated);

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

  let benchmark_store = temp_dir "benchmark" in
  let benchmark_content =
    Benchmark.report_content ~kind:"build" ~subject:"demo" ~build_id:"p2:demo"
      ~seconds:1.25 ~stats:"parsed=1\nreused=0\n"
  in
  let benchmark_ref = Benchmark.write_report benchmark_store benchmark_content in
  assert_equal "content-addressed benchmark ref" (Benchmark.report_ref benchmark_content)
    benchmark_ref;
  assert_equal "content-addressed benchmark content" benchmark_content
    (Store.read_file (Benchmark.report_path benchmark_store benchmark_ref));

  let dedupe_store = temp_dir "dedupe" in
  let h1 = Store.put_object dedupe_store "test" "same" in
  let h2 = Store.put_object dedupe_store "test" "same" in
  assert_equal "store dedupe hash" h1 h2;
  assert_true "store dedupe object count" (List.length (Store.list_objects dedupe_store) = 1);
  let old_global_store = Sys.getenv_opt "PROTOSS_GLOBAL_STORE" in
  let restore_global_store () =
    match old_global_store with
    | Some value -> Unix.putenv "PROTOSS_GLOBAL_STORE" value
    | None -> (
        match Sys.getenv_opt "HOME" with
        | Some home when home <> "" ->
            Unix.putenv "PROTOSS_GLOBAL_STORE" (Filename.concat home ".protoss/global-store")
        | _ -> Unix.putenv "PROTOSS_GLOBAL_STORE" "")
  in
  let global_store = temp_dir "global-store" in
  Fun.protect
    ~finally:restore_global_store
    (fun () ->
      Unix.putenv "PROTOSS_GLOBAL_STORE" global_store;
      let global_project_a = temp_dir "global-project-a" in
      let global_project_b = temp_dir "global-project-b" in
      let global_hash_a = Store.put_object global_project_a "test" "cross-project" in
      let global_hash_b = Store.put_object global_project_b "test" "cross-project" in
      assert_equal "global store cross-project hash" global_hash_a global_hash_b;
      let global_object = Store.object_path global_store global_hash_a in
      let object_a = Store.object_path global_project_a global_hash_a in
      let object_b = Store.object_path global_project_b global_hash_b in
      assert_true "global store writes shared object" (Sys.file_exists global_object);
      let global_stat = Unix.stat global_object in
      let stat_a = Unix.stat object_a in
      let stat_b = Unix.stat object_b in
      assert_equal "global store project a hardlink device"
        (string_of_int global_stat.Unix.st_dev)
        (string_of_int stat_a.Unix.st_dev);
      assert_equal "global store project a hardlink inode"
        (string_of_int global_stat.Unix.st_ino)
        (string_of_int stat_a.Unix.st_ino);
      assert_equal "global store project b hardlink inode"
        (string_of_int global_stat.Unix.st_ino)
        (string_of_int stat_b.Unix.st_ino));

  if integration_part "workspace" then (
  trace_test "integration:start";
  (* The workspace slices exercise store/package/audit *mechanics*, which are
     size-independent: a handful of declarations keeps every assertion
     meaningful (the recursive Json variant stays for the alias round-trip
     and package-interface tests) while builds and audits stop re-checking
     the full prelude on every workspace copy. Full-prelude workspace
     coverage (build + audit) lives in the web part. *)
  let mini_stdlib_root = temp_dir "mini-stdlib" in
  ensure_dir mini_stdlib_root;
  let mini_stdlib_path = Filename.concat mini_stdlib_root "prelude.protoss" in
  write_file mini_stdlib_path
    "; Minimal stdlib for workspace-mechanics tests.\n\
     (type Pair (A B) (Record (first A) (second B)))\n\
     (type Assoc (K V) (List (Pair K V)))\n\
     (variant Json\n\
     \  (JArray (List Json))\n\
     \  (JBool Bool)\n\
     \  (JNat Nat)\n\
     \  (JNull Unit)\n\
     \  (JObject (Assoc String Json))\n\
     \  (JString String))\n\
     (def Nat.add (-> Nat (-> Nat Nat))\n\
     \  (lambda (a Nat) (lambda (b Nat) ((prim.Nat.add a) b))))\n\
     (def Nat.mul (-> Nat (-> Nat Nat))\n\
     \  (lambda (a Nat) (lambda (b Nat) ((prim.Nat.mul a) b))))\n\
     (def List.mapNat (-> (List Nat) (-> (-> Nat Nat) (List Nat)))\n\
     \  (lambda (xs (List Nat))\n\
     \    (lambda (f (-> Nat Nat))\n\
     \      (foldList xs (Nil Nat)\n\
     \        (lambda (x Nat) (lambda (acc (List Nat)) (Cons Nat (f x) acc)))))))\n";
  let make_workspace name base_value bound =
    let root = temp_dir name in
    ensure_dir root;
    ensure_dir (Filename.concat root "src");
    write_file (Filename.concat root "protoss.toml")
      ("name = \"" ^ name ^ "\"\nversion = \"0.4.0\"\nentrypoints = [\"src/app.protoss\"]\nstdlib = \""
     ^ mini_stdlib_path
      ^ "\"\nsource_dirs = [\"src\"]\nstore_dir = \".protoss/store\"\ncache_dir = \".protoss/cache\"\ncapabilities = [\"Human.ask\"]\npolicies = [\"NoNetworkExceptDeclared\"]\n");
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
  let run_shell label cmd =
    match Sys.command cmd with
    | 0 -> ()
    | code -> fail (label ^ " failed with exit " ^ string_of_int code ^ ": " ^ cmd)
  in
  (* Rebuilds the workspace-a chain (build + lock + package + locked-build
     record) for slices that run without the project slice. Every step is
     deterministic over content-addressed state, so the resulting tree is
     byte-identical to what the project slice leaves behind — and re-running
     it over an existing tree is cheap (cached prepare, identical writes
     skipped). The interface*.json scratch files the project slice drops in
     the project root are intentionally absent: nothing downstream reads
     them. *)
  let rebuild_workspace_a () =
    let ws_a = make_workspace "workspace-a" 2 "x" in
    let manifest_a = Workspace.parse_manifest ws_a in
    let build_a = Workspace.build manifest_a in
    let lock_path, lock_hash = Workspace.write_lock manifest_a in
    let package_a = Workspace.write_package manifest_a in
    ignore (Workspace.build_locked manifest_a);
    (ws_a, manifest_a, build_a, lock_path, lock_hash, package_a)
  in
  if workspace_part "project" then (
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
  let policy_project_root = temp_dir "project-policy-capability" in
  ignore (Workspace.init policy_project_root);
  let policy_manifest_path = Filename.concat policy_project_root "protoss.toml" in
  write_file policy_manifest_path
    "name = \"policy-capability\"\n\
     version = \"0.1.0\"\n\
     entrypoints = [\"src/main.protoss\"]\n\
     stdlib = \"none\"\n\
     source_dirs = [\"src\"]\n\
     store_dir = \".protoss/store\"\n\
     cache_dir = \".protoss/cache\"\n\
     capabilities = []\n\
     policies = [\"NoNetworkExceptDeclared\"]\n";
  write_file (Filename.concat policy_project_root "src/main.protoss")
    "(capabilities Http.get)\n\
     (defcap fetch (capabilities Http.get) (Process String)\n\
     \  (Http.get \"https://example.invalid/status\"))\n";
  let policy_manifest = Workspace.parse_manifest policy_project_root in
  (try
     ignore (Workspace.build policy_manifest);
     fail "NoNetworkExceptDeclared should require manifest network capabilities"
   with Workspace.Error msg ->
     assert_true "NoNetworkExceptDeclared reports missing manifest capability"
       (contains_substring msg
          "policy NoNetworkExceptDeclared requires manifest capability declaration: Http.get"));
  write_file policy_manifest_path
    (replace_once (Store.read_file policy_manifest_path) "capabilities = []"
       "capabilities = [\"Http.get\"]");
  let policy_manifest_declared = Workspace.parse_manifest policy_project_root in
  ignore (Workspace.build policy_manifest_declared);
  let git_map_ws = make_workspace "workspace-git-map" 4 "g" in
  let git_q = Filename.quote git_map_ws in
  run_shell "git init" ("git -C " ^ git_q ^ " init -q");
  run_shell "git config email"
    ("git -C " ^ git_q ^ " config user.email protoss@example.invalid");
  run_shell "git config name" ("git -C " ^ git_q ^ " config user.name Protoss");
  run_shell "git add" ("git -C " ^ git_q ^ " add protoss.toml src");
  run_shell "git commit" ("git -C " ^ git_q ^ " commit -q -m initial");
  let git_map_manifest = Workspace.parse_manifest git_map_ws in
  let git_mapping = Workspace.write_git_mapping git_map_manifest in
  let git_map_content = Store.read_file git_mapping.Workspace.git_map_path in
  assert_true "git map records commit" (String.length git_mapping.git_commit = 40);
  assert_true "git map records branch" (String.length git_mapping.git_branch > 0);
  assert_true "git map records universe branch"
    (contains_substring git_mapping.git_universe_branch "p2:");
  assert_equal "git map records current universe root" git_mapping.git_universe_root
    (String.trim
       (Store.read_file (Workspace.universe_root_path (Workspace.store_root git_map_manifest))));
  assert_true "git map artifact links commit and root"
    (contains_substring git_map_content ("commit=" ^ git_mapping.git_commit)
    && contains_substring git_map_content ("universe-root=" ^ git_mapping.git_universe_root)
    && contains_substring git_map_content
         ("universe-branch=" ^ git_mapping.git_universe_branch));
  let git_blame_ledger = Workspace.write_git_blame_ledger git_map_manifest "src/app.protoss" in
  let git_blame_content = Store.read_file git_blame_ledger.Workspace.git_blame_path in
  assert_true "git blame ledger records file"
    (contains_substring git_blame_content "file=src/app.protoss");
  assert_true "git blame ledger records universe root"
    (contains_substring git_blame_content ("universe-root=" ^ git_mapping.git_universe_root));
  assert_true "git blame ledger records line commits"
    (List.length git_blame_ledger.Workspace.git_blame_entries > 0
    && contains_substring git_blame_content ("commit=" ^ git_mapping.git_commit));
  let layout_out = Filename.concat git_map_ws "portable-layout" in
  let layout = Workspace.export_layout ~out:layout_out git_map_manifest in
  assert_true "layout export writes protoss.lock"
    (Sys.file_exists layout.Workspace.layout_lock_path
    && contains_substring (Store.read_file layout.layout_lock_path) "protoss-lock-v1");
  assert_true "layout export writes pt views"
    (List.exists
       (fun path -> Filename.basename path = "app.pt" && Sys.file_exists path)
       layout.layout_view_paths);
  assert_equal "layout export ptb cache round trips"
    (Kernel.hash_program (Canonical_binary.checked_of_binary (Store.read_file layout.layout_cache_path)))
    (Workspace.build git_map_manifest).Workspace.build_id;
  assert_true "layout export writes harness layout"
    (Sys.file_exists layout.layout_harness_path
    && contains_substring (Store.read_file layout.layout_harness_path) "harnesses=0");
  let ws_a = make_workspace "workspace-a" 2 "x" in
  let harness_dir = Filename.concat ws_a "harness" in
  ensure_dir harness_dir;
  let package_harness_path = Filename.concat harness_dir "smoke.pth" in
  let package_harness_content = "harness smoke = example appMain\n" in
  write_file package_harness_path package_harness_content;
  let package_harness_ref = Harness.file_ref package_harness_content in
  let package_harness_node_ref =
    Harness.harness_id (List.hd (Harness.parse package_harness_content))
  in
  let manifest_a = Workspace.parse_manifest ws_a in
  trace_test "integration:workspace-a";
  Workspace.check_project manifest_a;
  let build_a = Workspace.build manifest_a in
  trace_test "integration:workspace-a:built";
  assert_true "project build parsed sources" (build_a.Workspace.stats.Workspace.parsed > 0);
  assert_true "project build normalized defs" (build_a.Workspace.stats.Workspace.normalized > 0);
  assert_true "project universe root is content addressed"
    (contains_substring build_a.Workspace.universe_root "p2:");
  List.iter
    (fun (target, kind) ->
      let backend_build, artifact = Workspace.build_compiler_backend manifest_a target in
      assert_equal ("backend " ^ target ^ " reuses universe root") build_a.universe_root
        backend_build.universe_root;
      assert_equal ("backend " ^ target ^ " artifact ref")
        (Workspace.compiled_artifact_ref ~universe_root:build_a.universe_root ~target
           ~optimization_policy:(Workspace.compiler_backend_optimization_policy target))
        artifact.Workspace.compiled_artifact_ref;
      let manifest_path =
        Filename.concat
          (Filename.dirname artifact.compiled_artifact_path)
          (Workspace.sanitize_id artifact.compiled_artifact_ref ^ "." ^ target ^ ".backend")
      in
      let manifest_content = Store.read_file manifest_path in
      assert_true ("backend " ^ target ^ " manifest")
        (contains_substring manifest_content "protoss-compiler-backend-v1"
        && contains_substring manifest_content ("kind=" ^ kind)
        && contains_substring manifest_content ("target=" ^ target)
        && contains_substring manifest_content ("universe-root=" ^ build_a.universe_root)
        && contains_substring manifest_content
             ("compiled-artifact-ref=" ^ artifact.compiled_artifact_ref)))
    [
      ("bytecode", "protoss-vm-bytecode-manifest");
      ("wasm", "webassembly-module-manifest");
      ("llvm", "llvm-native-manifest");
      ("javascript", "standalone-javascript-manifest");
      ("sql-dataflow", "sql-dataflow-manifest");
      ("gpu-kernel", "gpu-kernel-manifest");
    ];
  let universe_root_file = Workspace.universe_root_path build_a.store in
  let universe_root_content_file = Workspace.universe_root_content_path build_a.store in
  assert_equal "project universe root pointer" build_a.Workspace.universe_root
    (String.trim (Store.read_file universe_root_file));
  let universe_root_content = Store.read_file universe_root_content_file in
  assert_equal "project universe root hashes content" build_a.Workspace.universe_root
    (Kernel.hash_string (String.trim universe_root_content));
  assert_true "project universe root records defs"
    (contains_substring universe_root_content "(defs ");
  assert_true "project universe root records types"
    (contains_substring universe_root_content "(types ");
  assert_true "project universe root records harness files"
    (contains_substring universe_root_content "(harnesses (harness "
    && contains_substring universe_root_content "harness/smoke.pth"
    && contains_substring universe_root_content package_harness_ref);
  let package_harness_graph_ref = Harness.graph_ref (Workspace.harness_sources manifest_a) in
  assert_true "project universe root records harness graph"
    (contains_substring universe_root_content ("(harness-graph " ^ package_harness_graph_ref));
  let package_harness_graph_content =
    Store.read_file (Workspace.harness_graph_path build_a.store)
  in
  let package_harness_graph = Json.parse package_harness_graph_content in
  assert_equal "project harness graph format" Harness.graph_format
    (json_string_field "format" package_harness_graph);
  assert_equal "project harness graph hash" package_harness_graph_ref
    (json_string_field "harnessGraphHash" package_harness_graph);
  assert_equal "project harness graph count" "1"
    (string_of_int (json_nat_field "harnessCount" package_harness_graph));
  let package_harness_graph_item = List.hd (json_array_field "harnesses" package_harness_graph) in
  assert_equal "project harness graph source" "harness/smoke.pth"
    (json_string_field "source" package_harness_graph_item);
  assert_equal "project harness graph id" package_harness_node_ref
    (json_string_field "harnessId" package_harness_graph_item);
  assert_true "project universe root records policies"
    (contains_substring universe_root_content "(policies \"NoNetworkExceptDeclared\")");
  assert_true "project store list" (String.contains (Workspace.list_store build_a.store) 'a');
  assert_true "project store get" (String.length (Workspace.get_store build_a.store "appMain") > 0);
  assert_equal "project store deps" "Nat.add,base,total"
    (String.concat "," (Workspace.read_deps build_a.store "appMain"));
  assert_true "project store roots" (String.length (Workspace.roots_store build_a.store) > 0);
  assert_equal "project audit" "Audit OK\n" (Workspace.audit manifest_a);
  trace_test "integration:workspace-a:audit";
  let harness_graph_corrupt_root = temp_dir "workspace-harness-graph-corrupt" in
  copy_tree ws_a harness_graph_corrupt_root;
  let harness_graph_corrupt_manifest = Workspace.parse_manifest harness_graph_corrupt_root in
  write_file
    (Workspace.harness_graph_path (Workspace.store_root harness_graph_corrupt_manifest))
    "{\"format\":\"bad\"}\n";
  (try
     ignore (Workspace.audit harness_graph_corrupt_manifest);
     fail "audit should reject corrupt harness graph"
   with Workspace.Error msg ->
     assert_true "project audit rejects corrupt harness graph"
       (contains_substring msg "harness graph"));
  let universe_root_corrupt_root = temp_dir "workspace-universe-root-corrupt" in
  copy_tree ws_a universe_root_corrupt_root;
  let universe_root_corrupt_manifest = Workspace.parse_manifest universe_root_corrupt_root in
  let universe_root_corrupt_store = Workspace.store_root universe_root_corrupt_manifest in
  write_file (Workspace.universe_root_path universe_root_corrupt_store) "p2:bad\n";
  (try
     ignore (Workspace.audit universe_root_corrupt_manifest);
     fail "audit should reject stale universe root"
   with Workspace.Error msg ->
     assert_true "project audit rejects stale universe root"
       (contains_substring msg "universe root mismatch"));
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
  trace_test "integration:workspace-a:patch-audit";
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
  trace_test "integration:workspace-a:store-graph";
  let store_graph_checked = Workspace.checked_store_graph build_a.store store_graph_hash in
  assert_equal "project checked store graph hash" (Kernel.hash_program build_a.Workspace.checked)
    (Kernel.hash_program store_graph_checked);
  let store_graph_value, _ = Runtime.eval_entry store_graph_checked "appMain" in
  assert_equal "project checked store graph eval" "44" (Runtime.value_to_string store_graph_value);
  let graph_put_store = temp_dir "workspace-graph-put-store" in
  assert_equal "project put store graph hash" store_graph_hash
    (Workspace.put_store_graph graph_put_store (Store.read_file store_graph_path));
  assert_equal "project put store graph content" (Store.read_file store_graph_path)
    (Workspace.graph_store graph_put_store store_graph_hash);
  let graph_put_invalid_store = temp_dir "workspace-graph-put-invalid-store" in
  (try
     ignore (Workspace.put_store_graph graph_put_invalid_store (Store.read_file store_graph_path ^ "\n"));
     fail "graph-put should reject non-exact canonical JSON"
   with Workspace.Error msg ->
     assert_true "graph-put reports non-exact canonical JSON"
       (contains_substring msg "stored canonical graph is not exact canonical JSON"));
  assert_true "graph-put invalid does not create store" (not (Sys.file_exists graph_put_invalid_store));
  let store_graph_invariants = Invariants.check_store_graph build_a.store store_graph_hash in
  assert_equal "project store graph invariants hash" (Kernel.hash_program build_a.Workspace.checked)
    store_graph_invariants.Invariants.program_hash;
  let store_graph_process_invariants =
    Invariants.check_store_graph_process build_a.store store_graph_hash "askName" "String:Ada"
  in
  assert_equal "project store graph process invariant result" "Done \"Ada\""
    store_graph_process_invariants.Invariants.result;
  let store_graph_ledger_invariants =
    Invariants.check_store_graph_ledger_process
      ~ledger:(temp_dir "workspace-store-graph-ledger-invariants")
      build_a.store store_graph_hash "askName" "String:Ada"
  in
  assert_equal "project store graph ledger invariant result" "Done \"Ada\""
    store_graph_ledger_invariants.Invariants.result;
  trace_test "integration:workspace-a:graph-invariants";
  let store_graph_dot = Workspace.store_graph_dot build_a.store store_graph_hash in
  assert_true "project store graph dot header"
    (contains_substring store_graph_dot "digraph protoss");
  assert_true "project store graph dot deps"
    (contains_substring store_graph_dot "\"base\" -> \"appMain\"");
  let store_graph_stats = Workspace.store_graph_stats build_a.store store_graph_hash in
  assert_equal "project store graph stats hash" store_graph_hash
    store_graph_stats.Canonical_ir.graph_hash;
  assert_true "project store graph stats defs" (store_graph_stats.Canonical_ir.defs > 1);
  assert_true "project store graph stats edges" (store_graph_stats.Canonical_ir.edges > 0);
  let store_graph_app_main_ref = json_string_field "termRef" (graph_def store_graph "appMain") in
  let store_graph_node = Workspace.store_graph_node build_a.store store_graph_hash store_graph_app_main_ref in
  assert_equal "project store graph node kind" "Term" store_graph_node.Canonical_ir.node_kind;
  assert_true "project store graph node describes"
    (contains_substring (Canonical_ir.describe_graph_node store_graph_node) "Graph node");
  let store_graph_def = Workspace.store_graph_definition build_a.store store_graph_hash "appMain" in
  let store_graph_roots = Workspace.store_graph_definitions build_a.store store_graph_hash in
  assert_true "project store graph roots include appMain"
    (contains_substring (Canonical_ir.describe_graph_definitions store_graph_roots) "name=appMain");
  let store_graph_deps = Workspace.store_graph_dependencies_for build_a.store store_graph_hash "appMain" in
  assert_true "project store graph deps include base"
    (contains_substring (Canonical_ir.describe_graph_dependencies store_graph_deps)
       "depends_on=base");
  assert_true "project store graph deps include total"
    (contains_substring (Canonical_ir.describe_graph_dependencies store_graph_deps)
       "depends_on=total");
  let store_graph_caps = Workspace.store_graph_capabilities build_a.store store_graph_hash in
  assert_true "project store graph capabilities include Human.ask"
    (contains_substring (Canonical_ir.describe_graph_capabilities store_graph_caps)
       "capability=Human.ask");
  let store_graph_human_cap =
    Workspace.store_graph_capability build_a.store store_graph_hash "Human.ask"
  in
  assert_equal "project store graph capability name" "Human.ask"
    store_graph_human_cap.Canonical_ir.graph_cap_name;
  assert_true "project store graph capability request"
    (contains_substring
       (Canonical_ir.describe_graph_capabilities [ store_graph_human_cap ])
       "request=AskHuman");
  let store_graph_capability_scopes =
    Workspace.store_graph_capability_scopes_for build_a.store store_graph_hash "Human.ask"
  in
  assert_true "project store graph capability scope includes askName"
    (contains_substring
       (Canonical_ir.describe_graph_capability_scopes store_graph_capability_scopes)
       "def=askName");
  assert_true "project store graph capability scope includes Human.ask"
    (contains_substring
       (Canonical_ir.describe_graph_capability_scopes store_graph_capability_scopes)
       "capability=Human.ask");
  assert_equal "project store graph capability scope by ref" "1"
    (string_of_int
       (List.length
          (Workspace.store_graph_capability_scopes_for build_a.store store_graph_hash
             store_graph_human_cap.Canonical_ir.graph_cap_ref)));
  trace_test "integration:workspace-a:graph-queries";
  let store_graph_host_contract_json =
    Workspace.store_graph_host_contract build_a.store store_graph_hash
  in
  let store_host_contract_path = Workspace.host_contract_path build_a.store in
  assert_true "project store host contract file" (Sys.file_exists store_host_contract_path);
  assert_equal "project store host contract file content" store_graph_host_contract_json
    (Store.read_file store_host_contract_path);
  let store_graph_host_contract = Json.parse store_graph_host_contract_json in
  assert_equal "project store graph host contract format" "protoss-host-contract-v1"
    (json_string_field "format" store_graph_host_contract);
  assert_equal "project store graph host contract graph hash" store_graph_hash
    (json_string_field "graphHash" store_graph_host_contract);
  let store_host_contract_hash = json_string_field "contractHash" store_graph_host_contract in
  assert_equal "project store host contract current ref" store_host_contract_hash
    (String.trim (Store.read_file (Workspace.host_contract_current_path build_a.store)));
  assert_equal "project store host contract object" store_graph_host_contract_json
    (Store.read_file (Workspace.host_contract_object_path build_a.store store_host_contract_hash));
  assert_true "project store host contracts list current hash"
    (contains_substring (Workspace.host_contracts_store build_a.store) store_host_contract_hash);
  assert_equal "project store host contract current read" store_graph_host_contract_json
    (Workspace.host_contract_store build_a.store "current");
  assert_equal "project store host contract hash read" store_graph_host_contract_json
    (Workspace.host_contract_store build_a.store store_host_contract_hash);
  assert_equal "project store graph host contract deterministic" store_graph_host_contract_json
    (Workspace.store_graph_host_contract build_a.store store_graph_hash);
  assert_equal "project store graph host contract check" "Host contract OK\n"
    (Workspace.check_store_graph_host_contract build_a.store store_graph_hash
       store_graph_host_contract_json);
  assert_true "project store graph host contract includes capability"
    (contains_substring store_graph_host_contract_json "Human.ask");
  assert_true "project store graph host contract includes askName scope"
    (contains_substring store_graph_host_contract_json "\"def\": \"askName\"");
  (try
     ignore
       (Workspace.check_store_graph_host_contract build_a.store store_graph_hash
          (replace_once store_graph_host_contract_json "Human.ask" "Clock.read"));
     fail "drifted store graph host contract should be rejected"
  with Kernel.Error msg ->
     assert_true "drifted store graph host contract mismatch"
       (contains_substring msg "host contract mismatch"));
  trace_test "integration:workspace-a:host-contract";
  assert_equal "project store graph def name" "appMain" store_graph_def.Canonical_ir.graph_def_name;
  assert_equal "project store graph def term ref" store_graph_app_main_ref
    store_graph_def.Canonical_ir.graph_def_term_ref;
  assert_true "project store graph def deps"
    (List.exists (String.equal "base") store_graph_def.Canonical_ir.graph_def_deps);
  assert_true "project store graph def describes"
    (contains_substring (Canonical_ir.describe_graph_definition store_graph_def) "Graph def");
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
  trace_test "integration:workspace-a:pre-lock";
  let lock_path, lock_hash = Workspace.write_lock manifest_a in
  assert_true "project lock writes file" (Sys.file_exists lock_path);
  let lock_before = Store.read_file lock_path in
  assert_true "project lock records version" (contains_substring lock_before "protoss-lock-v1");
  assert_true "project lock records hash algorithm"
    (contains_substring lock_before "(hash-algorithm \"sha256\")");
  assert_true "project lock records hash prefix"
    (contains_substring lock_before "(hash-prefix \"p2:\")");
  assert_true "project lock records program hash" (contains_substring lock_before build_a.build_id);
  assert_equal "project lock records universe root" build_a.universe_root
    (sexp_atom_field "universe-root" lock_before);
  assert_equal "project lock records canonical graph hash" (json_string_field "graphHash" store_graph)
    (sexp_atom_field "program-graph-hash" lock_before);
  assert_equal "project lock records host contract hash" store_host_contract_hash
    (sexp_atom_field "host-contract-hash" lock_before);
  assert_true "project lock records source units" (contains_substring lock_before "(source-hash p2:");
  assert_true "project lock records policies"
    (contains_substring lock_before "(policies \"NoNetworkExceptDeclared\")");
  assert_equal "project lock check hash" lock_hash (Workspace.check_lock manifest_a);
  write_file lock_path (replace_once lock_before build_a.universe_root "p2:bad-universe-root");
  (try
     ignore (Workspace.check_lock manifest_a);
     fail "project lock check should reject stale universe root"
   with Workspace.Error _ -> ());
  write_file lock_path (replace_once lock_before build_a.universe_root "p2:bad-universe-root");
  (try
     ignore (Workspace.build_locked manifest_a);
     fail "locked build should reject stale universe root"
   with Workspace.Error _ -> ());
  write_file lock_path lock_before;
  let lock_path_again, lock_hash_again = Workspace.write_lock manifest_a in
  assert_equal "project lock deterministic path" lock_path lock_path_again;
  assert_equal "project lock deterministic hash" lock_hash lock_hash_again;
  assert_equal "project lock deterministic content" lock_before (Store.read_file lock_path_again);
  trace_test "integration:workspace-a:lock";
  trace_test "integration:package-a";
  let package_a = Workspace.write_package manifest_a in
  trace_test "integration:package-a:written";
  assert_true "project package writes file" (Sys.file_exists package_a.Workspace.package_path);
  assert_equal "project package records build" build_a.build_id package_a.build_id;
  assert_equal "project package records universe result" build_a.universe_root
    package_a.universe_root;
  assert_equal "project package records lock hash" lock_hash package_a.lock_hash;
  assert_equal "project package records universe root" build_a.universe_root
    (sexp_atom_field "universe-root" (Store.read_file package_a.package_path));
  assert_equal "project package records canonical graph hash" (json_string_field "graphHash" store_graph)
    (sexp_atom_field "program-graph-hash" (Store.read_file package_a.package_path));
  assert_equal "project package records host contract hash" store_host_contract_hash
    (sexp_atom_field "host-contract-hash" (Store.read_file package_a.package_path));
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
  assert_true "project package records policies"
    (contains_substring package_content "(policies \"NoNetworkExceptDeclared\")");
  assert_true "project package records harnesses"
    (contains_substring package_content "(harnesses (harness "
    && contains_substring package_content "harness/smoke.pth"
    && contains_substring package_content package_harness_ref);
  assert_true "project package records public interface"
    (contains_substring package_content "(interface ");
  let interface_hash = sexp_atom_field "interface-hash" package_content in
  assert_true "project package public interface records capability scope"
    (contains_substring package_content "(capability-scope \"Human.ask\")");
  assert_true "project package public interface records canonical types"
    (contains_substring package_content "(type-canonical ");
  assert_true "project package records recursive Json type"
    (contains_substring package_content "(name \"Json\")");
  let package_universe_root_outdated =
    replace_once package_content build_a.universe_root "p2:bad-universe-root"
  in
  let package_universe_root_outdated_ref = Kernel.hash_string package_universe_root_outdated in
  let package_universe_root_outdated_path =
    Filename.concat (Workspace.packages_dir manifest_a)
      (Workspace.sanitize_id package_universe_root_outdated_ref ^ ".package")
  in
  write_file package_universe_root_outdated_path package_universe_root_outdated;
  write_file (Workspace.package_current_path manifest_a)
    (package_universe_root_outdated_ref ^ "\n");
  (try
     ignore (Workspace.check_package manifest_a);
     fail "package check should reject stale universe root"
   with Workspace.Error _ -> ());
  write_file (Workspace.package_current_path manifest_a) (package_a.package_ref ^ "\n");
  let package_checked = Workspace.check_package manifest_a in
  trace_test "integration:package-a:checked";
  assert_equal "project package check ref" package_a.package_ref package_checked.package_ref;
  assert_equal "project package check lock" lock_hash package_checked.lock_hash;
  assert_equal "project package check interface ref" package_a.interface_ref
    package_checked.interface_ref;
  assert_equal "project package check interface path" package_a.interface_path
    package_checked.interface_path;
  assert_equal "project package check interface contract" package_a.interface_contract_hash
    package_checked.interface_contract_hash;
  write_file package_harness_path
    (package_harness_content ^ "harness changed = example appMain\n");
  (try
     ignore (Workspace.check_package manifest_a);
     fail "package check should reject harness drift"
   with Workspace.Error msg ->
     assert_true "project package check rejects harness drift"
       (contains_substring msg "lockfile out of date"
       || contains_substring msg "package descriptor out of date"
       || contains_substring msg "harnesses mismatch"));
  write_file package_harness_path package_harness_content;
  let package_interface_text = Workspace.package_interface_text manifest_a in
  trace_test "integration:package-a:interface-text";
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
  trace_test "integration:package-a:interface-json";
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
  trace_test "integration:package-a:interface-contract";
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
  trace_test "integration:package-a:invariants";
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
  trace_test "integration:package-a:audit";
  let package_again = Workspace.write_package manifest_a in
  trace_test "integration:package-a:written-again";
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
  trace_test "integration:package-a:locked";
  assert_equal "project package locked ref" package_a.package_ref package_locked.package_ref;
  assert_equal "project package locked interface ref" package_a.interface_ref
    package_locked.interface_ref;
  let package_copy_root = temp_dir "workspace-package-copy" in
  copy_tree ws_a package_copy_root;
  let package_copy_manifest = Workspace.parse_manifest package_copy_root in
  let package_copy = Workspace.write_package package_copy_manifest in
  trace_test "integration:package-a:copy";
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
  trace_test "integration:package-a:interface-constraint";
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
  trace_test "integration:package-a:invalid-constraint");
  if workspace_part "consumer" then (
  trace_test "integration:consumer-package";
  let ws_a, manifest_a, _build_a, lock_path, lock_hash, package_a = rebuild_workspace_a () in
  let lock_before = Store.read_file lock_path in
  let package_content = Store.read_file package_a.Workspace.package_path in
  let interface_hash = sexp_atom_field "interface-hash" package_content in
  let package_interface_contract_hash =
    json_string_field "contractHash" (Json.parse (Workspace.package_interface_json manifest_a))
  in
  let consumer_ws = make_workspace "workspace-consumer" 5 "z" in
  let consumer_manifest_path = Filename.concat consumer_ws "protoss.toml" in
  let consumer_manifest_base = Store.read_file consumer_manifest_path in
  write_file consumer_manifest_path
    (consumer_manifest_base ^ "package_imports = [\"workspace-a=" ^ ws_a
   ^ "\"]\npackage_interfaces = [\"workspace-a=" ^ interface_hash
  ^ "\"]\npackage_contracts = [\"workspace-a=" ^ package_interface_contract_hash ^ "\"]\n");
  let consumer_manifest = Workspace.parse_manifest consumer_ws in
  let consumer_package = Workspace.write_package consumer_manifest in
  trace_test "integration:consumer-package:written";
  let consumer_package_content = Store.read_file consumer_package.package_path in
  assert_true "package dependency records package ref"
    (contains_substring consumer_package_content ("workspace-a=" ^ package_a.package_ref));
  assert_true "package dependency records interface hash"
    (contains_substring consumer_package_content ("workspace-a=" ^ interface_hash));
  assert_true "package dependency records contract hash"
    (contains_substring consumer_package_content ("workspace-a=" ^ package_interface_contract_hash));
  let consumer_interface_text = Workspace.package_interface_text consumer_manifest in
  trace_test "integration:consumer-package:interface-text";
  assert_true "package interface prints imported package"
    (contains_substring consumer_interface_text ("import workspace-a package=" ^ package_a.package_ref));
  assert_true "package interface prints imported interface"
    (contains_substring consumer_interface_text ("interface=" ^ interface_hash));
  assert_true "package interface prints imported contract"
    (contains_substring consumer_interface_text ("contract=" ^ package_interface_contract_hash));
  let consumer_interface_obj = Json.parse (Workspace.package_interface_json consumer_manifest) in
  trace_test "integration:consumer-package:interface-json";
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
  trace_test "integration:consumer-package:checked";
  let alias_consumer_ws = make_workspace "workspace-consumer-alias" 7 "a" in
  let alias_consumer_manifest_path = Filename.concat alias_consumer_ws "protoss.toml" in
  let alias_consumer_manifest_base = Store.read_file alias_consumer_manifest_path in
  write_file alias_consumer_manifest_path
    (alias_consumer_manifest_base ^ "package_aliases = [\"workspace-a@0.4.0=" ^ ws_a
   ^ "\"]\npackage_imports = [\"workspace-a=workspace-a@0.4.0\"]\npackage_interfaces = [\"workspace-a="
   ^ interface_hash ^ "\"]\npackage_contracts = [\"workspace-a=" ^ package_interface_contract_hash
   ^ "\"]\n");
  let alias_consumer_manifest = Workspace.parse_manifest alias_consumer_ws in
  let alias_consumer_package = Workspace.write_package alias_consumer_manifest in
  let alias_consumer_package_content = Store.read_file alias_consumer_package.package_path in
  assert_true "package alias records semver alias"
    (contains_substring alias_consumer_package_content ("workspace-a@0.4.0=" ^ ws_a));
  assert_true "package alias resolves imported package ref"
    (contains_substring alias_consumer_package_content ("workspace-a=" ^ package_a.package_ref));
  let alias_bad_ws = make_workspace "workspace-consumer-alias-bad" 7 "b" in
  let alias_bad_manifest_path = Filename.concat alias_bad_ws "protoss.toml" in
  write_file alias_bad_manifest_path
    (Store.read_file alias_bad_manifest_path ^ "package_aliases = [\"workspace-a@9.9.9="
   ^ ws_a ^ "\"]\npackage_imports = [\"workspace-a=workspace-a@9.9.9\"]\n");
  let alias_bad_manifest = Workspace.parse_manifest alias_bad_ws in
  (try
     ignore (Workspace.write_package alias_bad_manifest);
     fail "package semver alias mismatch should reject"
   with Workspace.Error msg ->
     assert_true "package semver alias mismatch reports version"
       (contains_substring msg
          "package alias version mismatch for workspace-a: expected 9.9.9, got 0.4.0"));
  let policy_alias_ws = make_workspace "workspace-consumer-policy-alias" 7 "c" in
  let policy_alias_manifest_path = Filename.concat policy_alias_ws "protoss.toml" in
  let policy_alias_manifest_base = Store.read_file policy_alias_manifest_path in
  write_file policy_alias_manifest_path
    (policy_alias_manifest_base ^ "package_policy_aliases = [\"workspace-a@NoNetworkExceptDeclared="
   ^ ws_a
   ^ "\"]\npackage_imports = [\"workspace-a=workspace-a@NoNetworkExceptDeclared\"]\npackage_interfaces = [\"workspace-a="
   ^ interface_hash ^ "\"]\npackage_contracts = [\"workspace-a=" ^ package_interface_contract_hash
   ^ "\"]\n");
  let policy_alias_manifest = Workspace.parse_manifest policy_alias_ws in
  let policy_alias_package = Workspace.write_package policy_alias_manifest in
  let policy_alias_package_content = Store.read_file policy_alias_package.package_path in
  assert_true "package policy alias records alias"
    (contains_substring policy_alias_package_content
       ("workspace-a@NoNetworkExceptDeclared=" ^ ws_a));
  assert_true "package policy alias resolves imported package ref"
    (contains_substring policy_alias_package_content ("workspace-a=" ^ package_a.package_ref));
  let policy_alias_bad_ws = make_workspace "workspace-consumer-policy-alias-bad" 7 "d" in
  let policy_alias_bad_manifest_path = Filename.concat policy_alias_bad_ws "protoss.toml" in
  write_file policy_alias_bad_manifest_path
    (Store.read_file policy_alias_bad_manifest_path
   ^ "package_policy_aliases = [\"workspace-a@RequiresGpu=" ^ ws_a
   ^ "\"]\npackage_imports = [\"workspace-a=workspace-a@RequiresGpu\"]\n");
  let policy_alias_bad_manifest = Workspace.parse_manifest policy_alias_bad_ws in
  (try
     ignore (Workspace.write_package policy_alias_bad_manifest);
     fail "package policy alias mismatch should reject"
   with Workspace.Error msg ->
     assert_true "package policy alias mismatch reports policy"
       (contains_substring msg
          "package policy alias mismatch for workspace-a: missing policy RequiresGpu"));
  let registry_consumer_ws = make_workspace "workspace-consumer-registry" 7 "e" in
  let registry_file = Filename.concat registry_consumer_ws "packages.registry" in
  write_file registry_file ("workspace-a@0.4.0=" ^ ws_a ^ "\n");
  let registry_manifest_path = Filename.concat registry_consumer_ws "protoss.toml" in
  let registry_manifest_base = Store.read_file registry_manifest_path in
  write_file registry_manifest_path
    (registry_manifest_base ^ "package_registry_local = \"packages.registry\"\npackage_imports = [\"workspace-a=workspace-a@0.4.0\"]\npackage_interfaces = [\"workspace-a="
   ^ interface_hash ^ "\"]\npackage_contracts = [\"workspace-a=" ^ package_interface_contract_hash
   ^ "\"]\n");
  let registry_manifest = Workspace.parse_manifest registry_consumer_ws in
  let registry_package = Workspace.write_package registry_manifest in
  let registry_package_content = Store.read_file registry_package.package_path in
  assert_true "package registry records local registry"
    (contains_substring registry_package_content "package-registry"
    && contains_substring registry_package_content "local=packages.registry#p2:");
  assert_true "package registry resolves imported package ref"
    (contains_substring registry_package_content ("workspace-a=" ^ package_a.package_ref));
  assert_equal "package registry check ref" registry_package.package_ref
    (Workspace.check_package registry_manifest).Workspace.package_ref;
  let registry_dot_before_drift = snapshot (Filename.concat registry_consumer_ws ".protoss") in
  write_file registry_file ("workspace-a@0.4.0=/missing/workspace-a\n");
  (try
     ignore (Workspace.check_package registry_manifest);
     fail "package registry drift should reject"
   with Workspace.Error _ -> ());
  assert_true "package registry drift leaves package store untouched"
    (registry_dot_before_drift = snapshot (Filename.concat registry_consumer_ws ".protoss"));
  let global_registry_dir = temp_dir "workspace-global-package-registry" in
  ensure_dir global_registry_dir;
  let global_registry_file = Filename.concat global_registry_dir "packages.registry" in
  write_file global_registry_file ("workspace-a@NoNetworkExceptDeclared=" ^ ws_a ^ "\n");
  let global_registry_ws = make_workspace "workspace-consumer-global-registry" 7 "f" in
  let global_registry_manifest_path = Filename.concat global_registry_ws "protoss.toml" in
  let global_registry_manifest_base = Store.read_file global_registry_manifest_path in
  write_file global_registry_manifest_path
    (global_registry_manifest_base ^ "package_registry_global = \"" ^ global_registry_file
   ^ "\"\npackage_imports = [\"workspace-a=workspace-a@NoNetworkExceptDeclared\"]\npackage_interfaces = [\"workspace-a="
   ^ interface_hash ^ "\"]\npackage_contracts = [\"workspace-a=" ^ package_interface_contract_hash
   ^ "\"]\n");
  let global_registry_manifest = Workspace.parse_manifest global_registry_ws in
  let global_registry_package = Workspace.write_package global_registry_manifest in
  let global_registry_content = Store.read_file global_registry_package.package_path in
  assert_true "package registry records global registry"
    (contains_substring global_registry_content "package-registry"
    && contains_substring global_registry_content "global=/");
  assert_true "package registry resolves global imported package ref"
    (contains_substring global_registry_content ("workspace-a=" ^ package_a.package_ref));
  trace_test "integration:consumer-package:alias";
  let consumer_package_invariants = Invariants.check_package consumer_ws in
  trace_test "integration:consumer-package:invariants";
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
  trace_test "integration:consumer-package:drift-invariants";
  (try
     ignore (Workspace.check_package consumer_manifest);
     fail "package check should reject imported package source drift"
   with Workspace.Error _ -> ());
  trace_test "integration:consumer-package:drift-check";
  (try
     ignore (Workspace.write_package consumer_manifest);
     fail "package write should reject imported package source drift"
   with Workspace.Error _ -> ());
  trace_test "integration:consumer-package:drift-write";
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
  trace_test "integration:consumer-package:bad-interface";
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
  trace_test "integration:consumer-package:bad-contract";
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
  trace_test "integration:consumer-package:mismatch";
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
  trace_test "integration:consumer-package:capability-package";
  let capability_interface_hash =
    sexp_atom_field "interface-hash" (Store.read_file capability_package.package_path)
  in
  assert_true "package interface hash includes public capability scope"
    (not (String.equal interface_hash capability_interface_hash));
  let negative_capability_ws = make_workspace "workspace-negative-capability-import" 8 "r" in
  let negative_capability_manifest_path =
    Filename.concat negative_capability_ws "protoss.toml"
  in
  write_file negative_capability_manifest_path
    (Store.read_file negative_capability_manifest_path
    ^ "package_imports = [\"workspace-a=" ^ capability_interface_ws
    ^ "\"]\npackage_interfaces = [\"workspace-a=" ^ capability_interface_hash ^ "\"]\n");
  let negative_capability_manifest = Workspace.parse_manifest negative_capability_ws in
  (try
     ignore (Workspace.write_package negative_capability_manifest);
     fail "package import requiring undeclared capability should reject"
   with Workspace.Error msg ->
     assert_true "package import reports undeclared capability"
       (contains_substring msg "requires undeclared capabilities: Clock.read"));
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
  trace_test "integration:consumer-package:source-drift";
  assert_equal "project lock drift keeps lockfile" lock_before (Store.read_file lock_path);
  assert_true "project lock drift leaves .protoss untouched"
    (dot_before_drift = snapshot (Filename.concat ws_a ".protoss"));
  write_file math_path math_before;
  let locked_build = Workspace.build_locked manifest_a in
  trace_test "integration:consumer-package:locked-build";
  let locked_build_meta =
    Filename.concat
      (Filename.concat locked_build.Workspace.store "builds")
      (Workspace.sanitize_id locked_build.Workspace.build_id ^ ".build")
  in
  assert_true "locked build records lock hash"
    (contains_substring (Store.read_file locked_build_meta) ("lock_hash=" ^ lock_hash)));
  if workspace_part "corruption" then (
  let ws_a, manifest_a, build_a, _lock_path, _lock_hash, _package_a = rebuild_workspace_a () in
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
  trace_test "integration:consumer-package:scope-corrupt";
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
  trace_test "integration:consumer-package:package-corrupt";
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
  trace_test "integration:consumer-package:interface-corrupt";
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
  trace_test "integration:consumer-package:package-outdated";
  let package_host_contract_outdated_root = temp_dir "workspace-package-host-contract-outdated" in
  copy_tree ws_a package_host_contract_outdated_root;
  let package_host_contract_outdated_manifest =
    Workspace.parse_manifest package_host_contract_outdated_root
  in
  let package_host_contract_outdated_ref =
    String.trim
      (Store.read_file
         (Workspace.package_current_path package_host_contract_outdated_manifest))
  in
  let package_host_contract_outdated_content =
    Store.read_file
      (Filename.concat (Workspace.packages_dir package_host_contract_outdated_manifest)
         (Workspace.sanitize_id package_host_contract_outdated_ref ^ ".package"))
    |> fun content -> replace_once content "(host-contract-hash p2:" "(host-contract-hash p2:bad"
  in
  let package_host_contract_outdated_ref =
    Kernel.hash_string package_host_contract_outdated_content
  in
  let package_host_contract_outdated_path =
    Filename.concat (Workspace.packages_dir package_host_contract_outdated_manifest)
      (Workspace.sanitize_id package_host_contract_outdated_ref ^ ".package")
  in
  write_file package_host_contract_outdated_path package_host_contract_outdated_content;
  write_file
    (Workspace.package_current_path package_host_contract_outdated_manifest)
    (package_host_contract_outdated_ref ^ "\n");
  (try
     ignore (Workspace.check_package package_host_contract_outdated_manifest);
     fail "package check should reject out-of-date host contract hash"
   with Workspace.Error _ -> ());
  trace_test "integration:consumer-package:host-contract-outdated";
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
  trace_test "integration:consumer-package:graph-corrupt";
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
  trace_test "integration:consumer-package:graph-object-corrupt";
  let host_contract_corrupt_root = temp_dir "workspace-host-contract-corrupt" in
  copy_tree ws_a host_contract_corrupt_root;
  let host_contract_corrupt_manifest = Workspace.parse_manifest host_contract_corrupt_root in
  let host_contract_corrupt_store = Workspace.store_root host_contract_corrupt_manifest in
  Store.write_file_atomic
    (Workspace.host_contract_path host_contract_corrupt_store)
    (replace_once
       (Store.read_file (Workspace.host_contract_path host_contract_corrupt_store))
       "Human.ask" "Clock.read");
  (try
     ignore (Workspace.audit host_contract_corrupt_manifest);
     fail "audit should reject corrupt host contract"
   with Workspace.Error msg ->
     assert_true "audit reports host contract mismatch"
       (contains_substring msg "host contract mismatch: host.contract.json"));
  trace_test "integration:consumer-package:host-contract-corrupt";
  let host_contract_ref_corrupt_root = temp_dir "workspace-host-contract-ref-corrupt" in
  copy_tree ws_a host_contract_ref_corrupt_root;
  let host_contract_ref_corrupt_manifest =
    Workspace.parse_manifest host_contract_ref_corrupt_root
  in
  let host_contract_ref_corrupt_store = Workspace.store_root host_contract_ref_corrupt_manifest in
  Store.write_file_atomic (Workspace.host_contract_current_path host_contract_ref_corrupt_store)
    "p2:bad\n";
  (try
     ignore (Workspace.audit host_contract_ref_corrupt_manifest);
     fail "audit should reject corrupt host contract ref"
   with Workspace.Error msg ->
     assert_true "audit reports host contract ref mismatch"
       (contains_substring msg "host contract ref mismatch"));
  trace_test "integration:consumer-package:host-contract-ref-corrupt";
  let host_contract_object_corrupt_root = temp_dir "workspace-host-contract-object-corrupt" in
  copy_tree ws_a host_contract_object_corrupt_root;
  let host_contract_object_corrupt_manifest =
    Workspace.parse_manifest host_contract_object_corrupt_root
  in
  let host_contract_object_corrupt_store =
    Workspace.store_root host_contract_object_corrupt_manifest
  in
  let host_contract_object_hash =
    String.trim
      (Store.read_file (Workspace.host_contract_current_path host_contract_object_corrupt_store))
  in
  Store.write_file_atomic
    (Workspace.host_contract_object_path host_contract_object_corrupt_store
       host_contract_object_hash)
    (Store.read_file
       (Workspace.host_contract_object_path host_contract_object_corrupt_store
          host_contract_object_hash)
    ^ "corrupt\n");
  (try
     ignore (Workspace.audit host_contract_object_corrupt_manifest);
     fail "audit should reject corrupt content-addressed host contract"
   with Workspace.Error msg ->
     assert_true "audit reports content-addressed host contract mismatch"
       (contains_substring msg "content-addressed host contract mismatch"));
  let extra_graph_bad_root = temp_dir "workspace-extra-graph-bad" in
  copy_tree ws_a extra_graph_bad_root;
  let extra_graph_bad_manifest = Workspace.parse_manifest extra_graph_bad_root in
  let extra_graph_bad_store = Workspace.store_root extra_graph_bad_manifest in
  Store.write_file_atomic
    (Store.graph_path extra_graph_bad_store "p2:bad")
    (Store.read_file (Filename.concat extra_graph_bad_store "program.graph.json"));
  (try
     ignore (Workspace.audit extra_graph_bad_manifest);
     fail "audit should reject extra graph stored under wrong hash"
   with Workspace.Error msg ->
     assert_true "audit reports invalid extra graph object"
       (contains_substring msg "invalid content-addressed canonical graph p2:bad"
       && contains_substring msg "stored canonical graph hash mismatch"));

  trace_test "integration:modules-diff";
  let module_ws = temp_dir "workspace-modules" in
  ensure_dir module_ws;
  ensure_dir (Filename.concat module_ws "src");
  write_file (Filename.concat module_ws "protoss.toml")
    ("name = \"workspace-modules\"\nversion = \"0.5.0\"\nentrypoints = [\"src/app.protoss\"]\nstdlib = \""
   ^ mini_stdlib_path
    ^ "\"\nsource_dirs = [\"src\"]\nstore_dir = \".protoss/store\"\ncache_dir = \".protoss/cache\"\ncapabilities = []\n");
  let module_math_source =
    "(module Demo.Math)\n(export Number double)\n(type Number Nat)\n(def hidden Number 2)\n\
     (def double (-> Number Number) (lambda (x Number) ((Nat.mul x) hidden)))\n"
  in
  let module_math_hash = Kernel.hash_string ("source:" ^ module_math_source) in
  write_file (Filename.concat module_ws "src/math.protoss") module_math_source;
  write_file (Filename.concat module_ws "src/app.protoss")
    ("(import \"math.protoss#" ^ module_math_hash
   ^ "\")\n(def result Demo.Math.Number (Demo.Math.double 4))\n");
  let module_manifest = Workspace.parse_manifest module_ws in
  let module_build = Workspace.build module_manifest in
  let module_checked = module_build.Workspace.checked in
  let module_value, _ = Runtime.normalize_def module_checked "result" in
  assert_equal "workspace module export" "8" (Runtime.value_to_string module_value);
  write_file (Filename.concat module_ws "src/app.protoss")
    "(import \"math.protoss#p2:bad\")\n(def result Demo.Math.Number (Demo.Math.double 4))\n";
  (try
     ignore (Workspace.build module_manifest);
     fail "workspace import hash mismatch should reject"
   with Workspace.Error msg ->
     assert_true "workspace import hash mismatch error"
       (contains_substring msg "import hash mismatch"));
  write_file (Filename.concat module_ws "src/app.protoss")
    ("(import \"math.protoss#" ^ module_math_hash
   ^ "\")\n(def result Demo.Math.Number (Demo.Math.double 4))\n");
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
  let semantic_diff_text = Workspace.diff_to_text semantic_diff in
  assert_true "semantic diff text names change"
    (String.contains semantic_diff_text 'b');
  assert_true "semantic diff text has structural paths"
    (contains_substring semantic_diff_text "/definitions/");
  assert_true "semantic diff text has affected definitions"
    (contains_substring semantic_diff_text "affected.definitions=[");
  let semantic_diff_json_text = Workspace.diff_to_json semantic_diff in
  let semantic_diff_json = Json.parse semantic_diff_json_text in
  let affected = json_field "affected" semantic_diff_json in
  assert_true "semantic diff json affected definition"
    (List.exists (String.equal "base") (json_string_array_field "definitions" affected));
  assert_equal "semantic diff json affected harnesses empty" "0"
    (string_of_int (List.length (json_array_field "harnesses" affected)));
  let first_change =
    match json_array_field "changes" semantic_diff_json with
    | [] -> fail "semantic diff json should include changes"
    | change :: _ -> change
  in
  assert_true "semantic diff json path"
    (contains_substring (json_string_field "path" first_change) "/definitions/");
  assert_true "semantic diff json changed paths"
    (json_string_array_field "changedPaths" first_change <> []);
  let change_affected = json_field "affected" first_change in
  assert_true "semantic diff json change affected definitions"
    (json_string_array_field "definitions" change_affected <> []);

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
  let patched_host_contract =
    Canonical_ir.graph_host_contract (Store.read_file (Filename.concat patched_store "program.graph.json"))
  in
  assert_equal "patch updates host contract" patched_host_contract
    (Store.read_file (Workspace.host_contract_path patched_store));
  let patched_host_contract_hash =
    json_string_field "contractHash" (Json.parse patched_host_contract)
  in
  assert_equal "patch updates host contract ref" patched_host_contract_hash
    (String.trim (Store.read_file (Workspace.host_contract_current_path patched_store)));
  assert_equal "patch updates host contract object" patched_host_contract
    (Store.read_file (Workspace.host_contract_object_path patched_store patched_host_contract_hash));

  let corrupt_root = temp_dir "workspace-corrupt" in
  copy_tree ws_a corrupt_root;
  let corrupt_manifest = Workspace.parse_manifest corrupt_root in
  let corrupt_store = Workspace.store_root corrupt_manifest in
  write_file (Store.canonical_path corrupt_store "base") "corrupt\n";
  (try
     ignore (Workspace.audit corrupt_manifest);
     fail "audit should reject corrupt store"
   with Workspace.Error _ | Kernel.Error _ -> ())));

  if integration_part "web" then (
  let stdlib_path = find_up (Sys.getcwd ()) "stdlib/prelude.protoss" in
  let todo_src = find_up (Sys.getcwd ()) "examples/web/todo_app" in
  let site_vitrine_src = find_up (Sys.getcwd ()) "examples/web/site_vitrine" in
  (* Each web slice materializes its own todo project: the example sources
     plus a pinned manifest, built deterministically, so slices stay
     independent processes (same pattern as rebuild_workspace_a). Only src/
     is copied — CLI runs leave a git-ignored .protoss store in the example
     tree, and inheriting it would make the suite depend on (and trust)
     unversioned local state. *)
  let make_todo () =
    let todo = temp_dir "web-todo" in
    ensure_dir todo;
    copy_tree (Filename.concat todo_src "src") (Filename.concat todo "src");
    write_file (Filename.concat todo "protoss.toml")
      ("name = \"todo-web-alpha-test\"\nversion = \"0.1.0\"\nentrypoints = [\"src/app.protoss\"]\nstdlib = \""
      ^ stdlib_path
      ^ "\"\nsource_dirs = [\"src\"]\nstore_dir = \".protoss/store\"\ncache_dir = \".protoss/cache\"\ncapabilities = [\"Local.storage\"]\n");
    todo
  in
  if web_part "app" then (
  let todo = make_todo () in
  trace_test "integration:web";
  let contract = Web.app_check todo in
  assert_equal "web app model"
    "(Record (draft String) (items (List String)) (next Nat))"
    (Ast.string_of_typ contract.Web.model_ty);
  assert_equal "web app process architecture" "process" contract.Web.architecture;
  let human_site_contract = Web.app_check site_vitrine_src in
  assert_equal "human web app example checks" "process" human_site_contract.Web.architecture;
  assert_equal "human web app example model" "(Record (lead String) (status String))"
    (Ast.string_of_typ human_site_contract.Web.model_ty);
  let cmd_app = temp_dir "web-cmd-app" in
  ensure_dir (Filename.concat cmd_app "src");
  write_file (Filename.concat cmd_app "src/app.protoss")
    "(type Model Nat)\n\
     (variant Msg (Click Unit))\n\
     (def init (Tuple Model (Cmd (capabilities) Msg)) (tuple 0 unit))\n\
     (def update (-> Msg (-> Model (Tuple Model (Cmd (capabilities) Msg))))\n\
     \  (lambda (msg Msg) (lambda (model Model) (tuple (succ model) unit))))\n\
     (def view (-> Model (View Msg)) (lambda (model Model) (text \"cmd app\")))\n";
  write_file (Filename.concat cmd_app "protoss.toml")
    ("name = \"cmd-web-alpha-test\"\nversion = \"0.1.0\"\nentrypoints = [\"src/app.protoss\"]\nstdlib = \""
    ^ stdlib_path
    ^ "\"\nsource_dirs = [\"src\"]\nstore_dir = \".protoss/store\"\ncache_dir = \".protoss/cache\"\ncapabilities = []\n");
  let cmd_contract = Web.app_check cmd_app in
  assert_equal "web app cmd architecture" "cmd" cmd_contract.Web.architecture;
  assert_equal "web app cmd model" "Nat" (Ast.string_of_typ cmd_contract.Web.model_ty);
  let cmd_dist = temp_dir "web-cmd-dist" in
  ignore (Web.build ~out:cmd_dist cmd_app);
  let cmd_app_json = Json.parse (Store.read_file (Filename.concat cmd_dist "protoss-app.json")) in
  assert_equal "web app cmd architecture embedded" "cmd"
    (json_string_field "architecture" cmd_app_json);
  assert_equal "web app cmd initial model" "0"
    (string_of_int (json_nat_field "value" (json_field "initialModel" cmd_app_json)));
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
      "protoss-host-contract.json";
      "protoss-compiled-artifact.txt";
      "protoss-capabilities.json";
      "protoss-world.json";
    ];
  let web_canon_graph =
    Json.parse (Store.read_file (Filename.concat web_dist_a "protoss-canon-graph.json"))
  in
  let web_app_json = Json.parse (Store.read_file (Filename.concat web_dist_a "protoss-app.json")) in
  let embedded_program = json_field "program" web_app_json in
  let web_compiled_artifact = Store.read_file (Filename.concat web_dist_a "protoss-compiled-artifact.txt") in
  assert_equal "web compiled artifact ref is derived"
    web_a.Web.compiled_artifact.Workspace.compiled_artifact_ref
    (Workspace.compiled_artifact_ref ~universe_root:web_a.Web.build.Workspace.universe_root
       ~target:"web" ~optimization_policy:"web-default-v1");
  assert_equal "web app embeds compiled artifact"
    web_a.Web.compiled_artifact.Workspace.compiled_artifact_ref
    (json_string_field "compiledArtifact" web_app_json);
  assert_true "web compiled artifact records derivation"
    (contains_substring web_compiled_artifact
       ("universe-root=" ^ web_a.Web.build.Workspace.universe_root)
    && contains_substring web_compiled_artifact "target=web"
    && contains_substring web_compiled_artifact "optimization-policy=web-default-v1"
    && Sys.file_exists web_a.Web.compiled_artifact.Workspace.compiled_artifact_path);
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
  let web_host_contract = json_field "hostContract" web_app_json in
  let web_host_contract_artifact =
    Json.parse (Store.read_file (Filename.concat web_dist_a "protoss-host-contract.json"))
  in
  assert_true "web host contract artifact matches app" (web_host_contract = web_host_contract_artifact);
  assert_equal "web app embeds host contract format" "protoss-host-contract-v1"
    (json_string_field "format" web_host_contract);
  assert_equal "web app host contract graph hash"
    (json_string_field "graphHash" web_canon_graph)
    (json_string_field "graphHash" web_host_contract);
  assert_equal "web app host codec version" Canonical_ir.host_codec_version
    (json_string_field "hostCodecVersion" web_host_contract);
  let web_runtime_js = Store.read_file (Filename.concat web_dist_a "protoss-runtime.js") in
  assert_true "web runtime interprets canonical graph"
    (contains_substring web_runtime_js "evalProgram(app.program)");
  assert_true "web runtime exposes suspended requests"
    (contains_substring web_runtime_js "protoss:request");
  assert_true "web runtime exposes request signature refs"
    (contains_substring web_runtime_js "requestSignatureRef");
  assert_true "web runtime exposes host codec refs"
    (contains_substring web_runtime_js "requestCodecRef"
    && contains_substring web_runtime_js "responseCodecRef");
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
  let web_host_capabilities = json_array_field "capabilities" web_host_contract in
  let local_storage_host = List.hd web_host_capabilities in
  assert_equal "web host capability ref" local_storage_ref
    (json_string_field "capabilityRef" local_storage_host);
  let save_host_request =
    match
      json_array_field "requests" local_storage_host
      |> List.find_opt (fun req -> String.equal (json_string_field "tag" req) "SaveLocal")
    with
    | Some req -> req
    | None -> fail "missing web SaveLocal host request"
  in
  assert_equal "web host SaveLocal signature ref"
    (Kernel.req_signature_ref (Ast.SaveLocal ("", "")))
    (json_string_field "requestSignatureRef" save_host_request);
  assert_equal "web host SaveLocal request codec"
    (Canonical_ir.host_codec_ref (Kernel.req_payload_type (Ast.SaveLocal ("", ""))))
    (json_string_field "codecRef" (json_field "requestCodec" save_host_request));
  assert_equal "web host SaveLocal response codec"
    (Canonical_ir.host_codec_ref (Kernel.req_result_type (Ast.SaveLocal ("", ""))))
    (json_string_field "codecRef" (json_field "responseCodec" save_host_request));
  let local_storage_requests = json_array_field "requests" local_storage_descriptor in
  assert_true "web capability request signatures" (List.length local_storage_requests = 2);
  assert_true "web capability request refs"
    (List.for_all
       (fun request -> contains_substring (json_string_field "ref" request) "p2:")
       local_storage_requests);
  trace_test "integration:web:artifacts";
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
      "protoss-host-contract.json";
      "protoss-compiled-artifact.txt";
      "protoss-capabilities.json";
      "protoss-world.json";
    ];
  let web_second = Workspace.build (Workspace.parse_manifest todo) in
  assert_equal "web incremental parsed" "0" (string_of_int web_second.Workspace.stats.Workspace.parsed);
  assert_true "web incremental reused" (web_second.Workspace.stats.Workspace.reused > 0);
  assert_true "web inspect" (String.contains (Web.inspect todo) 'm');
  trace_test "integration:web:deterministic";

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
   with Kernel.Error _ -> ()));

  if web_part "patches" then (
  let todo = make_todo () in
  let web_a = Web.build ~out:(temp_dir "web-dist-patches") todo in
  let patch_dir = find_up (Sys.getcwd ()) "patches/web" in
  trace_test "integration:web-patches-ledger";
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
  let migration_todo = make_todo () in
  let migration_base = Web.build ~out:(temp_dir "web-migration-base-dist") migration_todo in
  let migration_old_store = migration_base.Web.build.Workspace.store in
  let migration_new_store = temp_dir "web-migration-new-store" in
  copy_tree migration_old_store migration_new_store;
  ignore (Patch.apply migration_new_store (Filename.concat patch_dir "model_with_migration.json"));
  let migration_report =
    Json.parse (Agent_protocol.generate_migration_json migration_old_store migration_new_store)
  in
  let migration_ops = json_array_field "ops" (json_field "patchCandidate" migration_report) in
  let migration_op = match migration_ops with op :: _ -> op | [] -> fail "expected migration op" in
  assert_true "agent migration generation proposes model migration"
    (json_bool_field "required" migration_report
    && contains_substring (json_string_field "expr" migration_report) "(filter \"\")"
    && String.equal (json_string_field "name" migration_op) "migrate_v1_v2");
  (try
     ignore (Patch.check web_store (Filename.concat patch_dir "model_without_migration.json"));
     fail "model patch without migration should be rejected"
   with Patch.Error _ -> ());
  ignore (Patch.check web_store (Filename.concat patch_dir "model_with_migration.json"));
  trace_test "integration:web:patches");

  if web_part "audit" then (
  let todo = make_todo () in
  ignore (Web.build ~out:(temp_dir "web-dist-audit") todo);
  (* Audit over the full prelude lives here: the workspace slices run on the
     mini stdlib, so this is the one place a complete-store audit still sees
     the real 572-def program. It runs on a freshly built store — Patch.apply
     records an audit chain for the patched program, so the patches slice
     would legitimately read as drift. *)
  assert_equal "web audit full prelude store" "Audit OK\n"
    (Workspace.audit (Workspace.parse_manifest todo));
  trace_test "integration:web:audit-ok";
  let corrupt_todo = temp_dir "web-corrupt" in
  copy_tree todo corrupt_todo;
  let corrupt_manifest = Workspace.parse_manifest corrupt_todo in
  ignore (Web.build ~out:(temp_dir "web-corrupt-dist") corrupt_todo);
  write_file (Store.canonical_path (Workspace.store_root corrupt_manifest) "buttonLabel") "corrupt\n";
  (try
     ignore (Workspace.audit corrupt_manifest);
     fail "web audit should reject corrupt store"
   with Workspace.Error _ | Kernel.Error _ -> ());
  trace_test "integration:web:corruption";

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
  let simulation_event, simulation_world =
    Ledger.simulate ledger_web "feature" world1 "try alternate local-storage value"
  in
  let simulation_content = Ledger.inspect_event ledger_web simulation_event in
  assert_true "ledger simulation event records fork"
    (contains_substring simulation_content "kind=simulation"
    && contains_substring simulation_content ("base-world=" ^ world1)
    && contains_substring simulation_content "branch=feature");
  assert_true "ledger simulation replay includes simulated event"
    (contains_substring (Ledger.replay ledger_web simulation_world) ("Event " ^ simulation_event));
  assert_true "ledger simulation diff is isolated to fork"
    (contains_substring (Ledger.diff ledger_web world1 simulation_world)
       ("only_b=" ^ simulation_event));
  assert_true "ledger simulation updates branch pointer"
    (contains_substring (Ledger.branches ledger_web) ("branch feature world=" ^ simulation_world));
  let harness_compare_path = Filename.concat ledger_web "branch-compare.pth" in
  let harness_compare_content = "harness branchComparison = example ledgerDiff\n" in
  write_file harness_compare_path harness_compare_content;
  let _control_event, control_world =
    Ledger.simulate ledger_web "control" world1 "try control local-storage value"
  in
  let branch_comparison =
    Ledger.compare_branches_by_harness ledger_web harness_compare_path "feature" "control"
  in
  assert_true "ledger harness branch comparison reports diff"
    (contains_substring branch_comparison "protoss-branch-harness-comparison-v1"
    && contains_substring branch_comparison
         ("harness-ref=" ^ Kernel.hash_string harness_compare_content)
    && contains_substring branch_comparison ("left-world=" ^ simulation_world)
    && contains_substring branch_comparison ("right-world=" ^ control_world)
    && contains_substring branch_comparison "result=diff");
  assert_true "ledger harness branch comparison passes identical branch"
    (contains_substring
       (Ledger.compare_branches_by_harness ledger_web harness_compare_path "feature" "feature")
       "result=pass");

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
  let human_request_codec_ref =
    Canonical_ir.host_codec_ref (Ast.TRecord [ ("prompt", Ast.TString) ])
  in
  let string_response_codec_ref = Canonical_ir.host_codec_ref Ast.TString in
  let human_cap_scope_ref = Kernel.capability_scope_ref [ "Human.ask" ] in
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
  assert_true "ledger request records host codec version"
    (contains_substring inspected_request_event
       ("host-codec-version=" ^ Canonical_ir.host_codec_version));
  assert_true "ledger request records request codec ref"
    (contains_substring inspected_request_event
       ("request-codec-ref=" ^ human_request_codec_ref));
  assert_true "ledger request records response codec ref"
    (contains_substring inspected_request_event
       ("response-codec-ref=" ^ string_response_codec_ref));
  assert_true "ledger request records cap scope ref"
    (contains_substring inspected_request_event ("cap-scope-ref=" ^ human_cap_scope_ref));
  assert_true "ledger world inspectable" (String.length (Ledger.inspect_world ledger_root next_world) > 0);
  let resume_event, resume_world =
    Ledger.record_resume ledger_root next_world event "String:Ada" "Done \"Ada\""
  in
  let inspected_resume_event = Ledger.inspect_event ledger_root resume_event in
  assert_true "ledger resume event inspectable" (String.length inspected_resume_event > 0);
  assert_true "ledger resume records response type"
    (contains_substring inspected_resume_event "response-type=String");
  assert_true "ledger resume records signature ref"
    (contains_substring inspected_resume_event ("request-signature-ref=" ^ human_signature_ref));
  assert_true "ledger resume records host codec version"
    (contains_substring inspected_resume_event
       ("host-codec-version=" ^ Canonical_ir.host_codec_version));
  assert_true "ledger resume records response codec ref"
    (contains_substring inspected_resume_event
       ("response-codec-ref=" ^ string_response_codec_ref));
  assert_true "ledger resume world inspectable"
    (String.length (Ledger.inspect_world ledger_root resume_world) > 0);
  let negative_event, negative_world =
    Ledger.record_external_error ledger_root next_world event "HOST_TIMEOUT" "host timed out"
  in
  let inspected_negative_event = Ledger.inspect_event ledger_root negative_event in
  assert_true "ledger negative external event inspectable"
    (String.length inspected_negative_event > 0);
  assert_true "ledger negative external event records kind"
    (contains_substring inspected_negative_event "kind=external-error");
  assert_true "ledger negative external event links request"
    (contains_substring inspected_negative_event ("negative=" ^ event));
  assert_true "ledger negative external event records response type"
    (contains_substring inspected_negative_event "response-type=String");
  assert_true "ledger negative external event records signature ref"
    (contains_substring inspected_negative_event ("request-signature-ref=" ^ human_signature_ref));
  assert_true "ledger negative external event records response codec ref"
    (contains_substring inspected_negative_event
       ("response-codec-ref=" ^ string_response_codec_ref));
  assert_true "ledger negative external event records typed error"
    (contains_substring inspected_negative_event "error-code=HOST_TIMEOUT"
    && contains_substring inspected_negative_event "error-message=host timed out");
  assert_true "ledger negative external world inspectable"
    (String.length (Ledger.inspect_world ledger_root negative_world) > 0);
  let merged_world = Ledger.merge ledger_root resume_world negative_world in
  let inspected_merged_world = Ledger.inspect_world ledger_root merged_world in
  assert_true "ledger merged world records left parent"
    (contains_substring inspected_merged_world "merge-left=");
  assert_true "ledger merged world records right parent"
    (contains_substring inspected_merged_world "merge-right=");
  let merged_replay = Ledger.replay ledger_root merged_world in
  assert_true "ledger merged replay includes resume event"
    (contains_substring merged_replay ("Event " ^ resume_event));
  assert_true "ledger merged replay includes negative event"
    (contains_substring merged_replay ("Event " ^ negative_event));
  assert_true "ledger branches list merge parents"
    (contains_substring (Ledger.branches ledger_root) "merge-left=");
  let bad_event_hash = "p2:bad-event-hash" in
  Store.write_file_atomic (Ledger.event_path ledger_root bad_event_hash) inspected_request_event;
  (try
     ignore (Ledger.inspect_event ledger_root bad_event_hash);
     fail "ledger event under wrong hash should be rejected"
   with Failure msg ->
     assert_true "ledger event hash mismatch is reported"
       (contains_substring msg "content hash mismatch"));
  let bad_world_hash = "p2:bad-world-hash" in
  Store.write_file_atomic (Ledger.world_path ledger_root bad_world_hash) inspected_merged_world;
  (try
     ignore (Ledger.inspect_world ledger_root bad_world_hash);
     fail "ledger world under wrong hash should be rejected"
   with Failure msg ->
     assert_true "ledger world hash mismatch is reported"
       (contains_substring msg "hash mismatch"));
  let bad_world_no_event = "p2:bad-world-no-event" in
  Store.write_file_atomic (Ledger.world_path ledger_root bad_world_no_event)
    ("previous=" ^ next_world ^ "\nevent=\n");
  (try
     ignore (Ledger.inspect_world ledger_root bad_world_no_event);
     fail "ledger world without event should be rejected"
   with Failure msg ->
     assert_true "ledger world requires explicit event"
       (contains_substring msg "previous/event"));
  let bad_negative_signature_ref_event = "p2:bad-negative-signature-ref-event" in
  Store.write_file_atomic (Ledger.event_path ledger_root bad_negative_signature_ref_event)
    (replace_once inspected_negative_event ("request-signature-ref=" ^ human_signature_ref)
       "request-signature-ref=p2:bad");
  (try
     ignore (Ledger.inspect_event ledger_root bad_negative_signature_ref_event);
     fail "ledger negative external event with bad signature ref should be rejected"
   with Failure _ -> ());
  let bad_resume_signature_ref_event = "p2:bad-resume-signature-ref-event" in
  Store.write_file_atomic (Ledger.event_path ledger_root bad_resume_signature_ref_event)
    (replace_once inspected_resume_event ("request-signature-ref=" ^ human_signature_ref)
       "request-signature-ref=p2:bad");
  (try
     ignore (Ledger.inspect_event ledger_root bad_resume_signature_ref_event);
     fail "ledger resume event with bad signature ref should be rejected"
   with Failure _ -> ());
  let bad_resume_codec_ref_event = "p2:bad-resume-codec-ref-event" in
  Store.write_file_atomic (Ledger.event_path ledger_root bad_resume_codec_ref_event)
    (replace_once inspected_resume_event ("response-codec-ref=" ^ string_response_codec_ref)
       "response-codec-ref=p2:bad");
  (try
     ignore (Ledger.inspect_event ledger_root bad_resume_codec_ref_event);
     fail "ledger resume event with bad codec ref should be rejected"
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
  let bad_request_codec_ref_event = "p2:bad-request-codec-ref-event" in
  Store.write_file_atomic (Ledger.event_path ledger_root bad_request_codec_ref_event)
    (replace_once inspected_request_event ("request-codec-ref=" ^ human_request_codec_ref)
       "request-codec-ref=p2:bad");
  (try
     ignore (Ledger.inspect_event ledger_root bad_request_codec_ref_event);
     fail "ledger request event with bad request codec ref should be rejected"
   with Failure _ -> ());
  let bad_response_codec_ref_event = "p2:bad-response-codec-ref-event" in
  Store.write_file_atomic (Ledger.event_path ledger_root bad_response_codec_ref_event)
    (replace_once inspected_request_event ("response-codec-ref=" ^ string_response_codec_ref)
       "response-codec-ref=p2:bad");
  (try
     ignore (Ledger.inspect_event ledger_root bad_response_codec_ref_event);
     fail "ledger request event with bad response codec ref should be rejected"
   with Failure _ -> ());
  let bad_cap_scope_ref_event = "p2:bad-cap-scope-ref-event" in
  Store.write_file_atomic (Ledger.event_path ledger_root bad_cap_scope_ref_event)
    (replace_once inspected_request_event ("cap-scope-ref=" ^ human_cap_scope_ref)
       "cap-scope-ref=p2:bad");
  (try
     ignore (Ledger.inspect_event ledger_root bad_cap_scope_ref_event);
     fail "ledger request event with bad cap scope ref should be rejected"
   with Failure _ -> ());
  let bad_resume_event = "p2:bad-resume-event" in
  Store.write_file_atomic (Ledger.event_path ledger_root bad_resume_event)
    (replace_once inspected_resume_event "response=String:Ada" "response=Nat:1");
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
  let http_request_codec_ref =
    Canonical_ir.host_codec_ref (Ast.TRecord [ ("url", Ast.TString) ])
  in
  let http_cap_scope_ref = Kernel.capability_scope_ref [ "Http.get" ] in
  Store.write_file_atomic (Ledger.event_path ledger_root mismatched_request_event)
    ("world=x\nkind=request\nrequest-id=req\nrequest=HttpGet:https://example.invalid\n\
      capability=Http.get\ncapability-ref="
    ^ http_capability_ref
    ^ "\nrequest-tag=HttpGet\nrequest-signature-ref=" ^ http_signature_ref
    ^ "\nrequest-payload-type=(Record (url String))\n\
      response-type=String\nhost-codec-version="
    ^ Canonical_ir.host_codec_version
    ^ "\nrequest-codec-ref=" ^ http_request_codec_ref
    ^ "\nresponse-codec-ref=" ^ string_response_codec_ref
    ^ "\ncontinuation-id=cont\ncap-scope=Http.get\ncap-scope-ref=" ^ http_cap_scope_ref
    ^ "\nsuspended="
    ^ String.escaped suspended ^ "\n");
  let bad_resume_mismatch_event = "p2:bad-resume-mismatch-event" in
  Store.write_file_atomic (Ledger.event_path ledger_root bad_resume_mismatch_event)
    ("world=x\nkind=resume\nresume=" ^ mismatched_request_event
   ^ "\nrequest-signature-ref=" ^ http_signature_ref
   ^ "\nresponse-type=String\nhost-codec-version=" ^ Canonical_ir.host_codec_version
   ^ "\nresponse-codec-ref=" ^ string_response_codec_ref
   ^ "\nresponse=String:Ada\nresult=bad\n");
  (try
     ignore (Ledger.inspect_event ledger_root bad_resume_mismatch_event);
     fail "ledger resume event with mismatched suspended request should be rejected"
   with Failure _ -> ());
  (try
     ignore (Runtime.parse_suspended "(protoss-runtime-v2 (suspended (request ReadClock)))");
     fail "invalid resume suspension should be rejected"
   with Kernel.Error _ -> ())));

  if integration_part "runtime" then (
  trace_test "integration:runtime-store";
  (* -- Runtime Store Foundation -------------------------------------------- *)
  let rt_stdlib = find_up (Sys.getcwd ()) "stdlib/prelude.protoss" in
  let rt_src = find_up (Sys.getcwd ()) "examples/web/todo_app" in
  let rt_project = temp_dir "runtime-todo" in
  (* Copy src/ only: the example tree in _build is read-only and may carry a
     git-ignored .protoss store from CLI runs; the manifest is written fresh
     below (same hermeticity rule as the web slices). *)
  ensure_dir rt_project;
  copy_tree (Filename.concat rt_src "src") (Filename.concat rt_project "src");
  write_file (Filename.concat rt_project "protoss.toml")
    ("name = \"todo-runtime-alpha\"\nversion = \"0.1.0\"\nentrypoints = [\"src/app.protoss\"]\nstdlib = \""
    ^ rt_stdlib
    ^ "\"\nsource_dirs = [\"src\"]\nstore_dir = \".protoss/store\"\ncache_dir = \".protoss/cache\"\ncapabilities = [\"Local.storage\"]\n");
  let rt_dir = Runtime_store.runtime_path rt_project in
  ignore (Runtime_store.init rt_project);
  assert_true "runtime init creates runtime.json"
    (Sys.file_exists (Filename.concat rt_dir "runtime.json"));
  assert_true "runtime init creates latest-world"
    (Sys.file_exists (Filename.concat rt_dir "latest-world"));
  List.iter
    (fun d ->
      assert_true ("runtime init creates dir " ^ d)
        (Sys.is_directory (Filename.concat rt_dir d)))
    [ "worlds"; "events"; "requests"; "responses"; "snapshots" ];
  let rt_worlds = Filename.concat rt_dir "worlds" in
  assert_true "runtime init writes one genesis world object"
    (Array.length (Sys.readdir rt_worlds) = 1);
  (* idempotent: re-init yields identical runtime.json and no duplicate worlds *)
  let rt_json_1 = Store.read_file (Filename.concat rt_dir "runtime.json") in
  ignore (Runtime_store.init rt_project);
  let rt_json_2 = Store.read_file (Filename.concat rt_dir "runtime.json") in
  assert_equal "runtime init idempotent runtime.json" rt_json_1 rt_json_2;
  assert_true "runtime init idempotent single world"
    (Array.length (Sys.readdir rt_worlds) = 1);
  (* status *)
  let rt_status = Runtime_store.status rt_project in
  assert_true "runtime status reports genesis" (contains_substring rt_status "GenesisWorld p2:");
  assert_true "runtime status reports latest" (contains_substring rt_status "LatestWorld p2:");
  (* inspect: deterministic and valid JSON *)
  let rt_inspect_1 = Runtime_store.inspect rt_project in
  let rt_inspect_2 = Runtime_store.inspect rt_project in
  assert_equal "runtime inspect deterministic" rt_inspect_1 rt_inspect_2;
  ignore (Json.parse rt_inspect_1);
  (* world: returns an existing genesis object *)
  assert_true "runtime world shows genesis kind"
    (contains_substring (Runtime_store.world rt_project) "genesis");
  (* audit: passes on a clean runtime *)
  assert_true "runtime audit clean"
    (contains_substring (Runtime_store.audit rt_project) "Runtime audit OK");
  (* corruption: latest-world points to an unknown world object *)
  let rt_latest_path = Filename.concat rt_dir "latest-world" in
  let rt_latest_good = Store.read_file rt_latest_path in
  Store.write_file_atomic rt_latest_path "p2:deadbeef";
  (try
     ignore (Runtime_store.audit rt_project);
     fail "runtime audit should reject an unknown latest-world"
   with Runtime_store.Error _ -> ());
  Store.write_file_atomic rt_latest_path rt_latest_good;
  assert_true "runtime audit clean after latest-world restored"
    (contains_substring (Runtime_store.audit rt_project) "Runtime audit OK");
  (* corruption: world object content no longer matches its content-address *)
  let rt_world_file = Filename.concat rt_worlds (Sys.readdir rt_worlds).(0) in
  let rt_world_good = Store.read_file rt_world_file in
  Store.write_file rt_world_file (rt_world_good ^ "tampered");
  (try
     ignore (Runtime_store.audit rt_project);
     fail "runtime audit should reject a tampered world object"
   with Runtime_store.Error _ -> ());
  Store.write_file rt_world_file rt_world_good;
  assert_true "runtime audit clean after world object restored"
    (contains_substring (Runtime_store.audit rt_project) "Runtime audit OK");
  (* reset: refused without confirmation, recreates a clean runtime with --yes *)
  (try
     ignore (Runtime_store.reset ~confirm:false rt_project);
     fail "runtime reset without --yes should be refused"
   with Runtime_store.Error _ -> ());
  ignore (Runtime_store.reset ~confirm:true rt_project);
  assert_true "runtime reset recreates a clean runtime"
    (contains_substring (Runtime_store.audit rt_project) "Runtime audit OK");
  assert_true "runtime reset keeps a single world"
    (Array.length (Sys.readdir rt_worlds) = 1);
  trace_test "integration:done");

  if not run_integration_tests then
    print_endline "integration tests skipped (set PROTOSS_RUN_INTEGRATION_TESTS=1)";

  print_endline "protoss tests ok"

let () =
  if Sys.getenv_opt "PROTOSS_RUN_SELF_HOST_TESTS" <> Some "1" then
    print_endline "self-host frontend tests skipped (set PROTOSS_RUN_SELF_HOST_TESTS=1)"
  else
  (* ===== Self-hosted frontend: parity with OCaml + conformance goldens =====
     One program = prelude + every driver def, checked once, then each entry is
     normalized through the evaluator. This exercises the Protoss-implemented
     frontend (stdlib/prelude.protoss) end to end. *)
  let read_all path =
    let ic = open_in_bin path in
    Fun.protect
      ~finally:(fun () -> close_in_noerr ic)
      (fun () -> really_input_string ic (in_channel_length ic))
  in
  let repo rel = find_up (Sys.getcwd ()) rel in
  let chomp s =
    let n = String.length s in
    if n > 0 && s.[n - 1] = '\n' then String.sub s 0 (n - 1) else s
  in
  let prelude = read_all (repo "stdlib/prelude.protoss") in
  let driver = Buffer.create (String.length prelude + 8192) in
  Buffer.add_string driver prelude;
  Buffer.add_char driver '\n';
  let entries = ref [] in
  let register name typ expr verify =
    Buffer.add_string driver (Printf.sprintf "(def %s %s (%s))\n" name typ expr);
    entries := (name, verify) :: !entries
  in
  let ocaml_clean src =
    try
      ignore (Parser.parse_string src |> Kernel.check_program);
      true
    with _ -> false
  in
  (* ---- conformance: self-hosted output must equal the checked-in golden ---- *)
  let conf name fn typ input golden =
    let src = read_all (repo ("conformance/self_host/inputs/" ^ input)) in
    let expected = chomp (read_all (repo ("conformance/self_host/golden/" ^ golden))) in
    register name typ
      (fn ^ " " ^ Ast.quote src)
      (fun got -> assert_equal ("self-host conformance " ^ golden) expected (chomp got))
  in
  conf "__conf_parse" "Protoss.selfParseJson" "String" "sample_module.protoss"
    "sample_parse.json";
  conf "__conf_resolve" "Protoss.selfResolveJson" "String" "sample_module.protoss"
    "sample_resolve.json";
  conf "__conf_deps" "Protoss.selfDepsJson" "String" "sample_module.protoss"
    "sample_deps.json";
  conf "__conf_caps" "Protoss.selfCapabilitiesJson" "String" "sample_module.protoss"
    "sample_capabilities.json";
  conf "__conf_static" "Protoss.selfStaticText" "String" "sample_module.protoss"
    "sample_static.json";
  conf "__conf_fmt" "Protoss.formatText" "(Result String String)"
    "sample_module.protoss" "sample_fmt.protoss";
  conf "__conf_cycle" "Protoss.selfDepsJson" "String" "cycle.protoss" "cycle_deps.json";
  conf "__conf_capmis" "Protoss.selfCapabilitiesJson" "String"
    "capability_mismatch.protoss" "capability_mismatch.json";
  conf "__conf_malformed" "Protoss.selfParseJson" "String" "malformed.protoss"
    "malformed_diagnostics.json";
  (* ---- parity: report-level agreement with the OCaml frontend ---- *)
  let parity name fn src needle ocaml_ok =
    register name "String"
      (fn ^ " " ^ Ast.quote src)
      (fun got ->
        assert_true (name ^ " self report contains " ^ needle)
          (contains_substring got needle));
    assert_true (name ^ " ocaml parity") (Bool.equal (ocaml_clean src) ocaml_ok)
  in
  parity "__par_valid" "Protoss.selfResolveJson" "(def x Nat 1) (def y Nat (succ x))"
    "\"missingTerms\":[]" true;
  parity "__par_exports" "Protoss.selfParseJson"
    "(module M) (export x) (def x Nat 1)" "\"exports\":[\"x\"]" true;
  parity "__par_unknown" "Protoss.selfResolveJson" "(def x Nat ghost)"
    "\"missingTerms\":[\"ghost\"]" false;
  parity "__par_duptype" "Protoss.selfResolveJson"
    "(record T (a Nat)) (record T (b Nat))" "\"duplicateTypes\":[\"T\"]" false;
  parity "__par_dupfield" "Protoss.selfResolveJson"
    "(record T (a Nat)) (record T (b Nat))" "\"duplicateTypes\":[\"T\"]" false;
  let component_report name fn component =
    register name "String"
      (fn ^ " " ^ Ast.quote "(def x Nat 1)")
      (fun got ->
        assert_true (name ^ " reports ok") (contains_substring got "\"status\":\"ok\"");
        assert_true (name ^ " reports component")
          (contains_substring got ("\"component\":\"" ^ component ^ "\"")))
  in
  component_report "__self_human_parser" "Protoss.selfHumanParserJson" "protoss-h-parser";
  component_report "__self_human_pretty_printer" "Protoss.selfHumanPrettyPrinterJson"
    "protoss-h-pretty-printer";
  component_report "__self_canonicalizer" "Protoss.selfCanonicalizerJson" "canonicalizer";
  component_report "__self_normalizer" "Protoss.selfNormalizerJson" "normalizer";
  component_report "__self_typechecker" "Protoss.selfTypecheckerJson" "typechecker";
  component_report "__self_patch_validator" "Protoss.selfPatchValidatorJson" "patch-validator";
  component_report "__self_harness_runner" "Protoss.selfHarnessRunnerJson" "harness-runner";
  component_report "__self_package_resolver" "Protoss.selfPackageResolverJson" "package-resolver";
  component_report "__self_mcp_server" "Protoss.selfMcpServerJson" "mcp-server";
  component_report "__self_optimizer" "Protoss.selfOptimizerJson" "optimizer";
  component_report "__self_compiler_backend" "Protoss.selfCompilerBackendJson" "compiler-backend";
  component_report "__self_trusted_boundary" "Protoss.selfTrustedBoundaryJson" "trusted-boundary";
  component_report "__self_bootstrap_plan" "Protoss.selfBootstrapPlanJson" "bootstrap";
  register "__tc_valid" "String"
    ("Protoss.tcTextJson " ^ Ast.quote "(def x Nat 1)")
    (fun got ->
      assert_true "self typecheck valid reports ok"
        (contains_substring got "\"status\":\"ok\"");
      assert_true "self typecheck valid reports type"
        (contains_substring got "\"type\":\"Nat\""));
  register "__tc_invalid" "String"
    ("Protoss.tcTextJson " ^ Ast.quote "(def bad Bool 1)")
    (fun got ->
      assert_true "self typecheck invalid reports error"
        (contains_substring got "\"status\":\"error\"");
      assert_true "self typecheck invalid reports mismatch"
        (contains_substring got "\"code\":\"SELF_TC002\""));
  let deep_tc_source =
    "(def main Nat (let (x (succ 1)) (let (y (succ x)) y))) "
    ^ "(def incLater (-> Nat Nat) (lambda (n Nat) (let (m (succ n)) m))) "
    ^ "(def nestedArg Nat (succ (succ 1))) "
    ^ "(def items (List Nat) (Cons Nat (succ 1) (Cons Nat 2 (Nil Nat)))) "
    ^ "(def folded Nat (foldList items 0 (lambda (x Nat) (lambda (acc Nat) (succ acc))))) "
    ^ "(def chosen Nat (case true (true (let (z 1) (succ z))) (false 0)))"
  in
  register "__tc_deep" "String"
    ("Protoss.tcTextJson " ^ Ast.quote deep_tc_source)
    (fun got ->
      assert_true "self typecheck deep reports ok"
        (contains_substring got "\"status\":\"ok\"");
      assert_true "self typecheck deep reports function type"
        (contains_substring got "\"name\":\"incLater\"");
      assert_true "self typecheck deep checks nested app arg"
        (contains_substring got "\"name\":\"nestedArg\"");
      assert_true "self typecheck deep has no unsupported constructs"
        (contains_substring got "\"unsupported\":[]"));
  register "__tc_nested_arg_invalid" "String"
    ("Protoss.tcTextJson "
    ^ Ast.quote "(def bad Nat (succ (let (flag true) flag)))")
    (fun got ->
      assert_true "self typecheck nested app arg reports error"
        (contains_substring got "\"status\":\"error\"");
      assert_true "self typecheck nested app arg reports mismatch"
        (contains_substring got "\"code\":\"SELF_TC002\"");
      assert_true "self typecheck nested app arg expected Nat"
        (contains_substring got "\"expected\":\"Nat\"");
      assert_true "self typecheck nested app arg actual Bool"
        (contains_substring got "\"actual\":\"Bool\""));
  register "__tc_record_field_nested" "String"
    ("Protoss.tcTextJson "
    ^ Ast.quote
        "(record Person (age Nat)) (def main Person (record (age (succ 35))))")
    (fun got ->
      assert_true "self typecheck nested record field reports ok"
        (contains_substring got "\"status\":\"ok\"");
      assert_true "self typecheck nested record field has no unsupported constructs"
        (contains_substring got "\"unsupported\":[]"));
  register "__tc_record_field_nested_invalid" "String"
    ("Protoss.tcTextJson "
    ^ Ast.quote
        "(record Person (age Nat)) (def main Person (record (age (let (flag true) flag))))")
    (fun got ->
      assert_true "self typecheck nested record field reports error"
        (contains_substring got "\"status\":\"error\"");
      assert_true "self typecheck nested record field reports mismatch"
        (contains_substring got "\"code\":\"SELF_TC002\"");
      assert_true "self typecheck nested record field actual Bool"
        (contains_substring got "\"actual\":\"Bool\""));
  register "__tc_case_branch_nested_invalid" "String"
    ("Protoss.tcTextJson "
    ^ Ast.quote
        "(def bad Nat (case true (true (let (flag true) flag)) (false 0)))")
    (fun got ->
      assert_true "self typecheck nested Bool case branch reports error"
        (contains_substring got "\"status\":\"error\"");
      assert_true "self typecheck nested Bool case branch reports mismatch"
        (contains_substring got "\"code\":\"SELF_TC002\"");
      assert_true "self typecheck nested Bool case branch expected Nat"
        (contains_substring got "\"expected\":\"Nat\"");
      assert_true "self typecheck nested Bool case branch actual Bool"
        (contains_substring got "\"actual\":\"Bool\"");
    );
  let register_self_tc_file name file verify =
    register name "String"
      ("Protoss.tcTextJson " ^ Ast.quote (read_all (repo file)))
      verify
  in
  register_self_tc_file "__tc_file_variants"
    "examples/self_host/typecheck_variants.protoss"
    (fun got ->
      assert_true "self typecheck variant case reports ok"
        (contains_substring got "\"status\":\"ok\"");
      assert_true "self typecheck variant case reports selected"
        (contains_substring got "\"name\":\"selected\"");
      assert_true "self typecheck variant case has no unsupported constructs"
        (contains_substring got "\"unsupported\":[]"));
  register_self_tc_file "__tc_file_variant_invalid"
    "examples/self_host/typecheck_variant_invalid.protoss"
    (fun got ->
      assert_true "self typecheck variant case invalid reports error"
        (contains_substring got "\"status\":\"error\"");
      assert_true "self typecheck variant case invalid reports mismatch"
        (contains_substring got "\"code\":\"SELF_TC002\""));
  register_self_tc_file "__tc_file_variant_missing"
    "examples/self_host/typecheck_variant_missing_branch.protoss"
    (fun got ->
      assert_true "self typecheck variant case missing branch reports error"
        (contains_substring got "\"status\":\"error\"");
      assert_true "self typecheck variant case missing branch reports code"
        (contains_substring got "\"code\":\"SELF_TC009\"");
      assert_true "self typecheck variant case missing branch reports message"
        (contains_substring got "variant case missing branch"));
  register_self_tc_file "__tc_file_variant_unknown"
    "examples/self_host/typecheck_variant_unknown_branch.protoss"
    (fun got ->
      assert_true "self typecheck variant case unknown branch reports error"
        (contains_substring got "\"status\":\"error\"");
      assert_true "self typecheck variant case unknown branch reports code"
        (contains_substring got "\"code\":\"SELF_TC009\"");
      assert_true "self typecheck variant case unknown branch reports message"
        (contains_substring got "variant case unknown branch"));
  register_self_tc_file "__tc_file_fold_variant"
    "examples/self_host/typecheck_fold_variant.protoss"
    (fun got ->
      assert_true "self typecheck foldVariant reports ok"
        (contains_substring got "\"status\":\"ok\"");
      assert_true "self typecheck foldVariant reports main"
        (contains_substring got "\"name\":\"main\"");
      assert_true "self typecheck foldVariant has no unsupported constructs"
        (contains_substring got "\"unsupported\":[]"));
  register_self_tc_file "__tc_file_fold_variant_invalid"
    "examples/self_host/typecheck_fold_variant_invalid.protoss"
    (fun got ->
      assert_true "self typecheck foldVariant invalid reports error"
        (contains_substring got "\"status\":\"error\"");
      assert_true "self typecheck foldVariant invalid reports mismatch"
        (contains_substring got "\"code\":\"SELF_TC002\""));
  register_self_tc_file "__tc_file_fold_variant_unknown"
    "examples/self_host/typecheck_fold_variant_unknown_branch.protoss"
    (fun got ->
      assert_true "self typecheck foldVariant unknown branch reports error"
        (contains_substring got "\"status\":\"error\"");
      assert_true "self typecheck foldVariant unknown branch reports code"
        (contains_substring got "\"code\":\"SELF_TC009\"");
      assert_true "self typecheck foldVariant unknown branch reports message"
        (contains_substring got "variant case unknown branch"));
  register_self_tc_file "__tc_file_fold_variant_target_invalid"
    "examples/self_host/typecheck_fold_variant_target_invalid.protoss"
    (fun got ->
      assert_true "self typecheck foldVariant target reports error"
        (contains_substring got "\"status\":\"error\"");
      assert_true "self typecheck foldVariant target reports mismatch"
        (contains_substring got "\"code\":\"SELF_TC002\"");
      assert_true "self typecheck foldVariant target reports message"
        (contains_substring got "foldVariant target must be Variant"));
  register_self_tc_file "__tc_file_process"
    "examples/self_host/typecheck_process.protoss"
    (fun got ->
      assert_true "self typecheck process reports ok"
        (contains_substring got "\"status\":\"ok\"");
      assert_true "self typecheck process reports bind definition"
        (contains_substring got "\"name\":\"askThenDone\"");
      assert_true "self typecheck process reports let definition"
        (contains_substring got "\"name\":\"askViaLet\"");
      assert_true "self typecheck process has no unsupported constructs"
        (contains_substring got "\"unsupported\":[]"));
  register_self_tc_file "__tc_file_process_bind_invalid"
    "examples/self_host/typecheck_process_bind_invalid.protoss"
    (fun got ->
      assert_true "self typecheck process invalid bind reports error"
        (contains_substring got "\"status\":\"error\"");
      assert_true "self typecheck process invalid bind reports mismatch"
        (contains_substring got "\"code\":\"SELF_TC002\"");
      assert_true "self typecheck process invalid bind reports expected process"
        (contains_substring got "\"expected\":\"(Process String)\""));
  register_self_tc_file "__tc_file_process_annotation_invalid"
    "examples/self_host/typecheck_process_annotation_invalid.protoss"
    (fun got ->
      assert_true "self typecheck process annotation reports error"
        (contains_substring got "\"status\":\"error\"");
      assert_true "self typecheck process annotation reports mismatch"
        (contains_substring got "\"code\":\"SELF_TC002\"");
      assert_true "self typecheck process annotation reports expected process Nat"
        (contains_substring got "\"expected\":\"(Process Nat)\""));
  register_self_tc_file "__tc_file_process_let_pure_invalid"
    "examples/self_host/typecheck_process_let_pure_invalid.protoss"
    (fun got ->
      assert_true "self typecheck process pure let reports error"
        (contains_substring got "\"status\":\"error\"");
      assert_true "self typecheck process pure let reports mismatch"
        (contains_substring got "\"code\":\"SELF_TC002\"");
      assert_true "self typecheck process pure let reports message"
        (contains_substring got "Process used as pure value in let p"));
  register_self_tc_file "__tc_file_defcap"
    "examples/self_host/typecheck_defcap.protoss"
    (fun got ->
      assert_true "self typecheck defcap reports ok"
        (contains_substring got "\"status\":\"ok\"");
      assert_true "self typecheck defcap reports scoped definition"
        (contains_substring got "\"name\":\"askAgain\"");
      assert_true "self typecheck defcap has no unsupported constructs"
        (contains_substring got "\"unsupported\":[]"));
  register_self_tc_file "__tc_file_defcap_invalid"
    "examples/self_host/typecheck_defcap_invalid.protoss"
    (fun got ->
      assert_true "self typecheck defcap invalid reports error"
        (contains_substring got "\"status\":\"error\"");
      assert_true "self typecheck defcap invalid reports mismatch"
        (contains_substring got "\"code\":\"SELF_TC002\""));
  register_self_tc_file "__tc_file_defcap_capability_invalid"
    "examples/self_host/typecheck_defcap_capability_invalid.protoss"
    (fun got ->
      assert_true "self typecheck defcap capability reports error"
        (contains_substring got "\"status\":\"error\"");
      assert_true "self typecheck defcap capability reports static error"
        (contains_substring got "\"code\":\"SELF_TC010\"");
      assert_true "self typecheck defcap capability reports missing declaration"
        (contains_substring got "missing capability declaration"));
  register_self_tc_file "__tc_file_inst"
    "examples/self_host/typecheck_inst.protoss"
    (fun got ->
      assert_true "self typecheck inst reports ok"
        (contains_substring got "\"status\":\"ok\"");
      assert_true "self typecheck inst reports monomorphic main"
        (contains_substring got "\"name\":\"main\"");
      assert_true "self typecheck inst substitutes nested list type"
        (contains_substring got "\"type\":\"(List String)\"");
      assert_true "self typecheck inst has no unsupported constructs"
        (contains_substring got "\"unsupported\":[]"));
  register_self_tc_file "__tc_file_implicit_poly"
    "examples/self_host/typecheck_implicit_poly.protoss"
    (fun got ->
      assert_true "self typecheck implicit poly reports ok"
        (contains_substring got "\"status\":\"ok\"");
      assert_true "self typecheck implicit poly reports contextual function"
        (contains_substring got "\"name\":\"f\"");
      assert_true "self typecheck implicit poly reports Nat application"
        (contains_substring got "\"name\":\"n\"");
      assert_true "self typecheck implicit poly reports List result"
        (contains_substring got "\"type\":\"(List Nat)\"");
      assert_true "self typecheck implicit poly has no unsupported constructs"
        (contains_substring got "\"unsupported\":[]"));
  register_self_tc_file "__tc_file_implicit_poly_contextual_direct"
    "examples/self_host/typecheck_implicit_poly_contextual_args_direct.protoss"
    (fun got ->
      assert_true "self typecheck implicit poly contextual direct reports ok"
        (contains_substring got "\"status\":\"ok\"");
      assert_true "self typecheck implicit poly contextual direct reports counted"
        (contains_substring got "\"name\":\"countedDirect\"");
      assert_true "self typecheck implicit poly contextual direct has no unsupported constructs"
        (contains_substring got "\"unsupported\":[]"));
  register_self_tc_file "__tc_file_implicit_poly_contextual_nil"
    "examples/self_host/typecheck_implicit_poly_contextual_args_nil.protoss"
    (fun got ->
      assert_true "self typecheck implicit poly contextual Nil reports ok"
        (contains_substring got "\"status\":\"ok\"");
      assert_true "self typecheck implicit poly contextual Nil reports counted"
        (contains_substring got "\"name\":\"countedNil\"");
      assert_true "self typecheck implicit poly contextual Nil has no unsupported constructs"
        (contains_substring got "\"unsupported\":[]"));
  register_self_tc_file "__tc_file_implicit_poly_invalid"
    "examples/self_host/typecheck_implicit_poly_invalid.protoss"
    (fun got ->
      assert_true "self typecheck implicit poly invalid reports error"
        (contains_substring got "\"status\":\"error\"");
      assert_true "self typecheck implicit poly invalid reports mismatch"
        (contains_substring got "\"code\":\"SELF_TC002\"");
      assert_true "self typecheck implicit poly invalid reports Bool actual"
        (contains_substring got "\"actual\":\"Bool\""));
  register_self_tc_file "__tc_file_implicit_poly_spine"
    "examples/self_host/typecheck_implicit_poly_spine.protoss"
    (fun got ->
      assert_true "self typecheck implicit poly spine reports ok"
        (contains_substring got "\"status\":\"ok\"");
      assert_true "self typecheck implicit poly spine reports picked"
        (contains_substring got "\"name\":\"picked\"");
      assert_true "self typecheck implicit poly spine checks expected suffix Nil"
        (contains_substring got "\"name\":\"keptEmpty\"");
      assert_true "self typecheck implicit poly spine checks expected prefix Nil"
        (contains_substring got "\"name\":\"keptPrefixEmpty\"");
      assert_true "self typecheck implicit poly spine reports mapped list"
        (contains_substring got "\"name\":\"sameWords\"");
      assert_true "self typecheck implicit poly spine has no unsupported constructs"
        (contains_substring got "\"unsupported\":[]"));
  register_self_tc_file "__tc_file_implicit_poly_spine_prefix_cons"
    "examples/self_host/typecheck_implicit_poly_spine_prefix_cons.protoss"
    (fun got ->
      assert_true "self typecheck implicit poly spine prefix Cons reports ok"
        (contains_substring got "\"status\":\"ok\"");
      assert_true "self typecheck implicit poly spine prefix Cons reports definition"
        (contains_substring got "\"name\":\"keptPrefixCons\"");
      assert_true "self typecheck implicit poly spine prefix Cons has no unsupported constructs"
        (contains_substring got "\"unsupported\":[]"));
  register_self_tc_file "__tc_file_implicit_poly_spine_contextual_args"
    "examples/self_host/typecheck_implicit_poly_spine_contextual_args.protoss"
    (fun got ->
      assert_true "self typecheck implicit poly spine contextual args reports ok"
        (contains_substring got "\"status\":\"ok\"");
      assert_true "self typecheck implicit poly spine contextual args reports definition"
        (contains_substring got "\"name\":\"countedSpine\"");
      assert_true "self typecheck implicit poly spine contextual args has no unsupported constructs"
        (contains_substring got "\"unsupported\":[]"));
  register_self_tc_file "__tc_file_implicit_poly_spine_contextual_typed_tail"
    "examples/self_host/typecheck_implicit_poly_spine_contextual_typed_tail.protoss"
    (fun got ->
      assert_true "self typecheck implicit poly spine contextual typed tail reports ok"
        (contains_substring got "\"status\":\"ok\"");
      assert_true "self typecheck implicit poly spine contextual typed tail reports definition"
        (contains_substring got "\"name\":\"countedTypedTail\"");
      assert_true
        "self typecheck implicit poly spine contextual typed tail has no unsupported constructs"
        (contains_substring got "\"unsupported\":[]"));
  register_self_tc_file "__tc_file_implicit_poly_spine_invalid"
    "examples/self_host/typecheck_implicit_poly_spine_invalid.protoss"
    (fun got ->
      assert_true "self typecheck implicit poly spine invalid reports error"
        (contains_substring got "\"status\":\"error\"");
      assert_true "self typecheck implicit poly spine invalid reports mismatch"
        (contains_substring got "\"code\":\"SELF_TC002\"");
      assert_true "self typecheck implicit poly spine invalid reports definition"
        (contains_substring got "\"definition\":\"badList\"");
      assert_true "self typecheck implicit poly spine invalid reports Bool actual"
        (contains_substring got "\"actual\":\"Bool\""));
  register_self_tc_file "__tc_file_implicit_poly_spine_prefix_invalid"
    "examples/self_host/typecheck_implicit_poly_spine_prefix_invalid.protoss"
    (fun got ->
      assert_true "self typecheck implicit poly spine prefix invalid reports error"
        (contains_substring got "\"status\":\"error\"");
      assert_true "self typecheck implicit poly spine prefix invalid reports mismatch"
        (contains_substring got "\"code\":\"SELF_TC002\"");
      assert_true "self typecheck implicit poly spine prefix invalid reports definition"
        (contains_substring got "\"definition\":\"badPrefixList\""));
  register_self_tc_file "__tc_file_inst_wrong_arity"
    "examples/self_host/typecheck_inst_wrong_arity.protoss"
    (fun got ->
      assert_true "self typecheck inst wrong arity reports error"
        (contains_substring got "\"status\":\"error\"");
      assert_true "self typecheck inst wrong arity reports inst code"
        (contains_substring got "\"code\":\"SELF_TC011\"");
      assert_true "self typecheck inst wrong arity reports message"
        (contains_substring got "wrong number of type arguments"));
  register_self_tc_file "__tc_file_defpoly_invalid"
    "examples/self_host/typecheck_defpoly_invalid.protoss"
    (fun got ->
      assert_true "self typecheck defpoly invalid reports error"
        (contains_substring got "\"status\":\"error\"");
      assert_true "self typecheck defpoly invalid reports mismatch"
        (contains_substring got "\"code\":\"SELF_TC002\"");
      assert_true "self typecheck defpoly invalid reports body actual"
        (contains_substring got "\"actual\":\"Nat\""));
  register_self_tc_file "__tc_file_defpolycap"
    "examples/self_host/typecheck_defpolycap.protoss"
    (fun got ->
      assert_true "self typecheck defpolycap reports ok"
        (contains_substring got "\"status\":\"ok\"");
      assert_true "self typecheck defpolycap reports main"
        (contains_substring got "\"name\":\"main\"");
      assert_true "self typecheck defpolycap has no unsupported constructs"
        (contains_substring got "\"unsupported\":[]"));
  register_self_tc_file "__tc_file_defrec"
    "examples/self_host/typecheck_defrec.protoss"
    (fun got ->
      assert_true "self typecheck defrec reports ok"
        (contains_substring got "\"status\":\"ok\"");
      assert_true "self typecheck defrec reports Nat recursion"
        (contains_substring got "\"name\":\"count\"");
      assert_true "self typecheck defrec reports List recursion"
        (contains_substring got "\"name\":\"bump\"");
      assert_true "self typecheck defrec reports polymorphic List recursion"
        (contains_substring got "\"name\":\"copy\"");
      assert_true "self typecheck defrec reports Variant recursion"
        (contains_substring got "\"name\":\"sizeTree\"");
      assert_true "self typecheck defrec reports Variant result"
        (contains_substring got "\"name\":\"treeSize\"");
      assert_true "self typecheck defrec has no unsupported constructs"
        (contains_substring got "\"unsupported\":[]"));
  register_self_tc_file "__tc_file_defrec_invalid"
    "examples/self_host/typecheck_defrec_invalid.protoss"
    (fun got ->
      assert_true "self typecheck defrec invalid reports error"
        (contains_substring got "\"status\":\"error\"");
      assert_true "self typecheck defrec invalid reports mismatch"
        (contains_substring got "\"code\":\"SELF_TC002\"");
      assert_true "self typecheck defrec invalid reports definition"
        (contains_substring got "\"definition\":\"badCount\"");
      assert_true "self typecheck defrec invalid reports Bool actual"
        (contains_substring got "\"actual\":\"Bool\""));
  register_self_tc_file "__tc_file_defrec_list_input_invalid"
    "examples/self_host/typecheck_defrec_list_input_invalid.protoss"
    (fun got ->
      assert_true "self typecheck defrec list input reports error"
        (contains_substring got "\"status\":\"error\"");
      assert_true "self typecheck defrec list input reports mismatch"
        (contains_substring got "\"code\":\"SELF_TC002\"");
      assert_true "self typecheck defrec list input reports definition"
        (contains_substring got "\"definition\":\"badList\"");
      assert_true "self typecheck defrec list input explains List"
        (contains_substring got "defrec list input must be List"));
  register_self_tc_file "__tc_file_defrec_variant_nonstructural_invalid"
    "examples/self_host/typecheck_defrec_variant_nonstructural_invalid.protoss"
    (fun got ->
      assert_true "self typecheck defrec variant nonstructural reports error"
        (contains_substring got "\"status\":\"error\"");
      assert_true "self typecheck defrec variant nonstructural reports mismatch"
        (contains_substring got "\"code\":\"SELF_TC002\"");
      assert_true "self typecheck defrec variant nonstructural reports definition"
        (contains_substring got "\"definition\":\"badSize\"");
      assert_true "self typecheck defrec variant nonstructural explains structural"
        (contains_substring got "recur argument is not a direct structural subterm"));
  register_self_tc_file "__tc_file_inferred_variants"
    "examples/self_host/typecheck_inferred_variants.protoss"
    (fun got ->
      assert_true "self typecheck inferred variants reports ok"
        (contains_substring got "\"status\":\"ok\"");
      assert_true "self typecheck inferred variants checks parameterized record"
        (contains_substring got "\"name\":\"boxed\"");
      assert_true "self typecheck inferred variants checks expected list"
        (contains_substring got "\"type\":\"(List (Maybe Nat))\"");
      assert_true "self typecheck inferred variants has no unsupported constructs"
        (contains_substring got "\"unsupported\":[]"));
  register_self_tc_file "__tc_file_inferred_variant_unknown"
    "examples/self_host/typecheck_inferred_variant_unknown.protoss"
    (fun got ->
      assert_true "self typecheck inferred variant unknown reports error"
        (contains_substring got "\"status\":\"error\"");
      assert_true "self typecheck inferred variant unknown reports code"
        (contains_substring got "\"code\":\"SELF_TC007\"");
      assert_true "self typecheck inferred variant unknown reports constructor"
        (contains_substring got "unknown variant constructor: Nope"));
  register_self_tc_file "__tc_file_inferred_variant_payload_invalid"
    "examples/self_host/typecheck_inferred_variant_payload_invalid.protoss"
    (fun got ->
      assert_true "self typecheck inferred variant payload reports error"
        (contains_substring got "\"status\":\"error\"");
      assert_true "self typecheck inferred variant payload reports mismatch"
        (contains_substring got "\"code\":\"SELF_TC002\"");
      assert_true "self typecheck inferred variant payload reports Bool"
        (contains_substring got "\"actual\":\"Bool\""));
  register "__tc_nested_invalid" "String"
    ("Protoss.tcTextJson "
    ^ Ast.quote "(def bad Nat (let (flag true) (succ flag)))")
    (fun got ->
      assert_true "self typecheck nested invalid reports error"
        (contains_substring got "\"status\":\"error\"");
      assert_true "self typecheck nested invalid reports mismatch"
        (contains_substring got "\"code\":\"SELF_TC002\"");
      assert_true "self typecheck nested invalid reports Bool actual"
        (contains_substring got "\"actual\":\"Bool\""));
  (* duplicate record field: the self-hosted frontend rejects it (parse-level
     diagnostic), mirroring the OCaml frontend. *)
  register "__par_dupfield2" "String"
    ("case (Protoss.checkTypeEnvText "
    ^ Ast.quote "(record T (a Nat) (a Nat))"
    ^ ") (Err e (Json.render (Protoss.jsonError e))) (Ok r (Json.render (Protoss.jsonStrings (get r duplicateRecordFields))))")
    (fun got ->
      assert_true "self frontend reports duplicate record field"
        (contains_substring got "duplicate record field"));
  assert_true "ocaml rejects duplicate record field"
    (not (ocaml_clean "(record T (a Nat) (a Nat))"));
  (* ===== Self-hosted canonicalizer: byte-for-byte parity with the kernel =====
     For every example fixture that the kernel checks in isolation, the
     Protoss-implemented canonicalizer (Protoss.canonProgramText) must either
     reproduce Kernel.serialize_checked_program exactly - DefIds are supplied
     by the kernel, which stays the only identity authority - or fail with an
     explicit error. It must never emit unverified canonical text. *)
  let canon_def_ids (checked : Kernel.checked) =
    checked.Kernel.defs
    |> List.map (fun (d : Kernel.checked_def) -> "(" ^ d.def.name ^ " " ^ d.def_id ^ ")")
    |> String.concat " "
  in
  let canon_expr def_ids source =
    "case ((Protoss.canonProgramText " ^ Ast.quote def_ids ^ ") " ^ Ast.quote source
    ^ ") (Err e ((String.concat \"ERR:\") e)) (Ok t ((String.concat \"OK:\") t))"
  in
  let has_prefix prefix s =
    String.length s >= String.length prefix
    && String.equal (String.sub s 0 (String.length prefix)) prefix
  in
  let strip_prefix prefix s =
    String.sub s (String.length prefix) (String.length s - String.length prefix)
  in
  let canon_parity_ok = ref 0 in
  let canon_unsupported = ref [] in
  let canon_fixture_index = ref 0 in
  let examples_dir = repo "examples" in
  Sys.readdir examples_dir |> Array.to_list |> List.sort String.compare
  |> List.iter (fun fixture ->
         if Filename.check_suffix fixture ".protoss" then begin
           let source = read_all (Filename.concat examples_dir fixture) in
           let source =
             if Elm_syntax.looks_like source then Elm_syntax.to_sexp_source source
             else source
           in
           match Parser.parse_string source |> Kernel.check_program with
           | exception _ -> () (* does not check in isolation: out of scope *)
           | fixture_checked ->
               let expected = Kernel.serialize_checked_program fixture_checked in
               let def_ids = canon_def_ids fixture_checked in
               incr canon_fixture_index;
               register
                 (Printf.sprintf "__canon_parity_%d" !canon_fixture_index)
                 "String" (canon_expr def_ids source)
                 (fun got ->
                   if has_prefix "OK:" got then begin
                     assert_equal
                       ("self canonicalizer parity for " ^ fixture)
                       expected (strip_prefix "OK:" got);
                     incr canon_parity_ok
                   end
                   else begin
                     assert_true
                       ("self canonicalizer explicit error for " ^ fixture)
                       (has_prefix "ERR:" got);
                     canon_unsupported := fixture :: !canon_unsupported
                   end)
         end);
  (* Golden contract: the canonical bytes for examples/basic.protoss are frozen
     in examples/basic.ptc, and the Protoss canonicalizer reproduces them. *)
  let golden_source = read_all (repo "examples/basic.protoss") in
  let golden_checked = Parser.parse_string golden_source |> Kernel.check_program in
  let golden_expected = chomp (read_all (repo "examples/basic.ptc")) in
  assert_equal "kernel canonical text matches examples/basic.ptc" golden_expected
    (Kernel.serialize_checked_program golden_checked);
  register "__canon_golden_basic" "String"
    (canon_expr (canon_def_ids golden_checked) golden_source)
    (fun got ->
      assert_equal "self canonicalizer reproduces examples/basic.ptc"
        ("OK:" ^ golden_expected) got);
  (* The kernel-supplied DefIds are load-bearing: with an empty table the
     component falls back to plain names, so the candidate must diverge from
     the kernel text and the byte comparison must be able to fail. *)
  register "__canon_defids_required" "String" (canon_expr "" golden_source)
    (fun got ->
      assert_true "self canonicalizer without def-ids still canonicalizes"
        (has_prefix "OK:" got);
      assert_true "self canonicalizer without def-ids diverges from the kernel"
        (not (String.equal (strip_prefix "OK:" got) golden_expected));
      assert_true "self canonicalizer without def-ids falls back to names"
        (contains_substring got "(ref choose)"));
  (* ---- run everything in a single checked program ---- *)
  let checked = Parser.parse_string (Buffer.contents driver) |> Kernel.check_program in
  List.iter
    (fun (name, verify) ->
      let v, _ = Runtime.normalize_def ~stdlib_fast_paths:true checked name in
      let got =
        match v with
        | Runtime.VString s -> s
        | Runtime.VVariant (_, "Ok", Runtime.VString s) -> s
        | other -> Runtime.value_to_string other
      in
      verify got)
    (List.rev !entries);
  (* Anti-hollow-test floor: the parity sweep must keep covering a healthy
     majority of the isolated example fixtures byte-for-byte. *)
  assert_true
    (Printf.sprintf
       "self canonicalizer parity floor: %d fixtures byte-identical (need >= 14), unsupported: %s"
       !canon_parity_ok
       (String.concat ", " (List.rev !canon_unsupported)))
    (!canon_parity_ok >= 14);
  if test_trace_enabled () then
    Printf.printf "self canonicalizer parity: %d byte-identical, %d explicit errors\n"
      !canon_parity_ok
      (List.length !canon_unsupported);
  print_endline "self-host frontend tests ok"
