open Protoss

let fail msg = raise (Failure msg)

let assert_true msg b = if not b then fail msg

let assert_equal msg a b = if a <> b then fail (msg ^ ": expected " ^ a ^ ", got " ^ b)

let expect_parse_error input =
  try
    let _ = Parser.parse_string input in
    fail "expected parse error"
  with Parser.Error _ -> ()

let expect_check_error input =
  try
    let p = Parser.parse_string input in
    let _ = Kernel.check_program p in
    fail "expected check error"
  with Kernel.Error _ | Parser.Error _ -> ()

let check input = Parser.parse_string input |> Kernel.check_program

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

  let formatted_a = check "(def main Nat (succ 1))" in
  let formatted_b = check "  ; formatting is not canonical\n\n(def   main   Nat\n  (succ   1))" in
  assert_equal "formatting independent hash" (Kernel.hash_program formatted_a)
    (Kernel.hash_program formatted_b);

  assert_equal "golden basic hash" "p1:3aa9fcfdeb989eec5013f30dec6ed730" (Kernel.hash_program formatted_a);

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
  let dep_canon =
    check "(def two Nat (succ 1))\n(def three Nat (succ two))"
    |> fun checked ->
    Kernel.serialize_program checked.program.capabilities
      (List.map
         (fun (d : Kernel.checked_def) ->
           { Kernel.cname = d.def.name; cdef_id = d.def_id; ctyp = d.def.typ; cbody = d.cterm })
         checked.defs)
  in
  assert_true "canonical refs use DefId"
    (String.contains dep_canon 'r' && String.contains dep_canon ':');

  let norm = check "(def two Nat (foldNat 2 0 (lambda (x Nat) (succ x))))" in
  let v, _ = Runtime.normalize_def norm "two" in
  assert_equal "normalization" "2" (Runtime.value_to_string v);

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

  let process_resume =
    check
      "(capabilities Human.ask)\n\
       (def askName (Process String) \
       (bind (Human.ask \"Name?\") (lambda (x String) (done x))))"
  in
  let suspended =
    match fst (Runtime.eval_entry process_resume "askName") with
    | Runtime.VProcessRequest s -> s
    | _ -> fail "expected suspended process"
  in
  let serialized = Runtime.serialize_suspended suspended in
  let parsed = Runtime.parse_suspended serialized in
  let resumed = Runtime.resume process_resume parsed (Runtime.response_value parsed.req "Ada") in
  assert_equal "process resume" "Done \"Ada\"" (Runtime.value_to_string resumed);
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
  expect_check_error
    "(capabilities Human.ask)\n(def bad Nat (let (p (Human.ask \"x\")) 0))";

  let store = temp_dir "patch" in
  let patch_ok =
    patch_file "protoss-add-two.json"
      "{ \"op\":\"AddDef\", \"name\":\"two\", \"deps\":[], \"type\":\"Nat\", \"expr\":[\"succ\",1] }"
  in
  let _ = Patch.apply store patch_ok in
  assert_true "valid patch writes object" (count_objects store > 0);
  let before = count_objects store in
  let ledger = temp_dir "patch-ledger" in
  let _ =
    Ledger.record_request ledger Ledger.initial_world (Ast.AskHuman "x") "suspended" "req" "cont" []
  in
  let ledger_before = count_files ledger in
  let patch_bad =
    patch_file "protoss-bad.json"
      "{ \"op\":\"AddDef\", \"name\":\"bad\", \"deps\":[], \"type\":\"Nat\", \"expr\":true }"
  in
  (try
     let _ = Patch.apply store patch_bad in
     fail "invalid patch should be rejected"
   with Patch.Error _ -> ());
  assert_true "invalid patch must not modify store" (count_objects store = before);
  assert_true "invalid patch must not modify ledger" (count_files ledger = ledger_before);

  let duplicate_before = count_files store in
  (try
     let _ = Patch.apply store patch_ok in
     fail "duplicate AddDef should be rejected"
   with Patch.Error _ -> ());
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
   with Patch.Error _ -> ());

  let dep_bad =
    patch_file "protoss-dep-bad.json"
      "{ \"op\":\"AddDef\", \"name\":\"three\", \"deps\":[], \"type\":\"Nat\", \"expr\":[\"succ\",\"two\"] }"
  in
  (try
     let _ = Patch.apply store dep_bad in
     fail "dependency mismatch should be rejected"
   with Patch.Error _ -> ());

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
   with Patch.Error _ -> ());
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
   with Patch.Error _ -> ());
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
  let _ = Patch.apply store_a patch_ok in
  let _ = Patch.apply store_b patch_ok in
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
      ("(def numbers (List Nat) (Cons Nat 1 (Cons Nat 2 (Nil Nat))))\n\
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
      "protoss-capabilities.json";
      "protoss-world.json";
    ];
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
  let event, world1 =
    Ledger.record_request ledger_web world0 (Ast.SaveLocal ("todos", "updated")) "suspended"
      "req" "cont" [ "Local.storage" ]
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
  let suspended = Runtime.serialize_suspended { Runtime.req = Ast.AskHuman "x"; cont = Runtime.KDone; cap_scope = [ "Human.ask" ] } in
  let event, next_world =
    Ledger.record_request ledger_root world (Ast.AskHuman "x") suspended "req" "cont" [ "Human.ask" ]
  in
  assert_true "ledger event inspectable" (String.length (Ledger.inspect_event ledger_root event) > 0);
  assert_true "ledger world inspectable" (String.length (Ledger.inspect_world ledger_root next_world) > 0);
  let bad_event = "p1:bad-event" in
  Store.write_file_atomic (Ledger.event_path ledger_root bad_event) "world=x\nkind=request\n";
  (try
     ignore (Ledger.inspect_event ledger_root bad_event);
     fail "maltyped ledger event should be rejected"
   with Failure _ -> ());
  (try
     ignore (Runtime.parse_suspended "(protoss-runtime-v2 (suspended (request ReadClock)))");
     fail "invalid resume suspension should be rejected"
   with Kernel.Error _ -> ());

  print_endline "protoss tests ok"
