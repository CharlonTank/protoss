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

let contains_substring haystack needle =
  let lh = String.length haystack and ln = String.length needle in
  let rec loop i =
    i + ln <= lh && (String.sub haystack i ln = needle || loop (i + 1))
  in
  ln = 0 || loop 0

let check src = Parser.parse_string src |> Kernel.check_program

let expect_parse_error src =
  try
    ignore (Parser.parse_string src);
    fail "expected parse error"
  with Parser.Error _ -> ()

let expect_check_error src =
  try
    ignore (check src);
    fail "expected check error"
  with Parser.Error _ | Kernel.Error _ -> ()

let temp_dir name =
  let root =
    Filename.concat (Filename.get_temp_dir_name ())
      ("protoss-smoke-" ^ name ^ "-" ^ string_of_int (Unix.getpid ()))
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

let rec snapshot root =
  if not (Sys.file_exists root) then []
  else if Sys.is_directory root then
    Sys.readdir root |> Array.to_list |> List.sort String.compare
    |> List.concat_map (fun f -> snapshot (Filename.concat root f))
  else [ (root, Store.read_file root) ]

let () =
  ignore (Parser.parse_string "(def main Nat 1)");
  expect_parse_error "(def main Nat 1";

  ignore (check "(def main Nat (succ 1))");
  expect_check_error "(def bad Nat true)";

  let alpha_a = check "(def id (-> Nat Nat) (lambda (x Nat) x))" in
  let alpha_b = check "(def id (-> Nat Nat) (lambda (y Nat) y))" in
  assert_equal "alpha-equivalent hashes" (Kernel.hash_program alpha_a) (Kernel.hash_program alpha_b);

  let norm = check "(def two Nat (succ 1))" in
  let value, _ = Runtime.normalize_def norm "two" in
  assert_equal "normalization" "2" (Runtime.value_to_string value);
  assert_equal "deterministic hash" (Kernel.hash_program norm) (Kernel.hash_program norm);
  assert_true "different hash"
    (Kernel.hash_program norm <> Kernel.hash_program (check "(def three Nat (succ 2))"));

  let memo =
    check
      "(def inc (-> Nat Nat) (lambda (x Nat) (succ x)))\n\
       (def b Nat (let (x (inc 41)) (let (y (inc 41)) y)))"
  in
  let _, trace = Runtime.eval_entry ~trace_cache:true memo "b" in
  assert_true "memo cache hit"
    (List.exists
       (fun line -> String.length line >= 9 && String.sub line 0 9 = "cache hit")
       trace);

  expect_check_error "(def loop Nat loop)";

  let process = check "(capabilities Human.ask)\n(def askName (Process String) (Human.ask \"Name?\"))" in
  let pv, _ = Runtime.eval_entry process "askName" in
  assert_true "process suspends"
    (match pv with Runtime.VProcessRequest { Runtime.req = Ast.AskHuman "Name?"; _ } -> true | _ -> false);
  expect_check_error "(def askName (Process String) (Human.ask \"Name?\"))";

  let pt_workspace = temp_dir "pt-workspace" in
  let pt_src = Filename.concat pt_workspace "src" in
  Store.ensure_dir pt_src;
  write_file (Filename.concat pt_workspace "protoss.toml")
    "name = \"pt-workspace\"\n\
     version = \"0.1.0\"\n\
     entrypoints = [\"src/main.pt\"]\n\
     stdlib = \"none\"\n\
     source_dirs = [\"src\"]\n\
     store_dir = \".protoss/store\"\n\
     cache_dir = \".protoss/cache\"\n\
     capabilities = []\n";
  write_file (Filename.concat pt_src "math.pt") "(def base Nat 2)\n";
  write_file (Filename.concat pt_src "main.pt") "(import \"math.pt\")\n(def main Nat (succ base))\n";
  let pt_build = Workspace.build (Workspace.parse_manifest pt_workspace) in
  let pt_main, _ = Runtime.normalize_def pt_build.Workspace.checked "main" in
  assert_equal ".pt workspace source discovery" "3" (Runtime.value_to_string pt_main);

  let store = temp_dir "patch" in
  let valid_patch = Filename.concat (Filename.get_temp_dir_name ())
      (string_of_int (Unix.getpid ()) ^ "-protoss-smoke-valid-patch.json") in
  write_file valid_patch
    "{ \"op\":\"AddDef\", \"name\":\"two\", \"deps\":[], \"type\":\"Nat\", \"expr\":[\"succ\",1] }";
  ignore (Patch.apply store valid_patch);
  let patched = Store.load_program store |> Kernel.check_program in
  let two, _ = Runtime.normalize_def patched "two" in
  assert_equal "patch valid accepted" "2" (Runtime.value_to_string two);

  let before = snapshot store in
  let invalid_patch = Filename.concat (Filename.get_temp_dir_name ())
      (string_of_int (Unix.getpid ()) ^ "-protoss-smoke-invalid-patch.json") in
  write_file invalid_patch
    "{ \"op\":\"AddDef\", \"name\":\"bad\", \"deps\":[], \"type\":\"Nat\", \"expr\":true }";
  (try
     ignore (Patch.apply store invalid_patch);
     fail "invalid patch should be rejected"
   with Patch.Error msg ->
     assert_true "invalid patch reports context" (contains_substring msg "AddDef bad"));
  assert_true "invalid patch rollback" (snapshot store = before);

  print_endline "protoss smoke tests ok"
