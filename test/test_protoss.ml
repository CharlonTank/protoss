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
  let graph_json = Canonical_ir.serialize_graph formatted_a in
  let graph = Json.parse graph_json in
  assert_equal "canonical graph version" Kernel.canonical_graph_version
    (json_string_field "version" graph);
  assert_equal "canonical graph program hash" (Kernel.hash_program formatted_a)
    (json_string_field "programHash" graph);
  assert_true "canonical graph has defs" (List.length (json_array_field "defs" graph) = 1);
  assert_true "canonical graph empty capability descriptors"
    (json_array_field "capabilityDescriptors" graph = []);
  let node_graph = json_field "nodeGraph" graph in
  assert_equal "canonical node graph version" Kernel.canonical_node_graph_version
    (json_string_field "version" node_graph);
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
  (try
     ignore (Canonical_ir.parse_graph (replace_once graph_json "\"value\": 1" "\"value\": 2"));
     fail "canonical graph typed node mismatch should be rejected"
   with Kernel.Error _ -> ());
  (try
     ignore
       (Canonical_ir.parse_graph
          (replace_once graph_json (Kernel.hash_program formatted_a) "p1:00000000000000000000000000000000"));
     fail "canonical graph program hash mismatch should be rejected"
   with Kernel.Error _ -> ());
  (try
     ignore (Canonical_ir.parse_graph (replace_once graph_json "\"kind\": \"Type\"" "\"kind\": \"Term\""));
     fail "canonical node graph mismatch should be rejected"
   with Kernel.Error _ -> ());
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
  assert_true "canonical graph omits bound names"
    (not (contains_substring (Canonical_ir.serialize_graph alpha_a) "\"x\"")
    && not (contains_substring (Canonical_ir.serialize_graph alpha_b) "\"y\""));
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
  assert_equal "parametric type alias transparent hash" (Kernel.hash_program maybe_expanded)
    (Kernel.hash_program maybe_alias);
  assert_equal "inferred variant constructor transparent hash" (Kernel.hash_program maybe_expanded)
    (Kernel.hash_program maybe_inferred_ctor);
  assert_equal "named variant transparent hash" (Kernel.hash_program maybe_expanded)
    (Kernel.hash_program maybe_variant_decl);
  let maybe_out, _ = Runtime.normalize_def maybe_alias "out" in
  assert_equal "parametric type alias runtime" "4" (Runtime.value_to_string maybe_out);
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
  assert_equal "defpoly type parameter alpha-stable hash" (Kernel.hash_program poly_a)
    (Kernel.hash_program poly_b);
  let poly_n, _ = Runtime.normalize_def poly_a "n" in
  assert_equal "defpoly Nat instantiation" "4" (Runtime.value_to_string poly_n);
  let poly_s, _ = Runtime.normalize_def poly_a "s" in
  assert_equal "defpoly String instantiation" "\"ok\"" (Runtime.value_to_string poly_s);
  let poly_out, _ = Runtime.normalize_def poly_a "out" in
  assert_equal "defpoly variant instantiation" "9" (Runtime.value_to_string poly_out);
  expect_check_error
    "(defpoly id (params A) (-> A A) (lambda (x A) x))\n\
     (def bad (-> Nat Nat) id)";
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
  let label, _ = Runtime.normalize_def stdlib_generics "label" in
  assert_equal "stdlib generic Maybe.map/default" "\"known\"" (Runtime.value_to_string label);
  let result_label, _ = Runtime.normalize_def stdlib_generics "resultLabel" in
  assert_equal "stdlib generic Result.map" "Ok \"ok\"" (Runtime.value_to_string result_label);
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
  assert_equal "process graph capability descriptor name" "Human.ask"
    (json_string_field "name" human_descriptor);
  let human_requests = json_array_field "requests" human_descriptor in
  assert_true "process graph capability request count" (List.length human_requests = 1);
  let ask_request = List.hd human_requests in
  assert_equal "process graph request tag" "AskHuman" (json_string_field "tag" ask_request);
  assert_equal "process graph response type" "String"
    (json_string_field "tag" (json_field "responseType" ask_request));

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
             "\"capabilityScope\": [\"Human.ask\"]" "\"capabilityScope\": [\"Clock.read\"]"));
     fail "canonical graph should reject corrupt capability scope"
   with Kernel.Error _ -> ());
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
  let _ = Patch.apply store patch_ok in
  assert_true "valid patch writes object" (count_objects store > 0);
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

  let unknown_cap_patch =
    patch_file "protoss-unknown-cap.json"
      "{ \"op\":\"AddDef\", \"name\":\"capBad\", \"deps\":[], \"capabilities\":[\"Space.laser\"], \
       \"type\":\"Nat\", \"expr\":0 }"
  in
  let unknown_cap_before = snapshot store in
  (try
     let _ = Patch.apply store unknown_cap_patch in
     fail "unknown capability patch should be rejected"
   with Patch.Error _ -> ());
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
	      ("(import \"math.protoss\")\n\
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
  let store_graph_path = Filename.concat build_a.store "program.graph.json" in
  assert_true "project store canonical graph" (Sys.file_exists store_graph_path);
  let store_graph = Json.parse (Store.read_file store_graph_path) in
  assert_equal "project store graph version" Kernel.canonical_graph_version
    (json_string_field "version" store_graph);
  assert_equal "project store graph hash" (Kernel.hash_program build_a.Workspace.checked)
    (json_string_field "programHash" store_graph);
  assert_equal "project store graph exact" (Kernel.checked_to_graph_json build_a.Workspace.checked)
    (Store.read_file store_graph_path);
  let capability_scope_file name =
    Filename.concat (Filename.concat build_a.store "capability-scopes") (name ^ ".capabilities")
  in
  assert_equal "project store process capability scope" "Human.ask"
    (String.trim (Store.read_file (capability_scope_file "askName")));
  assert_equal "project store pure capability scope" ""
    (String.trim (Store.read_file (capability_scope_file "appMain")));
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
  let graph_corrupt_root = temp_dir "workspace-graph-corrupt" in
  copy_tree ws_a graph_corrupt_root;
  let graph_corrupt_manifest = Workspace.parse_manifest graph_corrupt_root in
  let graph_corrupt_store = Workspace.store_root graph_corrupt_manifest in
  let graph_corrupt_path = Filename.concat graph_corrupt_store "program.graph.json" in
  write_file graph_corrupt_path
    (replace_once (Store.read_file graph_corrupt_path)
       (Kernel.hash_program build_a.Workspace.checked)
       "p1:00000000000000000000000000000000");
  (try
     ignore (Workspace.audit graph_corrupt_manifest);
     fail "audit should reject corrupt canonical graph"
   with Workspace.Error _ | Kernel.Error _ -> ());

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
  assert_equal "web capability descriptor name" "Local.storage"
    (json_string_field "name" local_storage_descriptor);
  assert_true "web capability request signatures"
    (List.length (json_array_field "requests" local_storage_descriptor) = 2);
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
