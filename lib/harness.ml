exception Error of string

let fail msg = raise (Error msg)

type t = {
  name : string;
  kind : kind;
}

and kind =
  | Example of string
  | Unit of string * string
  | Property of string * string option
  | Generator of string
  | Benchmark of string
  | Invariant of string * string
  | Migration of string * string
  | Scenario of string
  | Security of string * string
  | Diagnostic of string
  | AiEval of string * string

let format = "protoss-harness-v1"

let canonical_format = "protoss-harness-canonical-v1"

let has_prefix prefix s =
  let n = String.length prefix in
  String.length s >= n && String.sub s 0 n = prefix

let drop_prefix prefix s =
  if has_prefix prefix s then
    Some (String.sub s (String.length prefix) (String.length s - String.length prefix))
  else None

let split_once ch s =
  match String.index_opt s ch with
  | None -> None
  | Some i ->
      Some
        ( String.sub s 0 i,
          String.sub s (i + 1) (String.length s - i - 1) )

let split_once_string needle s =
  let needle_len = String.length needle in
  let rec loop i =
    if i + needle_len > String.length s then None
    else if String.sub s i needle_len = needle then
      Some
        ( String.sub s 0 i,
          String.sub s (i + needle_len) (String.length s - i - needle_len) )
    else loop (i + 1)
  in
  loop 0

let split_expected syntax line body =
  match split_once_string "==" body with
  | Some (entry, expected) when String.trim entry <> "" && String.trim expected <> "" ->
      (String.trim entry, String.trim expected)
  | _ -> fail (syntax ^ ": " ^ line)

let parse_entry prefix body constructor =
  match drop_prefix prefix body with
  | Some entry when String.trim entry <> "" -> Some (constructor (String.trim entry))
  | _ -> None

let parse_expected prefix syntax line body constructor =
  match drop_prefix prefix body with
  | Some rest ->
      let entry, expected = split_expected syntax line rest in
      Some (constructor entry expected)
  | None -> None

let parse_property line body =
  match drop_prefix "property " body with
  | None -> None
  | Some property_body -> (
      match split_once_string " with " property_body with
      | Some (entry, generator) when String.trim entry <> "" && String.trim generator <> "" ->
          Some (Property (String.trim entry, Some (String.trim generator)))
      | _ when String.trim property_body <> "" ->
          Some (Property (String.trim property_body, None))
      | _ ->
          fail
            ("property harness syntax is `harness name = property def [with generator]`: "
           ^ line))

let parse_kind line body =
  let alternatives =
    [
      parse_entry "example " body (fun entry -> Example entry);
      parse_expected "unit "
        "unit harness syntax is `harness name = unit def == expected`" line body
        (fun entry expected -> Unit (entry, expected));
      parse_property line body;
      parse_entry "generator " body (fun entry -> Generator entry);
      parse_entry "benchmark " body (fun entry -> Benchmark entry);
      parse_expected "invariant "
        "invariant harness syntax is `harness name = invariant def == expected`"
        line body
        (fun entry expected -> Invariant (entry, expected));
      parse_expected "migration "
        "migration harness syntax is `harness name = migration def == expected`"
        line body
        (fun entry expected -> Migration (entry, expected));
      parse_entry "scenario " body (fun entry -> Scenario entry);
      parse_expected "security "
        "security harness syntax is `harness name = security def == expected`"
        line body
        (fun entry expected -> Security (entry, expected));
      parse_entry "diagnostic " body (fun prompt -> Diagnostic prompt);
      parse_expected "ai-eval "
        "ai-eval harness syntax is `harness name = ai-eval def == expected`"
        line body
        (fun entry expected -> AiEval (entry, expected));
    ]
  in
  match List.find_map Fun.id alternatives with
  | Some kind -> kind
  | None -> fail ("unknown harness kind in declaration: " ^ line)

let parse_line line =
  let line = String.trim line in
  if line = "" || has_prefix "#" line then None
  else
    let rest =
      match drop_prefix "harness " line with
      | Some rest -> rest
      | None -> fail ("invalid harness declaration: " ^ line)
    in
    let name, body =
      match split_once '=' rest with
      | Some (name, body) when String.trim name <> "" -> (String.trim name, String.trim body)
      | _ -> fail ("harness declaration must be `harness name = ...`: " ^ line)
    in
    Some { name; kind = parse_kind line body }

let parse content =
  content |> String.split_on_char '\n' |> List.filter_map parse_line

let canonical_kind = function
  | Example entry ->
      "(kind \"example\") (entry " ^ Ast.quote entry ^ ")"
  | Unit (entry, expected) ->
      "(kind \"unit\") (entry " ^ Ast.quote entry ^ ") (expected "
      ^ Ast.quote expected ^ ")"
  | Property (entry, generator) ->
      "(kind \"property\") (entry " ^ Ast.quote entry ^ ")"
      ^
      (match generator with
      | None -> ""
      | Some generator -> " (generator " ^ Ast.quote generator ^ ")")
  | Generator entry ->
      "(kind \"generator\") (entry " ^ Ast.quote entry ^ ")"
  | Benchmark entry ->
      "(kind \"benchmark\") (entry " ^ Ast.quote entry ^ ")"
  | Invariant (entry, expected) ->
      "(kind \"invariant\") (entry " ^ Ast.quote entry ^ ") (expected "
      ^ Ast.quote expected ^ ")"
  | Migration (entry, expected) ->
      "(kind \"migration\") (entry " ^ Ast.quote entry ^ ") (expected "
      ^ Ast.quote expected ^ ")"
  | Scenario entry ->
      "(kind \"scenario\") (entry " ^ Ast.quote entry ^ ")"
  | Security (entry, expected) ->
      "(kind \"security\") (entry " ^ Ast.quote entry ^ ") (expected "
      ^ Ast.quote expected ^ ")"
  | Diagnostic prompt ->
      "(kind \"diagnostic\") (prompt " ^ Ast.quote prompt ^ ")"
  | AiEval (entry, expected) ->
      "(kind \"ai-eval\") (entry " ^ Ast.quote entry ^ ") (expected "
      ^ Ast.quote expected ^ ")"

let canonical harness =
  "(harness (name " ^ Ast.quote harness.name ^ ") " ^ canonical_kind harness.kind ^ ")"

let canonical_file harnesses =
  canonical_format ^ "\n" ^ String.concat "\n" (List.map canonical harnesses)

let canonical_bytes content = canonical_file (parse content)

let harness_id harness = Kernel.hash_string (canonical harness)

let file_ref content = Kernel.hash_string (canonical_bytes content)

let kind_name = function
  | Example _ -> "example"
  | Unit _ -> "unit"
  | Property _ -> "property"
  | Generator _ -> "generator"
  | Benchmark _ -> "benchmark"
  | Invariant _ -> "invariant"
  | Migration _ -> "migration"
  | Scenario _ -> "scenario"
  | Security _ -> "security"
  | Diagnostic _ -> "diagnostic"
  | AiEval _ -> "ai-eval"

let normalize_text checked entry =
  let value, _ = Runtime.normalize_def checked entry in
  (value, Runtime.value_to_string value)

let compare_entry checked entry expected =
  let _, actual = normalize_text checked entry in
  (String.equal actual expected, actual, expected, "")

let run_one checked harness =
  try
    let passed, actual, expected, diagnostic =
      match harness.kind with
      | Example entry | Generator entry | Benchmark entry | Scenario entry ->
          let _, actual = normalize_text checked entry in
          (true, actual, "", "")
      | Unit (entry, expected) | Invariant (entry, expected)
      | Migration (entry, expected) | Security (entry, expected)
      | AiEval (entry, expected) ->
          compare_entry checked entry expected
      | Property (entry, None) -> compare_entry checked entry "true"
      | Property (entry, Some generator) ->
          let property, _ = normalize_text checked entry in
          let sample, sample_text = normalize_text checked generator in
          let result = Runtime.apply checked property sample in
          let actual = Runtime.value_to_string result in
          ( String.equal actual "true",
            actual,
            "true",
            "generator=" ^ generator ^ " sample=" ^ sample_text )
      | Diagnostic prompt -> (true, prompt, "", "diagnostic prompt")
    in
    ( harness,
      passed,
      actual,
      expected,
      diagnostic )
  with
  | Kernel.Error msg | Failure msg -> (harness, false, "", "", msg)

let result_json (harness, passed, actual, expected, diagnostic) =
  Kernel.json_obj
    [
      Kernel.json_field "name" (Kernel.json_string harness.name);
      Kernel.json_field "harnessId" (Kernel.json_string (harness_id harness));
      Kernel.json_field "kind" (Kernel.json_string (kind_name harness.kind));
      Kernel.json_field "passed" (Kernel.json_bool passed);
      Kernel.json_field "actual" (Kernel.json_string actual);
      Kernel.json_field "expected" (Kernel.json_string expected);
      Kernel.json_field "diagnostic" (Kernel.json_string diagnostic);
    ]

let run_json checked ~source content =
  let harnesses = parse content in
  let results = List.map (run_one checked) harnesses in
  let passed = List.for_all (fun (_, ok, _, _, _) -> ok) results in
  Kernel.json_obj
    [
      Kernel.json_field "format" (Kernel.json_string format);
      Kernel.json_field "source" (Kernel.json_string source);
      Kernel.json_field "programHash" (Kernel.json_string (Kernel.hash_program checked));
      Kernel.json_field "status" (Kernel.json_string (if passed then "pass" else "fail"));
      Kernel.json_field "harnessCount" (string_of_int (List.length harnesses));
      Kernel.json_field "harnesses" (Kernel.json_array result_json results);
    ]
  ^ "\n"
