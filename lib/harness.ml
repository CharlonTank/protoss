exception Error of string

let fail msg = raise (Error msg)

type t = {
  name : string;
  kind : kind;
}

and kind =
  | Example of string
  | Unit of string * string

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
    let kind =
      match drop_prefix "example " body with
      | Some entry when String.trim entry <> "" -> Example (String.trim entry)
      | _ -> (
          match drop_prefix "unit " body with
          | Some unit_body -> (
              match split_once_string "==" unit_body with
              | Some (entry, expected)
                when String.trim entry <> "" && String.trim expected <> "" ->
                  Unit (String.trim entry, String.trim expected)
              | _ -> fail ("unit harness syntax is `harness name = unit def == expected`: " ^ line))
          | None -> fail ("unknown harness kind in declaration: " ^ line))
    in
    Some { name; kind }

let parse content =
  content |> String.split_on_char '\n' |> List.filter_map parse_line

let canonical_kind = function
  | Example entry ->
      "(kind \"example\") (entry " ^ Ast.quote entry ^ ")"
  | Unit (entry, expected) ->
      "(kind \"unit\") (entry " ^ Ast.quote entry ^ ") (expected "
      ^ Ast.quote expected ^ ")"

let canonical harness =
  "(harness (name " ^ Ast.quote harness.name ^ ") " ^ canonical_kind harness.kind ^ ")"

let canonical_file harnesses =
  canonical_format ^ "\n" ^ String.concat "\n" (List.map canonical harnesses)

let canonical_bytes content = canonical_file (parse content)

let harness_id harness = Kernel.hash_string (canonical harness)

let file_ref content = Kernel.hash_string (canonical_bytes content)

let run_one checked harness =
  let entry =
    match harness.kind with Example entry | Unit (entry, _) -> entry
  in
  try
    let value, _ = Runtime.normalize_def checked entry in
    let actual = Runtime.value_to_string value in
    let passed =
      match harness.kind with
      | Example _ -> true
      | Unit (_, expected) -> String.equal actual expected
    in
    ( harness,
      passed,
      actual,
      (match harness.kind with Example _ -> "" | Unit (_, expected) -> expected),
      "" )
  with
  | Kernel.Error msg | Failure msg -> (harness, false, "", "", msg)

let result_json (harness, passed, actual, expected, diagnostic) =
  Kernel.json_obj
    [
      Kernel.json_field "name" (Kernel.json_string harness.name);
      Kernel.json_field "harnessId" (Kernel.json_string (harness_id harness));
      Kernel.json_field "kind"
        (Kernel.json_string (match harness.kind with Example _ -> "example" | Unit _ -> "unit"));
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
