type item = {
  line : int;
  section : string;
  text : string;
  checked : bool;
  evidence : bool;
}

type report = {
  checked_count : int;
  missing : item list;
}

let starts_with prefix s =
  let lp = String.length prefix and ls = String.length s in
  ls >= lp && String.sub s 0 lp = prefix

let trim = String.trim

let is_heading line = starts_with "## " line

let is_todo line = starts_with "- [x] " line || starts_with "- [ ] " line

let has_evidence_marker line =
  let line = String.lowercase_ascii line |> trim in
  List.exists
    (fun marker -> starts_with marker line)
    [
      "preuves:";
      "preuve:";
      "proof:";
      "proofs:";
      "preuves de section:";
      "preuve de section:";
    ]

let checked_text line = String.sub line 6 (String.length line - 6) |> trim

let split_lines text =
  let rec loop start acc =
    match String.index_from_opt text start '\n' with
    | Some i ->
        let line = String.sub text start (i - start) in
        loop (i + 1) (line :: acc)
    | None ->
        let line = String.sub text start (String.length text - start) in
        List.rev (line :: acc)
  in
  if String.equal text "" then [] else loop 0 []

let parse content =
  let lines = split_lines content in
  let finish_item section_has_evidence items current =
    match current with
    | None -> items
    | Some (line, section, line_text, checked, block_has_evidence) ->
        {
          line;
          section;
          text = line_text;
          checked;
          evidence = block_has_evidence || section_has_evidence;
        }
        :: items
  in
  let rec loop line_no section section_has_evidence items current = function
    | [] -> List.rev (finish_item section_has_evidence items current)
    | line :: rest ->
        let trimmed = trim line in
        if is_heading trimmed then
          let items = finish_item section_has_evidence items current in
          loop (line_no + 1) trimmed false items None rest
        else if is_todo trimmed then
          let items = finish_item section_has_evidence items current in
          let checked = starts_with "- [x] " trimmed in
          let text = checked_text trimmed in
          loop (line_no + 1) section section_has_evidence items
            (Some (line_no, section, text, checked, has_evidence_marker trimmed))
            rest
        else
          let section_has_evidence =
            section_has_evidence
            || starts_with "preuves de section:" (String.lowercase_ascii trimmed)
            || starts_with "preuve de section:" (String.lowercase_ascii trimmed)
          in
          let current =
            match current with
            | None -> None
            | Some (line, section, text, checked, block_has_evidence) ->
                Some (line, section, text, checked, block_has_evidence || has_evidence_marker trimmed)
          in
          loop (line_no + 1) section section_has_evidence items current rest
  in
  loop 1 "" false [] None lines

let report content =
  let items = parse content in
  let checked = List.filter (fun item -> item.checked) items in
  {
    checked_count = List.length checked;
    missing =
      checked
      |> List.filter (fun item -> not item.evidence)
      |> List.sort (fun a b -> Int.compare a.line b.line);
  }

let format_missing item =
  Printf.sprintf "line %d (%s): %s" item.line item.section item.text

let report_text report =
  match report.missing with
  | [] -> Printf.sprintf "Spec audit OK\nchecked=%d\n" report.checked_count
  | missing ->
      "Spec audit failed\nchecked=" ^ string_of_int report.checked_count
      ^ "\nmissing-evidence=\n"
      ^ String.concat "\n" (List.map format_missing missing)
      ^ "\n"

let check_file path =
  let report = report (Store.read_file path) in
  if report.missing <> [] then failwith (report_text report);
  report
