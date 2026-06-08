open Ast

exception Error of string

let fail msg = raise (Error msg)

let is_digit = function '0' .. '9' -> true | _ -> false

let has_line_col_prefix s =
  let len = String.length s in
  let rec digits i =
    if i < len && is_digit s.[i] then digits (i + 1) else i
  in
  let line_end = digits 0 in
  if line_end = 0 || line_end >= len || s.[line_end] <> ':' then false
  else
    let col_start = line_end + 1 in
    let col_end = digits col_start in
    col_end > col_start && col_end < len && s.[col_end] = ':'

let locate path msg =
  if has_line_col_prefix msg then path ^ ":" ^ msg else path ^ ":1:1: " ^ msg

let read_file path =
  let ic = open_in path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () ->
      let len = in_channel_length ic in
      really_input_string ic len)

let is_space = function ' ' | '\t' | '\r' | '\n' -> true | _ -> false

let is_delim c = is_space c || c = '(' || c = ')' || c = ';' || c = '"'

let def_keywords = [ "def"; "defcap"; "defpoly"; "defpolycap"; "defrec"; "defrecpoly" ]

let type_keywords = [ "type"; "alias"; "record"; "variant" ]

let is_named_keyword keywords keyword = List.exists (String.equal keyword) keywords

let skip_spaces source i =
  let len = String.length source in
  let rec loop i =
    if i < len && is_space source.[i] then loop (i + 1) else i
  in
  loop i

let atom_at source i =
  let len = String.length source in
  if i >= len || is_delim source.[i] then None
  else
    let rec loop j =
      if j < len && not (is_delim source.[j]) then loop (j + 1) else j
    in
    let j = loop i in
    Some (String.sub source i (j - i), j)

let skip_string source i =
  let len = String.length source in
  let rec loop j =
    if j >= len then len
    else
      match source.[j] with
      | '\\' when j + 1 < len -> loop (j + 2)
      | '"' -> j + 1
      | _ -> loop (j + 1)
  in
  loop (i + 1)

let skip_comment source i =
  let len = String.length source in
  let rec loop j =
    if j >= len || source.[j] = '\n' then j else loop (j + 1)
  in
  loop (i + 1)

let line_col source offset =
  let limit = min (max 0 offset) (String.length source) in
  let line = ref 1 and col = ref 1 in
  for i = 0 to limit - 1 do
    if source.[i] = '\n' then (
      incr line;
      col := 1)
    else incr col
  done;
  (!line, !col)

let find_named_form_offsets keywords source name =
  let len = String.length source in
  let rec loop depth i acc =
    if i >= len then List.rev acc
    else
      match source.[i] with
      | '"' -> loop depth (skip_string source i) acc
      | ';' -> loop depth (skip_comment source i) acc
      | '(' -> (
          let acc =
            if depth = 0 then
              let keyword_start = skip_spaces source (i + 1) in
              match atom_at source keyword_start with
              | Some (keyword, after_keyword) when is_named_keyword keywords keyword -> (
                  let name_start = skip_spaces source after_keyword in
                  match atom_at source name_start with
                  | Some (candidate, _) when String.equal candidate name -> i :: acc
                  | _ -> acc)
              | _ -> acc
            else acc
          in
          loop (depth + 1) (i + 1) acc)
      | ')' -> loop (max 0 (depth - 1)) (i + 1) acc
      | _ -> loop depth (i + 1) acc
  in
  loop 0 0 []

let find_named_form_offset keywords source name =
  match find_named_form_offsets keywords source name with
  | offset :: _ -> Some offset
  | [] -> None

let find_named_form_last_offset keywords source name =
  match List.rev (find_named_form_offsets keywords source name) with
  | offset :: _ -> Some offset
  | [] -> None

let find_def_offset source name = find_named_form_offset def_keywords source name

let find_type_alias_offset source name = find_named_form_offset type_keywords source name

let location_for_offset path source offset =
  let line, col = line_col source offset in
  path ^ ":" ^ string_of_int line ^ ":" ^ string_of_int col

let symbol_location path source name =
  match find_def_offset source name with
  | Some offset -> Some (location_for_offset path source offset)
  | None -> (
      match find_type_alias_offset source name with
      | Some offset -> Some (location_for_offset path source offset)
      | None -> None)

let symbol_location_last path source name =
  match find_named_form_last_offset def_keywords source name with
  | Some offset -> Some (location_for_offset path source offset)
  | None -> (
      match find_named_form_last_offset type_keywords source name with
      | Some offset -> Some (location_for_offset path source offset)
      | None -> None)

let def_locations path source defs =
  List.map
    (fun d ->
      let loc =
        match symbol_location path source d.name with Some loc -> loc | None -> path ^ ":1:1"
      in
      (d.name, loc))
    defs

let type_alias_locations path source aliases =
  List.map
    (fun a ->
      let loc =
        match symbol_location path source a.type_name with
        | Some loc -> loc
        | None -> path ^ ":1:1"
      in
      (a.type_name, loc))
    aliases

let has_prefix prefix s =
  let lp = String.length prefix in
  String.length s >= lp && String.sub s 0 lp = prefix

let drop_prefix prefix s =
  let lp = String.length prefix in
  if has_prefix prefix s then Some (String.sub s lp (String.length s - lp)) else None

let take_until_colon s =
  match String.index_opt s ':' with
  | Some i -> Some (String.sub s 0 i)
  | None -> None

let take_until_space s =
  let len = String.length s in
  let rec loop i =
    if i >= len || is_space s.[i] then i else loop (i + 1)
  in
  let i = loop 0 in
  if i = 0 then None else Some (String.sub s 0 i)

let split_words s =
  s |> String.split_on_char ' ' |> List.filter (fun w -> not (String.equal w ""))

let extract_duplicate_type_parameter_owner msg =
  match split_words msg with
  | [ "duplicate"; "type"; "parameter"; _param; "in"; ("alias" | "definition"); owner ] ->
      Some owner
  | _ -> None

let extract_error_symbol_name msg =
  match drop_prefix "definition " msg with
  | Some rest -> take_until_colon rest
  | None -> (
      match drop_prefix "duplicate definition: " msg with
      | Some name -> Some name
      | None -> (
          match drop_prefix "definition shadows builtin: " msg with
          | Some name -> Some name
          | None -> (
              match drop_prefix "type alias shadows builtin type: " msg with
              | Some name -> Some name
              | None -> (
                  match drop_prefix "duplicate type alias: " msg with
                  | Some name -> Some name
                  | None -> (
                      match
                        drop_prefix
                          "recursive type alias must be guarded by a Variant constructor: "
                          msg
                      with
                      | Some name -> Some name
                      | None -> (
                          match drop_prefix "cyclic type alias: " msg with
                          | Some rest -> take_until_space rest
                          | None -> (
                              match drop_prefix "type alias " msg with
                              | Some rest -> take_until_space rest
                              | None -> (
                                  match drop_prefix "unknown recursive type alias: " msg with
                                  | Some name -> Some name
                                  | None -> extract_duplicate_type_parameter_owner msg))))))))

let is_duplicate_symbol_error msg =
  has_prefix "duplicate definition: " msg || has_prefix "duplicate type alias: " msg

let locate_source_error path source msg =
  match extract_error_symbol_name msg with
  | Some name -> (
      let loc =
        if is_duplicate_symbol_error msg then symbol_location_last path source name
        else symbol_location path source name
      in
      match loc with Some loc -> loc ^ ": " ^ msg | None -> locate path msg)
  | None -> locate path msg

let locate_kernel_error locations fallback_path msg =
  match extract_error_symbol_name msg with
  | Some name -> (
      match List.find_opt (fun (n, _) -> String.equal n name) locations with
      | Some (_, loc) -> loc ^ ": " ^ msg
      | None -> locate fallback_path msg)
  | None -> locate fallback_path msg

let normalize_path base path =
  let raw = if Filename.is_relative path then Filename.concat base path else path in
  try Unix.realpath raw with Unix.Unix_error _ -> raw

let dirname path = Filename.dirname path

type loaded = {
  program : program;
  locations : (string * string) list;
  public_symbols : string list;
  all_symbols : string list;
}

let empty_loaded =
  {
    program =
      { imports = []; capabilities = []; module_name = None; exports = None; type_aliases = []; defs = [] };
    locations = [];
    public_symbols = [];
    all_symbols = [];
  }

let sort_uniq xs = List.sort_uniq String.compare xs

let local_symbols program =
  List.map (fun d -> d.name) program.defs
  @ List.map (fun a -> a.type_name) program.type_aliases

let public_symbols program =
  match program.exports with Some names -> names | None -> local_symbols program

let merge_loaded acc loaded import_path =
  {
    program =
      {
        imports = acc.program.imports @ loaded.program.imports @ [ import_path ];
        capabilities = acc.program.capabilities @ loaded.program.capabilities;
        module_name = None;
        exports = None;
        type_aliases = acc.program.type_aliases @ loaded.program.type_aliases;
        defs = acc.program.defs @ loaded.program.defs;
      };
    locations = acc.locations @ loaded.locations;
    public_symbols = acc.public_symbols;
    all_symbols = sort_uniq (acc.all_symbols @ loaded.all_symbols);
  }

let rec type_refs = function
  | TUnit | TBool | TNat | TString -> []
  | TFun (a, b) -> type_refs a @ type_refs b
  | TRecord fields | TVariant fields -> List.concat_map (fun (_, t) -> type_refs t) fields
  | TList t | TView t | TProcess t -> type_refs t
  | TVar _ -> []
  | TForall (_, t) -> type_refs t
  | TNamed (n, args) -> n :: List.concat_map type_refs args

let rec expr_type_refs = function
  | EUnit | EBool _ | ENat _ | EString _ | EName _ | ERequest _ | ENilInfer -> []
  | ELambda (_, t, body) -> type_refs t @ expr_type_refs body
  | ELambdaInfer (_, body) -> expr_type_refs body
  | EApp (f, x) -> expr_type_refs f @ expr_type_refs x
  | ELet (_, e, body) -> expr_type_refs e @ expr_type_refs body
  | ELetAnnot (_, t, e, body) -> type_refs t @ expr_type_refs e @ expr_type_refs body
  | ELetRecord (record, _, body) -> expr_type_refs record @ expr_type_refs body
  | ERecord fields -> List.concat_map (fun (_, e) -> expr_type_refs e) fields
  | EField (e, _) -> expr_type_refs e
  | EVariant (t, _, e) -> type_refs t @ expr_type_refs e
  | EVariantInferred (_, e) -> expr_type_refs e
  | EInst (_, args) -> List.concat_map type_refs args
  | ECase (e, branches) -> expr_type_refs e @ List.concat_map branch_type_refs branches
  | EFoldNat (n, z, step) -> expr_type_refs n @ expr_type_refs z @ expr_type_refs step
  | EFoldVariant (target, result, scrut, branches) ->
      type_refs target @ type_refs result @ expr_type_refs scrut
      @ List.concat_map branch_type_refs branches
  | ERecur e -> expr_type_refs e
  | ENil t -> type_refs t
  | ECons (t, head, tail) -> type_refs t @ expr_type_refs head @ expr_type_refs tail
  | EConsInfer (head, tail) -> expr_type_refs head @ expr_type_refs tail
  | EFoldList (xs, z, step) -> expr_type_refs xs @ expr_type_refs z @ expr_type_refs step
  | ECaseList (xs, nil_body, _, _, cons_body) ->
      expr_type_refs xs @ expr_type_refs nil_body @ expr_type_refs cons_body
  | EText e | EColumn e | ERow e | EDone e -> expr_type_refs e
  | EImage (src, alt) | EButton (src, alt) | EInput (src, alt) | EListView (src, alt)
  | EWhenView (src, alt) ->
      expr_type_refs src @ expr_type_refs alt
  | EBind (p, _, t, body) -> expr_type_refs p @ type_refs t @ expr_type_refs body
  | EBindInfer (p, _, body) -> expr_type_refs p @ expr_type_refs body

and branch_type_refs = function
  | BBool (_, e) -> expr_type_refs e
  | BVariant (_, _, e) -> expr_type_refs e
  | BVariantUnit (_, e) -> expr_type_refs e

let parsed_type_refs parsed =
  let alias_refs =
    parsed.type_aliases
    |> List.concat_map (fun a ->
           type_refs a.type_body
           |> List.filter (fun n -> not (List.exists (String.equal n) a.type_params)))
  in
  let def_refs =
    parsed.defs
    |> List.concat_map (fun (d : def) ->
           type_refs d.typ @ expr_type_refs d.body
           |> List.filter (fun n -> not (List.exists (String.equal n) d.type_params)))
  in
  sort_uniq (alias_refs @ def_refs)

let check_import_access path parsed imported direct_imports =
  let imported_all = direct_imports |> List.concat_map (fun (_, loaded) -> loaded.all_symbols) |> sort_uniq in
  let imported_public =
    direct_imports |> List.concat_map (fun (_, loaded) -> loaded.public_symbols) |> sort_uniq
  in
  let reject_private kind name =
    if List.exists (String.equal name) imported_all
       && not (List.exists (String.equal name) imported_public)
    then fail (locate path (kind ^ " is not exported by an import: " ^ name))
  in
  let all_defs = imported.program.defs @ parsed.defs in
  List.iter
    (fun d ->
      Kernel.dependencies_of_defs all_defs d.name |> List.iter (reject_private "definition"))
    parsed.defs;
  parsed_type_refs parsed |> List.iter (reject_private "type")

let rec load_file_with_locations ?(stack = []) path =
  let path = normalize_path (Sys.getcwd ()) path in
  if List.exists (String.equal path) stack then
    fail
      ("import cycle: "
      ^ String.concat " -> " (List.rev (path :: stack)));
  let source = try read_file path with Sys_error msg -> fail (locate path msg) in
  let parsed =
    try Parser.parse_string source with
    | Parser.Error msg -> fail (locate_source_error path source msg)
  in
  let base = dirname path in
  let imported, direct_imports =
    List.fold_left
      (fun (acc, direct) import_path ->
        let target = normalize_path base import_path in
        let imported = load_file_with_locations ~stack:(path :: stack) target in
        (merge_loaded acc imported import_path, (import_path, imported) :: direct))
      (empty_loaded, [])
      parsed.imports
  in
  check_import_access path parsed imported direct_imports;
  let program =
    {
      imports = imported.program.imports @ parsed.imports;
      capabilities = List.sort_uniq String.compare (imported.program.capabilities @ parsed.capabilities);
      module_name = parsed.module_name;
      exports = parsed.exports;
      type_aliases = imported.program.type_aliases @ parsed.type_aliases;
      defs = imported.program.defs @ parsed.defs;
    }
  in
  let local = local_symbols parsed in
  {
    program;
    locations =
      imported.locations @ def_locations path source parsed.defs
      @ type_alias_locations path source parsed.type_aliases;
    public_symbols = public_symbols parsed;
    all_symbols = sort_uniq (imported.all_symbols @ local);
  }

let load_file ?stack path = (load_file_with_locations ?stack path).program

let parse_file path = load_file path

let check_file path =
  let loaded = load_file_with_locations path in
  try Kernel.check_program loaded.program with
  | Kernel.Error msg -> fail (locate_kernel_error loaded.locations path msg)
