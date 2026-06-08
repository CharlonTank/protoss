open Ast

exception Error of string

let fail msg = raise (Error msg)

let locate path msg = path ^ ":1:1: " ^ msg

let read_file path =
  let ic = open_in path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () ->
      let len = in_channel_length ic in
      really_input_string ic len)

let is_space = function ' ' | '\t' | '\r' | '\n' -> true | _ -> false

let is_delim c = is_space c || c = '(' || c = ')' || c = ';' || c = '"'

let line_col source offset =
  let line = ref 1 and col = ref 1 in
  for i = 0 to max 0 (offset - 1) do
    if source.[i] = '\n' then (
      incr line;
      col := 1)
    else incr col
  done;
  (!line, !col)

let find_def_offset source name =
  let len = String.length source in
  let name_len = String.length name in
  let rec loop i =
    if i + 4 + name_len > len then None
    else if String.sub source i 4 = "(def" then
      let j = ref (i + 4) in
      while !j < len && is_space source.[!j] do
        incr j
      done;
      if !j + name_len <= len
         && String.sub source !j name_len = name
         && (!j + name_len = len || is_delim source.[!j + name_len])
      then Some i
      else loop (i + 1)
    else loop (i + 1)
  in
  loop 0

let def_locations path source defs =
  List.map
    (fun d ->
      let line, col =
        match find_def_offset source d.name with
        | Some offset -> line_col source offset
        | None -> (1, 1)
      in
      (d.name, path ^ ":" ^ string_of_int line ^ ":" ^ string_of_int col))
    defs

let has_prefix prefix s =
  let lp = String.length prefix in
  String.length s >= lp && String.sub s 0 lp = prefix

let take_until_colon s =
  match String.index_opt s ':' with
  | Some i -> Some (String.sub s 0 i)
  | None -> None

let extract_error_def_name msg =
  if has_prefix "definition " msg then
    let rest = String.sub msg 11 (String.length msg - 11) in
    take_until_colon rest
  else if has_prefix "duplicate definition: " msg then
    Some (String.sub msg 22 (String.length msg - 22))
  else if has_prefix "definition shadows builtin: " msg then
    Some (String.sub msg 28 (String.length msg - 28))
  else None

let locate_kernel_error locations fallback_path msg =
  match extract_error_def_name msg with
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
    | Parser.Error msg -> fail (locate path msg)
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
    locations = imported.locations @ def_locations path source parsed.defs;
    public_symbols = public_symbols parsed;
    all_symbols = sort_uniq (imported.all_symbols @ local);
  }

let load_file ?stack path = (load_file_with_locations ?stack path).program

let parse_file path = load_file path

let check_file path =
  let loaded = load_file_with_locations path in
  try Kernel.check_program loaded.program with
  | Kernel.Error msg -> fail (locate_kernel_error loaded.locations path msg)
