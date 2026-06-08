let usage () =
  prerr_endline
    "usage: protoss parse|check|nf|hash <file> | protoss check|nf|hash --graph <graph.json>\n\
     \       protoss canon <file> | protoss canon --graph <file> | protoss canon --from-graph <graph.json>\n\
     \       protoss eval <file> --entry <name> [--trace-cache] [--cache <dir>]\n\
     \       protoss eval --graph <graph.json> --entry <name> [--trace-cache] [--cache <dir>]\n\
     \       protoss run <file> --entry <name> [--ledger <root>]\n\
     \       protoss run --graph <graph.json> --entry <name> [--ledger <root>]\n\
     \       protoss resume <file> --entry <name> --event <event> --response <value> [--ledger <root>]\n\
     \       protoss resume --graph <graph.json> --entry <name> --event <event> --response <value> [--ledger <root>]\n\
     \       protoss world init [<ledger-root>]\n\
     \       protoss ledger event|world|inspect|replay|diff|export|import|branches [args]\n\
     \       protoss app check <project>\n\
     \       protoss web build|serve|inspect <project> [--out <dir>] [--port <n>]\n\
     \       protoss project init|check|build|lock|package [project] [--stats|--locked|--check]\n\
     \       protoss build [project] [--target web] [--stats] [--locked]\n\
     \       protoss patch check|apply <store> <patch.json>\n\
     \       protoss patch from-diff <store-a> <store-b>\n\
     \       protoss diff [--json] <store-a> <store-b>\n\
     \       protoss audit [project]\n\
     \       protoss invariants file <file> | graph <graph.json> | alpha <file-a> <file-b>\n\
     \       protoss invariants process <file> --entry <name> --response <value>\n\
     \       protoss invariants process --graph <graph.json> --entry <name> --response <value>\n\
     \       protoss invariants ledger <file> --entry <name> --response <value> [--ledger <root>]\n\
     \       protoss invariants ledger --graph <graph.json> --entry <name> --response <value> [--ledger <root>]\n\
     \       protoss invariants package <project>\n\
     \       protoss fmt [--check] <file>\n\
     \       protoss graph <project> --out <graph.json> | --dot <graph.dot>\n\
     \       protoss repl\n\
     \       protoss explain <error-code>\n\
     \       protoss bench build <project>\n\
     \       protoss cache stats <dir>\n\
     \       protoss store list|get|deps|roots|stats [args]";
  exit 2

let parse_and_check file =
  Protoss.Loader.check_file file

let find_entry args =
  let rec loop = function
    | "--entry" :: name :: rest -> (name, rest)
    | _ :: rest -> loop rest
    | [] -> usage ()
  in
  loop args

let has_flag flag args = List.exists (String.equal flag) args

let find_arg flag args =
  let rec loop = function
    | x :: value :: _ when String.equal x flag -> Some value
    | _ :: rest -> loop rest
    | [] -> None
  in
  loop args

let required_arg flag args = match find_arg flag args with Some v -> v | None -> usage ()

let print_error kind msg =
  prerr_endline (kind ^ ": " ^ msg);
  exit 1

let protect f =
  try f () with
  | Protoss.Parser.Error msg -> print_error "parse error" msg
  | Protoss.Loader.Error msg -> print_error "load error" msg
  | Protoss.Kernel.Error msg -> print_error "check error" msg
  | Protoss.Patch.Error msg -> print_error "patch error" msg
  | Protoss.Store.Error msg -> print_error "store error" msg
  | Protoss.Workspace.Error msg -> print_error "workspace error" msg
  | Protoss.Web.Error msg -> print_error "web error" msg
  | Unix.Unix_error (err, fn, arg) ->
      print_error "system error" (fn ^ "(" ^ arg ^ "): " ^ Unix.error_message err)
  | Failure msg -> print_error "error" msg
  | Sys_error msg -> print_error "system error" msg

let command_parse file =
  let p = Protoss.Parser.parse_file file in
  Printf.printf "Program: %d definitions\n" (List.length p.Protoss.Ast.defs);
  (match p.module_name with Some name -> Printf.printf "Module: %s\n" name | None -> ());
  (match p.exports with
  | Some exports -> Printf.printf "Exports: %s\n" (String.concat ", " exports)
  | None -> ());
  if p.capabilities <> [] then
    Printf.printf "Capabilities: %s\n" (String.concat ", " p.capabilities);
  List.iter
    (fun a ->
      Printf.printf "type %s = %s\n" a.Protoss.Ast.type_name
        (Protoss.Ast.string_of_typ a.type_body))
    p.type_aliases;
  List.iter
    (fun d ->
      Printf.printf "%s : %s\n" d.Protoss.Ast.name (Protoss.Ast.string_of_typ d.typ))
    p.defs

let command_check file =
  let checked = parse_and_check file in
  Printf.printf "OK: %d definitions\n" (List.length checked.Protoss.Kernel.defs)

let checked_graph file =
  Protoss.Canonical_ir.checked_of_graph (Protoss.Store.read_file file)

let command_check_graph file =
  let checked = checked_graph file in
  Printf.printf "Graph OK: %d definitions\n" (List.length checked.Protoss.Kernel.defs)

let command_nf file =
  let checked = parse_and_check file in
  Protoss.Runtime.normalize_all checked
  |> List.iter (fun (name, value) ->
         Printf.printf "%s = %s\n" name (Protoss.Runtime.value_to_string value))

let command_nf_graph file =
  let checked = checked_graph file in
  Protoss.Runtime.normalize_all checked
  |> List.iter (fun (name, value) ->
         Printf.printf "%s = %s\n" name (Protoss.Runtime.value_to_string value))

let command_hash file =
  let checked = parse_and_check file in
  print_endline (Protoss.Kernel.hash_program checked)

let command_hash_graph file =
  let checked = checked_graph file in
  print_endline (Protoss.Kernel.hash_program checked)

let canonical_program checked =
  Protoss.Kernel.serialize_program checked.Protoss.Kernel.program.capabilities
    (List.map
       (fun (d : Protoss.Kernel.checked_def) ->
         {
           Protoss.Kernel.cname = d.def.name;
           cdef_id = d.def_id;
           ctyp = d.def.typ;
           cbody = d.cterm;
         })
       checked.defs)

let command_canon file =
  let checked = parse_and_check file in
  print_endline (canonical_program checked)

let command_canon_graph file =
  let checked = parse_and_check file in
  print_string (Protoss.Canonical_ir.serialize_graph checked)

let command_canon_from_graph file =
  print_endline (Protoss.Canonical_ir.graph_to_program (Protoss.Store.read_file file))

let command_eval file args =
  let entry, rest = find_entry args in
  let checked = parse_and_check file in
  let value, trace =
    Protoss.Runtime.eval_entry ~trace_cache:(has_flag "--trace-cache" rest)
      ?cache_dir:(find_arg "--cache" rest) checked entry
  in
  List.iter print_endline trace;
  Printf.printf "%s = %s\n" entry (Protoss.Runtime.value_to_string value)

let command_eval_graph file args =
  let entry, rest = find_entry args in
  let checked = checked_graph file in
  let value, trace =
    Protoss.Runtime.eval_entry ~trace_cache:(has_flag "--trace-cache" rest)
      ?cache_dir:(find_arg "--cache" rest) checked entry
  in
  List.iter print_endline trace;
  Printf.printf "%s = %s\n" entry (Protoss.Runtime.value_to_string value)

let run_checked checked args =
  let entry, _ = find_entry args in
  let value, _ = Protoss.Runtime.eval_entry checked entry in
  match value with
  | Protoss.Runtime.VProcessDone v ->
      Printf.printf "Done %s\n" (Protoss.Runtime.value_to_string v)
  | Protoss.Runtime.VProcessRequest s ->
      let root = Option.value (find_arg "--ledger" args) ~default:(Filename.concat "target" "ledger") in
      let event, next_world =
        Protoss.Ledger.record_request root Protoss.Ledger.initial_world s.req
          (Protoss.Runtime.serialize_suspended s)
          (Protoss.Runtime.request_id s) (Protoss.Runtime.continuation_id s) s.cap_scope
      in
      Printf.printf "WorldRef %s\n" Protoss.Ledger.initial_world;
      Printf.printf "RequestId %s\n" (Protoss.Runtime.request_id s);
      Printf.printf "Request %s\n" (Protoss.Kernel.req_to_canonical s.req);
      Printf.printf "ContinuationId %s\n" (Protoss.Runtime.continuation_id s);
      Printf.printf "CapScope %s\n" (String.concat "," s.cap_scope);
      Printf.printf "Event %s\n" event;
      Printf.printf "NextWorldRef %s\n" next_world
  | other ->
      Protoss.Kernel.fail ("run entry is not a Process: " ^ Protoss.Runtime.value_to_string other)

let command_run file args =
  run_checked (parse_and_check file) args

let command_run_graph file args =
  run_checked (checked_graph file) args

let resume_checked checked args =
  let _entry, _ = find_entry args in
  let event = required_arg "--event" args in
  let response = required_arg "--response" args in
  let root = Option.value (find_arg "--ledger" args) ~default:(Filename.concat "target" "ledger") in
  let fields = Protoss.Ledger.event_fields root event in
  let previous_world =
    match Protoss.Ledger.field "world" fields with
    | Some w -> w
    | None -> Protoss.Kernel.fail ("event has no WorldRef: " ^ event)
  in
  let current_world = Protoss.Ledger.next_world previous_world event in
  let suspended = Protoss.Ledger.event_suspended root event |> Protoss.Runtime.parse_suspended in
  let response_value = Protoss.Runtime.response_value suspended.req response in
  let result = Protoss.Runtime.resume checked suspended response_value in
  let result_text = Protoss.Runtime.value_to_string result in
  let resume_event, next_world =
    Protoss.Ledger.record_resume root current_world event response result_text
  in
  Printf.printf "WorldRef %s\n" current_world;
  Printf.printf "ResumeEvent %s\n" resume_event;
  Printf.printf "NextWorldRef %s\n" next_world;
  match result with
  | Protoss.Runtime.VProcessDone v ->
      Printf.printf "Done %s\n" (Protoss.Runtime.value_to_string v)
  | Protoss.Runtime.VProcessRequest s ->
      Printf.printf "Request %s\n" (Protoss.Kernel.req_to_canonical s.req);
      Printf.printf "Suspended %s\n" (Protoss.Runtime.serialize_suspended s)
  | other ->
      Printf.printf "Value %s\n" (Protoss.Runtime.value_to_string other)

let command_resume file args =
  resume_checked (parse_and_check file) args

let command_resume_graph file args =
  resume_checked (checked_graph file) args

let default_ledger = Filename.concat "target" "ledger"

let command_world = function
  | [ "init" ] -> print_endline (Protoss.Ledger.init default_ledger)
  | [ "init"; root ] -> print_endline (Protoss.Ledger.init root)
  | _ -> usage ()

let command_ledger = function
  | [ "event"; event ] -> print_string (Protoss.Ledger.inspect_event default_ledger event)
  | [ "event"; root; event ] -> print_string (Protoss.Ledger.inspect_event root event)
  | [ "world"; world ] -> print_string (Protoss.Ledger.inspect_world default_ledger world)
  | [ "world"; root; world ] -> print_string (Protoss.Ledger.inspect_world root world)
  | [ "inspect"; ref_ ] -> print_string (Protoss.Ledger.inspect default_ledger ref_)
  | [ "inspect"; root; ref_ ] -> print_string (Protoss.Ledger.inspect root ref_)
  | [ "replay"; world ] -> print_string (Protoss.Ledger.replay default_ledger world)
  | [ "replay"; root; world ] -> print_string (Protoss.Ledger.replay root world)
  | [ "diff"; world_a; world_b ] -> print_string (Protoss.Ledger.diff default_ledger world_a world_b)
  | [ "diff"; root; world_a; world_b ] -> print_string (Protoss.Ledger.diff root world_a world_b)
  | [ "export"; world ] -> print_string (Protoss.Ledger.export default_ledger world)
  | [ "export"; root; world ] -> print_string (Protoss.Ledger.export root world)
  | [ "import"; file ] ->
      print_endline (Protoss.Ledger.import default_ledger (Protoss.Store.read_file file))
  | [ "import"; root; file ] ->
      print_endline (Protoss.Ledger.import root (Protoss.Store.read_file file))
  | [ "branches" ] -> print_string (Protoss.Ledger.branches default_ledger)
  | [ "branches"; root ] -> print_string (Protoss.Ledger.branches root)
  | _ -> usage ()

let command_patch = function
  | "from-diff" :: store_a :: store_b :: [] ->
      print_string
        (Protoss.Workspace.patch_from_diff
           (Protoss.Workspace.store_of_arg store_a)
           (Protoss.Workspace.store_of_arg store_b))
  | "check" :: store :: patch :: [] ->
      let checked = Protoss.Patch.check store patch in
      Printf.printf "Patch valid %s\n" (Protoss.Patch.describe_checked checked)
  | "apply" :: store :: patch :: [] ->
      let hash = Protoss.Patch.apply store patch in
      Printf.printf "Patch accepted %s\n" hash
  | _ -> usage ()

let command_store = function
  | [ "list" ] ->
      print_string (Protoss.Workspace.list_store (Protoss.Workspace.project_store_of_cwd ()))
  | [ "list"; project_or_store ] ->
      print_string
        (Protoss.Workspace.list_store (Protoss.Workspace.store_of_arg project_or_store))
  | [ "get"; id ] ->
      print_string (Protoss.Workspace.get_store (Protoss.Workspace.project_store_of_cwd ()) id)
  | [ "get"; root; hash ] ->
      if Sys.file_exists (Filename.concat (Protoss.Workspace.project_root root) "protoss.toml") then
        print_string (Protoss.Workspace.get_store (Protoss.Workspace.store_of_arg root) hash)
      else print_string (Protoss.Store.get_object root hash)
  | [ "deps"; name ] ->
      print_endline
        (String.concat "\n"
           (Protoss.Workspace.read_deps (Protoss.Workspace.project_store_of_cwd ()) name))
  | [ "deps"; project_or_store; name ] ->
      print_endline
        (String.concat "\n"
           (Protoss.Workspace.read_deps (Protoss.Workspace.store_of_arg project_or_store) name))
  | [ "roots" ] -> print_string (Protoss.Workspace.roots_store (Protoss.Workspace.project_store_of_cwd ()))
  | [ "roots"; project_or_store ] ->
      print_string (Protoss.Workspace.roots_store (Protoss.Workspace.store_of_arg project_or_store))
  | [ "stats"; root ] ->
      let objects, defs, canonical = Protoss.Store.stats root in
      Printf.printf "objects=%d\ndefs=%d\ncanonical=%d\n" objects defs canonical
  | _ -> usage ()

let command_cache = function
  | [ "stats"; root ] ->
      let hits, misses, entries = Protoss.Runtime.persistent_cache_stats root in
      Printf.printf "hits=%d\nmisses=%d\nentries=%d\n" hits misses entries
  | _ -> usage ()

let is_flag s = String.length s > 0 && s.[0] = '-'

let project_arg args =
  match List.filter (fun s -> not (is_flag s)) args with
  | [] -> "."
  | [ path ] -> path
  | _ -> usage ()

let find_flag_value flag args = find_arg flag args

let parse_build_args args =
  let rec loop target stats locked paths = function
    | [] -> (List.rev paths, target, stats, locked)
    | "--stats" :: rest -> loop target true locked paths rest
    | "--locked" :: rest -> loop target stats true paths rest
    | "--target" :: value :: rest -> loop (Some value) stats locked paths rest
    | x :: _rest when is_flag x -> usage ()
    | x :: rest -> loop target stats locked (x :: paths) rest
  in
  loop None false false [] args

let command_project_build args =
  let paths, target, stats, locked = parse_build_args args in
  let root = match paths with [] -> "." | [ path ] -> path | _ -> usage () in
  let manifest = Protoss.Workspace.parse_manifest (Protoss.Workspace.project_root root) in
  let result =
    match target with
    | Some "web" ->
        if locked then ignore (Protoss.Workspace.check_lock manifest);
        (Protoss.Web.build root).Protoss.Web.build
    | Some other -> Protoss.Workspace.fail ("unknown build target: " ^ other)
    | None ->
        if locked then Protoss.Workspace.build_locked manifest
        else Protoss.Workspace.build manifest
  in
  Printf.printf "Build %s\nStore %s\n" result.Protoss.Workspace.build_id result.store;
  if stats then print_string (Protoss.Workspace.stats_to_string result.stats)

let command_project_lock args =
  let root = project_arg args in
  let manifest = Protoss.Workspace.parse_manifest (Protoss.Workspace.project_root root) in
  if has_flag "--check" args then
    let hash = Protoss.Workspace.check_lock manifest in
    Printf.printf "Lock OK %s\nPath %s\n" hash (Protoss.Workspace.lock_path manifest)
  else
    let path, hash = Protoss.Workspace.write_lock manifest in
    Printf.printf "Lock %s\nPath %s\n" hash path

let command_project_package args =
  let root = project_arg args in
  let manifest = Protoss.Workspace.parse_manifest (Protoss.Workspace.project_root root) in
  if has_flag "--check" args then
    let result = Protoss.Workspace.check_package manifest in
    Printf.printf "Package OK %s\nPath %s\nLock %s\nBuild %s\nStore %s\n"
      result.Protoss.Workspace.package_ref result.package_path result.lock_hash result.build_id
      result.store
  else
    let result = Protoss.Workspace.write_package ~locked:(has_flag "--locked" args) manifest in
    Printf.printf "Package %s\nPath %s\nLock %s\nBuild %s\nStore %s\n"
      result.Protoss.Workspace.package_ref result.package_path result.lock_hash result.build_id
      result.store

let command_project = function
  | "init" :: args ->
      let root = project_arg args in
      let path = Protoss.Workspace.init root in
      Printf.printf "Initialized %s\n" path
  | "check" :: args ->
      let root = project_arg args in
      let manifest = Protoss.Workspace.parse_manifest (Protoss.Workspace.project_root root) in
      Protoss.Workspace.check_project manifest;
      Printf.printf "Project OK %s\n" manifest.name
  | "build" :: args -> command_project_build args
  | "lock" :: args -> command_project_lock args
  | "package" :: args -> command_project_package args
  | _ -> usage ()

let command_app = function
  | [ "check"; project ] ->
      let contract = Protoss.Web.app_check project in
      Printf.printf "App OK model=%s msg=%s\n"
        (Protoss.Ast.string_of_typ contract.Protoss.Web.model_ty)
        (Protoss.Ast.string_of_typ contract.msg_ty)
  | _ -> usage ()

let command_web = function
  | "build" :: project :: args ->
      let out = find_flag_value "--out" args in
      let result = Protoss.Web.build ?out project in
      Printf.printf "Web build %s\nOut %s\n" result.Protoss.Web.build.build_id result.out_dir
  | "inspect" :: project :: [] -> print_string (Protoss.Web.inspect project)
  | "serve" :: project :: args ->
      let port =
        match find_flag_value "--port" args with Some p -> int_of_string p | None -> 8080
      in
      Protoss.Web.serve ~port project
  | _ -> usage ()

let command_diff = function
  | [ "--json"; a; b ] ->
      print_string
        (Protoss.Workspace.diff_to_json
           (Protoss.Workspace.diff (Protoss.Workspace.store_of_arg a) (Protoss.Workspace.store_of_arg b)))
  | [ a; b ] ->
      print_string
        (Protoss.Workspace.diff_to_text
           (Protoss.Workspace.diff (Protoss.Workspace.store_of_arg a) (Protoss.Workspace.store_of_arg b)))
  | _ -> usage ()

let command_audit = function
  | [] ->
      let manifest = Protoss.Workspace.parse_manifest (Sys.getcwd ()) in
      print_string (Protoss.Workspace.audit manifest)
  | [ project ] ->
      let manifest = Protoss.Workspace.parse_manifest (Protoss.Workspace.project_root project) in
      print_string (Protoss.Workspace.audit manifest)
  | _ -> usage ()

let command_invariants = function
  | [ "file"; file ] ->
      print_string (Protoss.Invariants.describe_file (Protoss.Invariants.check_file file))
  | [ "graph"; file ] ->
      print_string (Protoss.Invariants.describe_file (Protoss.Invariants.check_graph file))
  | [ "alpha"; left; right ] ->
      print_string
        (Protoss.Invariants.describe_alpha (Protoss.Invariants.check_alpha left right))
  | "process" :: "--graph" :: file :: args ->
      let entry = fst (find_entry args) in
      let response = required_arg "--response" args in
      print_string
        (Protoss.Invariants.describe_process
           (Protoss.Invariants.check_graph_process file entry response))
  | "process" :: file :: args ->
      let entry = fst (find_entry args) in
      let response = required_arg "--response" args in
      print_string
        (Protoss.Invariants.describe_process
           (Protoss.Invariants.check_process file entry response))
  | "ledger" :: "--graph" :: file :: args ->
      let entry = fst (find_entry args) in
      let response = required_arg "--response" args in
      print_string
        (Protoss.Invariants.describe_ledger
           (Protoss.Invariants.check_graph_ledger_process ?ledger:(find_arg "--ledger" args)
              file entry response))
  | "ledger" :: file :: args ->
      let entry = fst (find_entry args) in
      let response = required_arg "--response" args in
      print_string
        (Protoss.Invariants.describe_ledger
           (Protoss.Invariants.check_ledger_process ?ledger:(find_arg "--ledger" args) file
              entry response))
  | [ "package"; project ] ->
      print_string
        (Protoss.Invariants.describe_package (Protoss.Invariants.check_package project))
  | _ -> usage ()

let command_fmt = function
  | [ file ] -> print_string (Protoss.Ast.string_of_program (Protoss.Parser.parse_file file))
  | [ "--check"; file ] ->
      let original = Protoss.Store.read_file file in
      let formatted = Protoss.Ast.string_of_program (Protoss.Parser.parse_file file) in
      if String.equal original formatted then print_endline "OK"
      else Protoss.Kernel.fail ("format check failed: " ^ file)
  | _ -> usage ()

let command_graph = function
  | [ project; "--out"; out ] ->
      let manifest = Protoss.Workspace.parse_manifest (Protoss.Workspace.project_root project) in
      let build = Protoss.Workspace.build manifest in
      Protoss.Store.write_file_atomic out (Protoss.Web.stored_graph_json build.store);
      Printf.printf "Wrote %s\n" out
  | [ project; "--dot"; out ] ->
      let manifest = Protoss.Workspace.parse_manifest (Protoss.Workspace.project_root project) in
      let build = Protoss.Workspace.build manifest in
      Protoss.Store.write_file_atomic out (Protoss.Web.stored_graph_dot build.store);
      Printf.printf "Wrote %s\n" out
  | _ -> usage ()

let command_repl () =
  print_endline "Protoss REPL. Enter a single expression or EOF.";
  try
    while true do
      print_string "protoss> ";
      flush stdout;
      let line = read_line () in
      if String.trim line <> "" then
        let program =
          Protoss.Parser.parse_string ("(def it Nat " ^ line ^ ")")
          |> Protoss.Kernel.check_program
        in
        let value, _ = Protoss.Runtime.eval_entry program "it" in
        print_endline (Protoss.Runtime.value_to_string value)
    done
  with End_of_file -> ()

let command_explain = function
  | [ code ] ->
      let msg =
        match code with
        | "WEB001" -> "Missing init/update/view definition in a web app."
        | "WEB007" -> "view returns a View whose message type does not match update."
        | "PATCH_DEPS" -> "Patch deps must exactly match canonical definition dependencies."
        | "CAPABILITY" -> "Effects require explicit capabilities in the project or source."
        | _ -> "Unknown error code."
      in
      print_endline msg
  | _ -> usage ()

let command_bench = function
  | [ "build"; project ] ->
      let start = Unix.gettimeofday () in
      let manifest = Protoss.Workspace.parse_manifest (Protoss.Workspace.project_root project) in
      let result = Protoss.Workspace.build manifest in
      let elapsed = Unix.gettimeofday () -. start in
      Printf.printf "build=%s\nseconds=%.6f\n%s" result.build_id elapsed
        (Protoss.Workspace.stats_to_string result.stats)
  | _ -> usage ()

let () =
  protect (fun () ->
      match Array.to_list Sys.argv |> List.tl with
      | [ "parse"; file ] -> command_parse file
      | [ "check"; "--graph"; file ] -> command_check_graph file
      | [ "check"; file ] -> command_check file
      | [ "nf"; "--graph"; file ] -> command_nf_graph file
      | [ "nf"; file ] -> command_nf file
      | [ "hash"; "--graph"; file ] -> command_hash_graph file
      | [ "hash"; file ] -> command_hash file
      | [ "canon"; "--version" ] -> print_endline Protoss.Kernel.canonical_version
      | [ "canon"; "--graph"; file ] -> command_canon_graph file
      | [ "canon"; "--from-graph"; file ] -> command_canon_from_graph file
      | [ "canon"; file ] -> command_canon file
      | "eval" :: "--graph" :: file :: args -> command_eval_graph file args
      | "eval" :: file :: args -> command_eval file args
      | "run" :: "--graph" :: file :: args -> command_run_graph file args
      | "run" :: file :: args -> command_run file args
      | "resume" :: "--graph" :: file :: args -> command_resume_graph file args
      | "resume" :: file :: args -> command_resume file args
      | "world" :: args -> command_world args
      | "ledger" :: args -> command_ledger args
      | "app" :: args -> command_app args
      | "web" :: args -> command_web args
      | "project" :: args -> command_project args
      | "build" :: args -> command_project_build args
      | "patch" :: args -> command_patch args
      | "diff" :: args -> command_diff args
      | "audit" :: args -> command_audit args
      | "invariants" :: args -> command_invariants args
      | "fmt" :: args -> command_fmt args
      | "graph" :: args -> command_graph args
      | [ "repl" ] -> command_repl ()
      | "explain" :: args -> command_explain args
      | "bench" :: args -> command_bench args
      | "cache" :: args -> command_cache args
      | "store" :: args -> command_store args
      | _ -> usage ())
