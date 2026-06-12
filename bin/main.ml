(* Batch-tool GC tuning: a large minor heap and a relaxed space overhead cut
   GC time substantially on allocation-heavy canonicalization/eval workloads.
   Purely a time/memory trade-off; no observable behavior change. *)
let () =
  Gc.set
    { (Gc.get ()) with Gc.minor_heap_size = 16 * 1024 * 1024; Gc.space_overhead = 120 }

let usage () =
  prerr_endline
    "usage: protoss parse|check|nf|hash <file> | protoss check|nf|hash --graph <graph.json>\n\
     \       protoss check|nf|hash --store-graph <project-or-store> <graphHash>\n\
     \       protoss canon <file> | protoss canon --ptb <file> | protoss canon --graph <file> | protoss canon --from-graph <graph.json> | protoss canon --migrate-graph <graph.json>\n\
     \       protoss convert [--from-graph] --to pt|ptc|ptb <file>\n\
     \       protoss compare <file-a> <file-b> | protoss compare --graph <graph-a.json> <graph-b.json> | protoss compare --project <project-a> <project-b>\n\
     \       protoss capabilities <file> | protoss capabilities --project <project>\n\
     \       protoss duplicates <file> | protoss duplicates --project <project>\n\
     \       protoss termination <file> <definition>\n\
     \       protoss eval <file> --entry <name> [--trace-cache] [--cache <dir>]\n\
     \       protoss eval --graph <graph.json> --entry <name> [--trace-cache] [--cache <dir>]\n\
     \       protoss eval --store-graph <project-or-store> <graphHash> --entry <name> [--trace-cache] [--cache <dir>]\n\
     \       protoss run <file> --entry <name> [--ledger <root>]\n\
     \       protoss run --graph <graph.json> --entry <name> [--ledger <root>]\n\
     \       protoss run --store-graph <project-or-store> <graphHash> --entry <name> [--ledger <root>]\n\
     \       protoss resume <file> --entry <name> --event <event> --response <value> [--ledger <root>]\n\
     \       protoss resume --graph <graph.json> --entry <name> --event <event> --response <value> [--ledger <root>]\n\
     \       protoss resume --store-graph <project-or-store> <graphHash> --entry <name> --event <event> --response <value> [--ledger <root>]\n\
     \       protoss world init [<ledger-root>]\n\
     \       protoss ledger event|world|inspect|replay|diff|export|import|fork|simulate|compare-branches|merge|branches|reject [args]\n\
     \       protoss app check <project>\n\
     \       protoss web build|serve|inspect <project> [--out <dir>] [--port <n>]\n\
     \       protoss live [project] [--port <n>]   (build + serve the full-stack app)\n\
     \       protoss runtime init|status|inspect|world|audit <project> | protoss runtime reset <project> --yes\n\
     \       protoss harness run <project-or-store> <harness.pth>\n\
     \       protoss self parse|resolve|deps|capabilities|static <file> [--json]\n\
     \       protoss self typecheck <file> [--json] | type-of <file> --entry <name> | compare-typecheck <file>\n\
     \       protoss self fmt [--check] <file> | protoss self canon <file> [--compare]\n\
     \       protoss init [project] [--minimal]   (alias for project init: scaffold a full-stack app)\n\
     \       protoss project init|check|build|lock|package|interface|export-layout [project] [--stats|--locked|--check [interface.json]|--json|--out <dir>]\n\
     \       protoss build [project] [--target web] [--stats] [--locked]\n\
     \       protoss patch check|apply <store> <patch.json> | protoss patch audit <store> [latest|ref] | protoss patch review <patch.json>\n\
     \       protoss patch from-diff <store-a> <store-b> | protoss patch from-text-diff <store> <diff.patch>\n\
     \       protoss diff [--json] <store-a> <store-b>\n\
     \       protoss audit [project]\n\
     \       protoss git map [project] | protoss git blame [project] <file>\n\
     \       protoss invariants file <file> | graph <graph.json> | alpha <file-a> <file-b>\n\
     \       protoss invariants graph --store-graph <project-or-store> <graphHash>\n\
     \       protoss invariants process <file> --entry <name> --response <value>\n\
     \       protoss invariants process --graph <graph.json> --entry <name> --response <value>\n\
     \       protoss invariants process --store-graph <project-or-store> <graphHash> --entry <name> --response <value>\n\
     \       protoss invariants ledger <file> --entry <name> --response <value> [--ledger <root>]\n\
     \       protoss invariants ledger --graph <graph.json> --entry <name> --response <value> [--ledger <root>]\n\
     \       protoss invariants ledger --store-graph <project-or-store> <graphHash> --entry <name> --response <value> [--ledger <root>]\n\
     \       protoss invariants package <project>\n\
     \       protoss fmt [--human] [--check] <file>\n\
     \       protoss graph <project> --out <graph.json> | --dot <graph.dot>\n\
     \       protoss graph --stats <graph.json> | --roots <graph.json> | --deps <graph.json> [nameOrDefId] | --capabilities <graph.json> | --capability <graph.json> <nameOrCapRef> | --capability-scopes <graph.json> [nameOrCapRef] | --host-contract <graph.json> | --check-host-contract <graph.json> <contract.json> | --node <graph.json> <nodeRef> | --def <graph.json> <nameOrDefId>\n\
     \       protoss graph --store-graph <project-or-store> <graphHash> --out <graph.json> | --dot <graph.dot> | --stats | --roots | --deps [nameOrDefId] | --capabilities | --capability <nameOrCapRef> | --capability-scopes [nameOrCapRef] | --host-contract | --check-host-contract <contract.json> | --node <nodeRef> | --def <nameOrDefId>\n\
     \       protoss agent graph <graph.json> [--summary|--stats|--roots|--deps [nameOrDefId]|--capabilities|--capability <nameOrCapRef>|--capability-scopes [nameOrCapRef]|--host-contract|--node <nodeRef>|--def <nameOrDefId>|--explain <nameOrDefId>]\n\
     \       protoss agent graph --store-graph <project-or-store> <graphHash> [--summary|--stats|--roots|--deps [nameOrDefId]|--capabilities|--capability <nameOrCapRef>|--capability-scopes [nameOrCapRef]|--host-contract|--node <nodeRef>|--def <nameOrDefId>|--explain <nameOrDefId>]\n\
     \       protoss agent explain <graph.json> <nameOrDefId> | protoss agent explain --store-graph <project-or-store> <graphHash> <nameOrDefId>\n\
     \       protoss agent protocol | guard-write <path> | commit <store> <patch.json> --harness <harness.pth> [--harness <harness.pth> ...] | factor-identical <project-or-store> [--out <patch.json>] | synthesize-tests <project-or-store> | generate-migration <old-project-or-store> <new-project-or-store> | compare-candidates <project-or-store> <left.patch.json> <right.patch.json>\n\
     \       protoss mcp serve\n\
     \       protoss repl\n\
     \       protoss explain <error-code>|--list\n\
     \       protoss grammar kernel|human\n\
     \       protoss spec check [protoss-spec.md]\n\
     \       protoss bench build <project>\n\
     \       protoss bytecode <file> | bytecode run <file> --entry <name> | bytecode exec <file.ptvm> --entry <name>\n\
     \       protoss edit import|explain <store-or-project-a> <store-or-project-b>\n\
     \       protoss doctor --v1 [--json]\n\
     \       protoss cache stats|list <dir>\n\
     \       protoss store list|get|deps|roots|graphs|graph|graph-put|host-contracts|host-contract|stats|gc [args]";
  exit 2

let parse_and_check file =
  Protoss.Loader.check_file file

let is_canonical_file file =
  match Filename.extension file with ".ptc" | ".ptb" -> true | _ -> false

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
  let code = Protoss.Public_error.code_for_cli_kind kind msg in
  prerr_endline (code ^ " " ^ kind ^ ": " ^ msg);
  exit 1

let protect f =
  try f () with
  | Protoss.Parser.Error msg -> print_error "parse error" msg
  | Protoss.Loader.Error msg -> print_error "load error" msg
  | Protoss.Kernel.Error msg -> print_error "check error" msg
  | Protoss.Patch.Error msg -> print_error "patch error" msg
  | Protoss.Harness.Error msg -> print_error "harness error" msg
  | Protoss.Store.Error msg -> print_error "store error" msg
  | Protoss.Workspace.Error msg -> print_error "workspace error" msg
  | Protoss.Web.Error msg -> print_error "web error" msg
  | Protoss.Runtime_store.Error msg -> print_error "runtime error" msg
  | Unix.Unix_error (err, fn, arg) ->
      print_error "system error" (fn ^ "(" ^ arg ^ "): " ^ Unix.error_message err)
  | Failure msg -> print_error "error" msg
  | Invalid_argument msg -> print_error "internal error" ("invalid argument: " ^ msg)
  | Not_found -> print_error "internal error" "missing value"
  | Match_failure _ -> print_error "internal error" "pattern match failure"
  | End_of_file -> print_error "input error" "unexpected end of file"
  | Sys_error msg -> print_error "system error" msg
  | _ -> print_error "internal error" "unexpected exception"

let command_parse file =
  let p =
    if is_canonical_file file then Protoss.Loader.parse_file file
    else Protoss.Parser.parse_file file
  in
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

let checked_store_graph project_or_store graph_hash =
  Protoss.Workspace.checked_store_graph
    (Protoss.Workspace.store_of_arg project_or_store)
    graph_hash

let command_check_graph file =
  let checked = checked_graph file in
  Printf.printf "Graph OK: %d definitions\n" (List.length checked.Protoss.Kernel.defs)

let command_check_store_graph project_or_store graph_hash =
  let checked = checked_store_graph project_or_store graph_hash in
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

let command_nf_store_graph project_or_store graph_hash =
  let checked = checked_store_graph project_or_store graph_hash in
  Protoss.Runtime.normalize_all checked
  |> List.iter (fun (name, value) ->
         Printf.printf "%s = %s\n" name (Protoss.Runtime.value_to_string value))

let command_hash file =
  let checked = parse_and_check file in
  print_endline (Protoss.Kernel.hash_program checked)

let command_hash_graph file =
  let checked = checked_graph file in
  print_endline (Protoss.Kernel.hash_program checked)

let command_hash_store_graph project_or_store graph_hash =
  let checked = checked_store_graph project_or_store graph_hash in
  print_endline (Protoss.Kernel.hash_program checked)

let finish_compare left_hash right_hash =
  if String.equal left_hash right_hash then Printf.printf "same\nhash=%s\n" left_hash
  else (
    Printf.printf "different\nleft=%s\nright=%s\n" left_hash right_hash;
    exit 1)

let command_compare = function
  | [ left; right ] ->
      finish_compare
        (Protoss.Kernel.hash_program (parse_and_check left))
        (Protoss.Kernel.hash_program (parse_and_check right))
  | [ "--graph"; left; right ] ->
      finish_compare
        (Protoss.Kernel.hash_program (checked_graph left))
        (Protoss.Kernel.hash_program (checked_graph right))
  | [ "--project"; left; right ] ->
      let project_hash root =
        let manifest =
          Protoss.Workspace.parse_manifest
            (Protoss.Workspace.project_root root)
        in
        (Protoss.Workspace.build ~write:false manifest).Protoss.Workspace.build_id
      in
      finish_compare (project_hash left) (project_hash right)
  | _ -> usage ()

let capability_audit_text checked =
  let program_caps = List.sort_uniq String.compare checked.Protoss.Kernel.program.capabilities in
  let def_lines =
    checked.defs
    |> List.map (fun (d : Protoss.Kernel.checked_def) ->
           let caps = List.sort_uniq String.compare d.capabilities in
           d.def.name ^ " cap-scope-ref=" ^ Protoss.Kernel.capability_scope_ref caps
           ^ " caps=[" ^ String.concat "," caps ^ "]")
  in
  let risk_lines = Protoss.Kernel.secret_leak_risks checked in
  "program-hash=" ^ Protoss.Kernel.hash_program checked ^ "\nprogram-caps=["
  ^ String.concat "," program_caps ^ "]\ndefs=\n" ^ String.concat "\n" def_lines
  ^ (if def_lines = [] then "" else "\n")
  ^ "risks=\n"
  ^ (match risk_lines with [] -> "none\n" | lines -> String.concat "\n" lines ^ "\n")

let command_capabilities = function
  | [ file ] -> print_string (capability_audit_text (parse_and_check file))
  | [ "--project"; project ] ->
      let manifest =
        Protoss.Workspace.parse_manifest
          (Protoss.Workspace.project_root project)
      in
      let build = Protoss.Workspace.build ~write:false manifest in
      print_string (capability_audit_text build.Protoss.Workspace.checked)
  | _ -> usage ()

let duplicate_audit_text checked =
  let groups = Hashtbl.create 32 in
  List.iter
    (fun (d : Protoss.Kernel.checked_def) ->
      let names = Option.value (Hashtbl.find_opt groups d.def_id) ~default:[] in
      Hashtbl.replace groups d.def_id (d.def.name :: names))
    checked.Protoss.Kernel.defs;
  let duplicates =
    Hashtbl.fold
      (fun def_id names acc ->
        let names = List.sort String.compare names in
        match names with _ :: _ :: _ -> (def_id, names) :: acc | _ -> acc)
      groups []
    |> List.sort (fun (a, _) (b, _) -> String.compare a b)
  in
  let lines =
    List.map
      (fun (def_id, names) ->
        "def-id=" ^ def_id ^ " names=[" ^ String.concat "," names ^ "]")
      duplicates
  in
  "program-hash=" ^ Protoss.Kernel.hash_program checked ^ "\nduplicates="
  ^ string_of_int (List.length duplicates) ^ "\n" ^ String.concat "\n" lines
  ^ if lines = [] then "" else "\n"

let command_duplicates = function
  | [ file ] -> print_string (duplicate_audit_text (parse_and_check file))
  | [ "--project"; project ] ->
      let manifest =
        Protoss.Workspace.parse_manifest
          (Protoss.Workspace.project_root project)
      in
      let build = Protoss.Workspace.build ~write:false manifest in
      print_string (duplicate_audit_text build.Protoss.Workspace.checked)
  | _ -> usage ()

let command_termination = function
  | [ file; name ] ->
      print_string
        (Protoss.Kernel.termination_explanation_text (parse_and_check file) name)
  | _ -> usage ()

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

let command_canon_ptb file =
  let checked = parse_and_check file in
  print_string (Protoss.Canonical_binary.checked_to_binary checked)

let command_canon_graph file =
  let checked = parse_and_check file in
  print_string (Protoss.Canonical_ir.serialize_graph checked)

let command_canon_from_graph file =
  print_endline (Protoss.Canonical_ir.graph_to_program (Protoss.Store.read_file file))

let command_canon_migrate_graph file =
  print_string (Protoss.Canonical_ir.migrate_graph (Protoss.Store.read_file file))

let print_conversion target checked =
  match target with
  | "pt" ->
      print_string (Protoss.Ast.string_of_program checked.Protoss.Kernel.program)
  | "ptc" -> print_endline (canonical_program checked)
  | "ptb" -> print_string (Protoss.Canonical_binary.checked_to_binary checked)
  | _ -> usage ()

let command_convert = function
  | [ "--to"; target; file ] -> print_conversion target (parse_and_check file)
  | [ "--from-graph"; "--to"; target; file ] ->
      print_conversion target (checked_graph file)
  | _ -> usage ()

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

let command_eval_store_graph project_or_store graph_hash args =
  let entry, rest = find_entry args in
  let checked = checked_store_graph project_or_store graph_hash in
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
      Printf.printf "ProcessEvalKey %s\n"
        (Protoss.Runtime.process_eval_key_for_def ~world_ref:Protoss.Ledger.initial_world checked
           entry);
      Printf.printf "Done %s\n" (Protoss.Runtime.value_to_string v)
  | Protoss.Runtime.VProcessRequest s ->
      let root = Option.value (find_arg "--ledger" args) ~default:(Filename.concat "target" "ledger") in
      Printf.printf "ProcessEvalKey %s\n"
        (Protoss.Runtime.process_eval_key_for_def ~world_ref:Protoss.Ledger.initial_world
           ~cap_scope:s.cap_scope checked entry);
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
      Printf.printf "CapScopeRef %s\n" (Protoss.Kernel.capability_scope_ref s.cap_scope);
      Printf.printf "CapabilityRef %s\n"
        (Option.value (Protoss.Kernel.req_capability_ref s.req) ~default:"-");
      Printf.printf "RequestSignatureRef %s\n" (Protoss.Kernel.req_signature_ref s.req);
      Printf.printf "Event %s\n" event;
      Printf.printf "NextWorldRef %s\n" next_world
  | other ->
      Protoss.Kernel.fail ("run entry is not a Process: " ^ Protoss.Runtime.value_to_string other)

let command_run file args =
  run_checked (parse_and_check file) args

let command_run_graph file args =
  run_checked (checked_graph file) args

let command_run_store_graph project_or_store graph_hash args =
  run_checked (checked_store_graph project_or_store graph_hash) args

let checked_project_or_store project_or_store =
  let root = Protoss.Workspace.project_root project_or_store in
  if Sys.file_exists (Protoss.Workspace.manifest_path root) then
    let build =
      Protoss.Workspace.build (Protoss.Workspace.parse_manifest root)
    in
    build.Protoss.Workspace.checked
  else
    let store = Protoss.Workspace.store_of_arg project_or_store in
    let graph_path = Filename.concat store "program.graph.json" in
    if Sys.file_exists graph_path then
      Protoss.Canonical_ir.checked_of_graph (Protoss.Store.read_file graph_path)
    else Protoss.Kernel.fail ("missing current program graph: " ^ graph_path)

let command_harness = function
  | [ "run"; project_or_store; harness_file ] ->
      let checked = checked_project_or_store project_or_store in
      print_string
        (Protoss.Harness.run_json checked ~source:harness_file
           (Protoss.Store.read_file harness_file))
  | _ -> usage ()

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

let command_resume_store_graph project_or_store graph_hash args =
  resume_checked (checked_store_graph project_or_store graph_hash) args

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
  | [ "fork"; name; world ] -> print_endline (Protoss.Ledger.fork default_ledger name world)
  | [ "fork"; root; name; world ] -> print_endline (Protoss.Ledger.fork root name world)
  | [ "simulate"; name; world; description ] ->
      let event, simulated_world =
        Protoss.Ledger.simulate default_ledger name world description
      in
      Printf.printf "SimulationEvent %s\nWorld %s\n" event simulated_world
  | [ "simulate"; root; name; world; description ] ->
      let event, simulated_world = Protoss.Ledger.simulate root name world description in
      Printf.printf "SimulationEvent %s\nWorld %s\n" event simulated_world
  | [ "compare-branches"; root; harness; left; right ] ->
      print_string (Protoss.Ledger.compare_branches_by_harness root harness left right)
  | [ "merge"; world_a; world_b ] ->
      print_endline (Protoss.Ledger.merge default_ledger world_a world_b)
  | [ "merge"; root; world_a; world_b ] ->
      print_endline (Protoss.Ledger.merge root world_a world_b)
  | [ "branches" ] -> print_string (Protoss.Ledger.branches default_ledger)
  | [ "branches"; root ] -> print_string (Protoss.Ledger.branches root)
  | [ "reject"; root; world; event; code; message ] ->
      let negative_event, next_world =
        Protoss.Ledger.record_external_error root world event code message
      in
      Printf.printf "ExternalErrorEvent %s\nWorld %s\n" negative_event next_world
  | _ -> usage ()

let command_patch = function
  | "from-diff" :: store_a :: store_b :: [] ->
      print_string
        (Protoss.Workspace.patch_from_diff
           (Protoss.Workspace.store_of_arg store_a)
           (Protoss.Workspace.store_of_arg store_b))
  | "from-text-diff" :: store :: diff :: [] ->
      print_string (Protoss.Patch.from_text_diff (Protoss.Workspace.store_of_arg store) diff)
  | "check" :: store :: patch :: [] ->
      let checked = Protoss.Patch.check store patch in
      Printf.printf "Patch valid %s\n" (Protoss.Patch.describe_checked checked)
  | "apply" :: store :: patch :: [] ->
      let hash = Protoss.Patch.apply store patch in
      Printf.printf "Patch accepted %s\n" hash
  | "review" :: patch :: [] -> print_string (Protoss.Patch.review_text patch)
  | "audit" :: store :: [] -> print_string (Protoss.Patch.inspect_audit store)
  | "audit" :: store :: patch_ref :: [] ->
      print_string (Protoss.Patch.inspect_audit ~ref:patch_ref store)
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
  | [ "graphs" ] ->
      print_string (Protoss.Workspace.graphs_store (Protoss.Workspace.project_store_of_cwd ()))
  | [ "graphs"; project_or_store ] ->
      print_string (Protoss.Workspace.graphs_store (Protoss.Workspace.store_of_arg project_or_store))
  | [ "graph"; graph_hash ] ->
      print_string
        (Protoss.Workspace.graph_store (Protoss.Workspace.project_store_of_cwd ()) graph_hash)
  | [ "graph"; project_or_store; graph_hash ] ->
      print_string
        (Protoss.Workspace.graph_store
           (Protoss.Workspace.store_of_arg project_or_store)
           graph_hash)
  | [ "graph-put"; project_or_store; graph_file ] ->
      let graph_hash =
        Protoss.Workspace.put_store_graph
          (Protoss.Workspace.store_of_arg project_or_store)
          (Protoss.Store.read_file graph_file)
      in
      print_endline graph_hash
  | [ "host-contracts" ] ->
      print_string
        (Protoss.Workspace.host_contracts_store (Protoss.Workspace.project_store_of_cwd ()))
  | [ "host-contracts"; project_or_store ] ->
      print_string
        (Protoss.Workspace.host_contracts_store
           (Protoss.Workspace.store_of_arg project_or_store))
  | [ "host-contract" ] ->
      print_string
        (Protoss.Workspace.host_contract_store (Protoss.Workspace.project_store_of_cwd ())
           "current")
  | [ "host-contract"; id ] ->
      print_string
        (Protoss.Workspace.host_contract_store (Protoss.Workspace.project_store_of_cwd ()) id)
  | [ "host-contract"; project_or_store; id ] ->
      print_string
        (Protoss.Workspace.host_contract_store
           (Protoss.Workspace.store_of_arg project_or_store)
           id)
  | [ "stats"; root ] ->
      let objects, defs, canonical = Protoss.Store.stats root in
      Printf.printf "objects=%d\ndefs=%d\ncanonical=%d\n" objects defs canonical
  | [ "gc"; root ] -> print_string (Protoss.Store.gc_report (Protoss.Store.gc root))
  | [ "gc"; "--sweep"; "--yes"; root ] ->
      print_string (Protoss.Store.gc_report (Protoss.Store.gc ~delete:true root))
  | _ -> usage ()

let command_cache = function
  | [ "stats"; root ] ->
      let hits, misses, entries = Protoss.Runtime.persistent_cache_stats root in
      Printf.printf "hits=%d\nmisses=%d\nentries=%d\n" hits misses entries
  | [ "list"; root ] ->
      Protoss.Runtime.persistent_cache_entries root
      |> List.iter print_endline
  | _ -> usage ()

let is_flag s = String.length s > 0 && s.[0] = '-'

let project_arg args =
  match List.filter (fun s -> not (is_flag s)) args with
  | [] -> "."
  | [ path ] -> path
  | _ -> usage ()

let find_flag_value flag args = find_arg flag args

let rec find_up dir rel =
  let candidate = Filename.concat dir rel in
  if Sys.file_exists candidate then Some candidate
  else
    let parent = Filename.dirname dir in
    if String.equal parent dir then None else find_up parent rel

(* Installed layout (dune install): the binary lives at <prefix>/bin/protoss and
   the prelude is packaged at <prefix>/share/protoss/prelude.protoss. Derive it
   from the executable's own path so an installed protoss is self-contained. *)
let installed_prelude_path () =
  let exe_dir = Filename.dirname Sys.executable_name in
  let candidate =
    Filename.concat exe_dir
      (Filename.concat Filename.parent_dir_name "share/protoss/prelude.protoss")
  in
  if Sys.file_exists candidate then Some candidate else None

let prelude_path () =
  match Sys.getenv_opt "PROTOSS_STDLIB" with
  | Some p -> p
  | None -> (
      match find_up (Sys.getcwd ()) "stdlib/prelude.protoss" with
      | Some p -> p
      | None -> (
          match installed_prelude_path () with
          | Some p -> p
          | None ->
              Protoss.Kernel.fail "cannot locate stdlib/prelude.protoss (set PROTOSS_STDLIB)"))

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
  let compiled_artifact = ref None in
  let result =
    match target with
    | Some "web" ->
        if locked then ignore (Protoss.Workspace.check_lock manifest);
        let web = Protoss.Web.build root in
        compiled_artifact := Some web.Protoss.Web.compiled_artifact;
        web.Protoss.Web.build
    | Some other when Protoss.Workspace.is_compiler_backend_target other ->
        if locked then ignore (Protoss.Workspace.check_lock manifest);
        let build, artifact =
          Protoss.Workspace.build_compiler_backend manifest other
        in
        compiled_artifact := Some artifact;
        build
    | Some other -> Protoss.Workspace.fail ("unknown build target: " ^ other)
    | None ->
        if locked then Protoss.Workspace.build_locked manifest
        else Protoss.Workspace.build manifest
  in
  Printf.printf "Build %s\nUniverseRoot %s\nStore %s\n" result.Protoss.Workspace.build_id
    result.universe_root result.store;
  (match !compiled_artifact with
  | None -> ()
  | Some artifact ->
      Printf.printf "CompiledArtifact %s\n" artifact.Protoss.Workspace.compiled_artifact_ref);
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
    Printf.printf
      "Package OK %s\nPath %s\nInterface %s\nInterfacePath %s\nContract %s\nLock %s\nBuild %s\nUniverseRoot %s\nStore %s\n"
      result.Protoss.Workspace.package_ref result.package_path result.interface_ref
      result.interface_path result.interface_contract_hash result.lock_hash result.build_id
      result.universe_root result.store
  else
    let result = Protoss.Workspace.write_package ~locked:(has_flag "--locked" args) manifest in
    Printf.printf
      "Package %s\nPath %s\nInterface %s\nInterfacePath %s\nContract %s\nLock %s\nBuild %s\nUniverseRoot %s\nStore %s\n"
      result.Protoss.Workspace.package_ref result.package_path result.interface_ref
      result.interface_path result.interface_contract_hash result.lock_hash result.build_id
      result.universe_root result.store

let parse_project_interface_args args =
  let rec loop json check paths = function
    | [] -> (json, check, List.rev paths)
    | "--json" :: rest -> loop true check paths rest
    | "--check" :: file :: rest -> loop json (Some file) paths rest
    | x :: _ when is_flag x -> usage ()
    | x :: rest -> loop json check (x :: paths) rest
  in
  match loop false None [] args with
  | true, Some _, _ -> usage ()
  | json, check, [] -> (json, check, ".")
  | json, check, [ root ] -> (json, check, root)
  | _ -> usage ()

let command_project_interface args =
  let json, check, root = parse_project_interface_args args in
  let manifest = Protoss.Workspace.parse_manifest (Protoss.Workspace.project_root root) in
  match check with
  | Some expected ->
      print_string (Protoss.Workspace.check_package_interface_contract manifest expected)
  | None ->
      if json then print_string (Protoss.Workspace.package_interface_json manifest)
      else print_string (Protoss.Workspace.package_interface_text manifest)

let parse_project_export_layout_args args =
  let rec loop out paths = function
    | [] -> (out, List.rev paths)
    | "--out" :: dir :: rest -> loop (Some dir) paths rest
    | x :: _ when is_flag x -> usage ()
    | x :: rest -> loop out (x :: paths) rest
  in
  match loop None [] args with
  | out, [] -> (out, ".")
  | out, [ root ] -> (out, root)
  | _ -> usage ()

let command_project_export_layout args =
  let out, root = parse_project_export_layout_args args in
  let manifest = Protoss.Workspace.parse_manifest (Protoss.Workspace.project_root root) in
  print_string (Protoss.Workspace.layout_export_text (Protoss.Workspace.export_layout ?out manifest))

let command_project = function
  | "init" :: args ->
      (* Default: a runnable full-stack app skeleton (counter) wired to the
         resolved prelude, so `protoss live` works straight away. `--minimal`
         keeps the trivial `(def main Nat 0)` module with no stdlib. *)
      let minimal = List.mem "--minimal" args in
      let root = project_arg (List.filter (fun a -> not (String.equal a "--minimal")) args) in
      (* Full-stack app by default, but it needs the prelude path for its
         manifest. If the prelude can't be located (e.g. an installed binary
         without the packaged prelude), fall back to a minimal module so init
         always succeeds rather than erroring out. *)
      let stdlib = if minimal then None else (try Some (prelude_path ()) with _ -> None) in
      let path =
        match stdlib with
        | Some s -> Protoss.Workspace.init ~app:true ~stdlib:s root
        | None -> Protoss.Workspace.init root
      in
      Printf.printf "Initialized %s\n" path;
      (match stdlib with
      | Some _ -> Printf.printf "Run it:  protoss live %s\n" root
      | None ->
          if not minimal then
            Printf.printf "(prelude not found; created a minimal module — set PROTOSS_STDLIB for full-stack)\n")
  | "check" :: args ->
      let root = project_arg args in
      let manifest = Protoss.Workspace.parse_manifest (Protoss.Workspace.project_root root) in
      Protoss.Workspace.check_project manifest;
      Printf.printf "Project OK %s\n" manifest.name
  | "build" :: args -> command_project_build args
  | "lock" :: args -> command_project_lock args
  | "package" :: args -> command_project_package args
  | "interface" :: args -> command_project_interface args
  | "export-layout" :: args -> command_project_export_layout args
  | _ -> usage ()

let command_app = function
  | [ "check"; project ] ->
      let contract = Protoss.Web.app_check project in
      Printf.printf "App OK model=%s msg=%s architecture=%s\n"
        (Protoss.Ast.string_of_typ contract.Protoss.Web.model_ty)
        (Protoss.Ast.string_of_typ contract.msg_ty)
        contract.architecture;
      (match contract.Protoss.Web.backend with
      | None -> ()
      | Some b ->
          Printf.printf "Backend OK backendModel=%s toBackend=%s toFrontend=%s\n"
            (Protoss.Ast.string_of_typ b.Protoss.Web.backend_model_ty)
            (Protoss.Ast.string_of_typ b.Protoss.Web.to_backend_ty)
            (Protoss.Ast.string_of_typ b.Protoss.Web.to_frontend_ty))
  | _ -> usage ()

let command_web = function
  | "build" :: project :: args ->
      let out = find_flag_value "--out" args in
      let result = Protoss.Web.build ?out project in
      Printf.printf "Web build %s\nOut %s\nCompiledArtifact %s\n"
        result.Protoss.Web.build.build_id result.out_dir
        result.compiled_artifact.Protoss.Workspace.compiled_artifact_ref
  | "inspect" :: project :: [] -> print_string (Protoss.Web.inspect project)
  | "serve" :: project :: args ->
      let port =
        match find_flag_value "--port" args with Some p -> int_of_string p | None -> 8080
      in
      Protoss.Web.serve ~port project
  | _ -> usage ()

(* `protoss live [project] [--port N]`: the convenience "run my app" command —
   build the full-stack bundle and serve it over HTTP (same as `web serve`, but
   the obvious verb for a freshly `project init`'d app). *)
let command_live args =
  let port =
    match find_flag_value "--port" args with Some p -> int_of_string p | None -> 8080
  in
  (* drop --port and its value so project_arg sees only the project path *)
  let rec strip = function
    | "--port" :: _ :: rest -> strip rest
    | x :: rest -> x :: strip rest
    | [] -> []
  in
  let project = project_arg (strip args) in
  Protoss.Web.serve ~port project

let command_runtime = function
  | [ "init"; project ] -> print_string (Protoss.Runtime_store.init project)
  | [ "status"; project ] -> print_string (Protoss.Runtime_store.status project)
  | [ "inspect"; project ] -> print_string (Protoss.Runtime_store.inspect project)
  | [ "world"; project ] -> print_string (Protoss.Runtime_store.world project)
  | [ "audit"; project ] -> print_string (Protoss.Runtime_store.audit project)
  | [ "reset"; project ] -> print_string (Protoss.Runtime_store.reset ~confirm:false project)
  | [ "reset"; project; "--yes" ] ->
      print_string (Protoss.Runtime_store.reset ~confirm:true project)
  | [ "reset"; "--yes"; project ] ->
      print_string (Protoss.Runtime_store.reset ~confirm:true project)
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

let command_git = function
  | [ "map" ] ->
      let manifest = Protoss.Workspace.parse_manifest (Sys.getcwd ()) in
      let mapping = Protoss.Workspace.write_git_mapping manifest in
      print_string (Protoss.Workspace.git_mapping_content mapping)
  | [ "map"; project ] ->
      let manifest = Protoss.Workspace.parse_manifest (Protoss.Workspace.project_root project) in
      let mapping = Protoss.Workspace.write_git_mapping manifest in
      print_string (Protoss.Workspace.git_mapping_content mapping)
  | [ "blame"; file ] ->
      let manifest = Protoss.Workspace.parse_manifest (Sys.getcwd ()) in
      let ledger = Protoss.Workspace.write_git_blame_ledger manifest file in
      print_string (Protoss.Workspace.git_blame_ledger_content ledger)
  | [ "blame"; project; file ] ->
      let manifest = Protoss.Workspace.parse_manifest (Protoss.Workspace.project_root project) in
      let ledger = Protoss.Workspace.write_git_blame_ledger manifest file in
      print_string (Protoss.Workspace.git_blame_ledger_content ledger)
  | _ -> usage ()

let command_invariants = function
  | [ "file"; file ] ->
      print_string (Protoss.Invariants.describe_file (Protoss.Invariants.check_file file))
  | [ "graph"; file ] ->
      print_string (Protoss.Invariants.describe_file (Protoss.Invariants.check_graph file))
  | [ "graph"; "--store-graph"; project_or_store; graph_hash ] ->
      print_string
        (Protoss.Invariants.describe_file
           (Protoss.Invariants.check_store_graph project_or_store graph_hash))
  | [ "alpha"; left; right ] ->
      print_string
        (Protoss.Invariants.describe_alpha (Protoss.Invariants.check_alpha left right))
  | "process" :: "--store-graph" :: project_or_store :: graph_hash :: args ->
      let entry = fst (find_entry args) in
      let response = required_arg "--response" args in
      print_string
        (Protoss.Invariants.describe_process
           (Protoss.Invariants.check_store_graph_process project_or_store graph_hash entry
              response))
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
  | "ledger" :: "--store-graph" :: project_or_store :: graph_hash :: args ->
      let entry = fst (find_entry args) in
      let response = required_arg "--response" args in
      print_string
        (Protoss.Invariants.describe_ledger
           (Protoss.Invariants.check_store_graph_ledger_process
              ?ledger:(find_arg "--ledger" args) project_or_store graph_hash entry response))
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

(* Render the Protoss/H projection, refusing to emit text whose canonical
   hash would diverge from the source's: the view must match the canon. When
   the file does not check in isolation (unresolved imports), the re-parse is
   still required to succeed but the hash guard is skipped. *)
let render_human file =
  let program = Protoss.Parser.parse_file file in
  let rendered =
    try Protoss.Surface_syntax.render_program program
    with Protoss.Surface_syntax.Unrenderable msg ->
      Protoss.Kernel.fail ("no Protoss/H projection for " ^ file ^ ": " ^ msg)
  in
  let reparsed =
    try Protoss.Parser.parse_string rendered
    with Protoss.Parser.Error msg ->
      Protoss.Kernel.fail ("Protoss/H projection does not re-parse: " ^ msg)
  in
  let checked_hash program =
    try Some (Protoss.Kernel.hash_program (Protoss.Kernel.check_program program))
    with Protoss.Kernel.Error _ -> None
  in
  (match checked_hash program with
  | None -> ()
  | Some original_hash -> (
      match checked_hash reparsed with
      | Some rendered_hash when String.equal original_hash rendered_hash -> ()
      | Some _ -> Protoss.Kernel.fail ("Protoss/H projection changes the hash of " ^ file)
      | None -> Protoss.Kernel.fail ("Protoss/H projection does not check: " ^ file)));
  rendered

let command_fmt = function
  | [ file ] -> print_string (Protoss.Ast.string_of_program (Protoss.Parser.parse_file file))
  | [ "--check"; file ] ->
      let original = Protoss.Store.read_file file in
      let formatted = Protoss.Ast.string_of_program (Protoss.Parser.parse_file file) in
      if String.equal original formatted then print_endline "OK"
      else Protoss.Kernel.fail ("format check failed: " ^ file)
  | [ "--human"; file ] -> print_string (render_human file)
  | [ "--human"; "--check"; file ] ->
      let original = Protoss.Store.read_file file in
      if String.equal original (render_human file) then print_endline "OK"
      else Protoss.Kernel.fail ("human format check failed: " ^ file)
  | _ -> usage ()

let command_graph = function
  | [ "--stats"; file ] ->
      print_string
        (Protoss.Canonical_ir.describe_graph_stats
           (Protoss.Canonical_ir.graph_stats (Protoss.Store.read_file file)))
  | [ "--roots"; file ] ->
      print_string
        (Protoss.Canonical_ir.describe_graph_definitions
           (Protoss.Canonical_ir.graph_definitions (Protoss.Store.read_file file)))
  | [ "--deps"; file ] ->
      print_string
        (Protoss.Canonical_ir.describe_graph_dependencies
           (Protoss.Canonical_ir.graph_dependencies (Protoss.Store.read_file file)))
  | [ "--deps"; file; id ] ->
      print_string
        (Protoss.Canonical_ir.describe_graph_dependencies
           (Protoss.Canonical_ir.graph_dependencies_for (Protoss.Store.read_file file) id))
  | [ "--capabilities"; file ] ->
      print_string
        (Protoss.Canonical_ir.describe_graph_capabilities
           (Protoss.Canonical_ir.graph_capabilities (Protoss.Store.read_file file)))
  | [ "--capability"; file; id ] ->
      print_string
        (Protoss.Canonical_ir.describe_graph_capabilities
           [ Protoss.Canonical_ir.graph_capability (Protoss.Store.read_file file) id ])
  | [ "--capability-scopes"; file ] ->
      print_string
        (Protoss.Canonical_ir.describe_graph_capability_scopes
           (Protoss.Canonical_ir.graph_capability_scopes (Protoss.Store.read_file file)))
  | [ "--capability-scopes"; file; id ] ->
      print_string
        (Protoss.Canonical_ir.describe_graph_capability_scopes
           (Protoss.Canonical_ir.graph_capability_scopes_for (Protoss.Store.read_file file) id))
  | [ "--host-contract"; file ] ->
      print_string (Protoss.Canonical_ir.graph_host_contract (Protoss.Store.read_file file))
  | [ "--check-host-contract"; file; contract_file ] ->
      print_string
        (Protoss.Canonical_ir.check_graph_host_contract (Protoss.Store.read_file file)
           (Protoss.Store.read_file contract_file))
  | [ "--node"; file; node_ref ] ->
      print_string
        (Protoss.Canonical_ir.describe_graph_node
           (Protoss.Canonical_ir.graph_node (Protoss.Store.read_file file) node_ref))
  | [ "--def"; file; id ] ->
      print_string
        (Protoss.Canonical_ir.describe_graph_definition
           (Protoss.Canonical_ir.graph_definition (Protoss.Store.read_file file) id))
  | [ "--store-graph"; project_or_store; graph_hash; "--out"; out ] ->
      let store = Protoss.Workspace.store_of_arg project_or_store in
      Protoss.Store.write_file_atomic out (Protoss.Workspace.graph_store store graph_hash);
      Printf.printf "Wrote %s\n" out
  | [ "--store-graph"; project_or_store; graph_hash; "--dot"; out ] ->
      let store = Protoss.Workspace.store_of_arg project_or_store in
      Protoss.Store.write_file_atomic out (Protoss.Workspace.store_graph_dot store graph_hash);
      Printf.printf "Wrote %s\n" out
  | [ "--store-graph"; project_or_store; graph_hash; "--stats" ] ->
      let store = Protoss.Workspace.store_of_arg project_or_store in
      print_string
        (Protoss.Canonical_ir.describe_graph_stats
           (Protoss.Workspace.store_graph_stats store graph_hash))
  | [ "--store-graph"; project_or_store; graph_hash; "--roots" ] ->
      let store = Protoss.Workspace.store_of_arg project_or_store in
      print_string
        (Protoss.Canonical_ir.describe_graph_definitions
           (Protoss.Workspace.store_graph_definitions store graph_hash))
  | [ "--store-graph"; project_or_store; graph_hash; "--deps" ] ->
      let store = Protoss.Workspace.store_of_arg project_or_store in
      print_string
        (Protoss.Canonical_ir.describe_graph_dependencies
           (Protoss.Workspace.store_graph_dependencies store graph_hash))
  | [ "--store-graph"; project_or_store; graph_hash; "--deps"; id ] ->
      let store = Protoss.Workspace.store_of_arg project_or_store in
      print_string
        (Protoss.Canonical_ir.describe_graph_dependencies
           (Protoss.Workspace.store_graph_dependencies_for store graph_hash id))
  | [ "--store-graph"; project_or_store; graph_hash; "--capabilities" ] ->
      let store = Protoss.Workspace.store_of_arg project_or_store in
      print_string
        (Protoss.Canonical_ir.describe_graph_capabilities
           (Protoss.Workspace.store_graph_capabilities store graph_hash))
  | [ "--store-graph"; project_or_store; graph_hash; "--capability"; id ] ->
      let store = Protoss.Workspace.store_of_arg project_or_store in
      print_string
        (Protoss.Canonical_ir.describe_graph_capabilities
           [ Protoss.Workspace.store_graph_capability store graph_hash id ])
  | [ "--store-graph"; project_or_store; graph_hash; "--capability-scopes" ] ->
      let store = Protoss.Workspace.store_of_arg project_or_store in
      print_string
        (Protoss.Canonical_ir.describe_graph_capability_scopes
           (Protoss.Workspace.store_graph_capability_scopes store graph_hash))
  | [ "--store-graph"; project_or_store; graph_hash; "--capability-scopes"; id ] ->
      let store = Protoss.Workspace.store_of_arg project_or_store in
      print_string
        (Protoss.Canonical_ir.describe_graph_capability_scopes
           (Protoss.Workspace.store_graph_capability_scopes_for store graph_hash id))
  | [ "--store-graph"; project_or_store; graph_hash; "--host-contract" ] ->
      let store = Protoss.Workspace.store_of_arg project_or_store in
      print_string (Protoss.Workspace.store_graph_host_contract store graph_hash)
  | [ "--store-graph"; project_or_store; graph_hash; "--check-host-contract"; contract_file ] ->
      let store = Protoss.Workspace.store_of_arg project_or_store in
      print_string
        (Protoss.Workspace.check_store_graph_host_contract store graph_hash
           (Protoss.Store.read_file contract_file))
  | [ "--store-graph"; project_or_store; graph_hash; "--node"; node_ref ] ->
      let store = Protoss.Workspace.store_of_arg project_or_store in
      print_string
        (Protoss.Canonical_ir.describe_graph_node
           (Protoss.Workspace.store_graph_node store graph_hash node_ref))
  | [ "--store-graph"; project_or_store; graph_hash; "--def"; id ] ->
      let store = Protoss.Workspace.store_of_arg project_or_store in
      print_string
        (Protoss.Canonical_ir.describe_graph_definition
           (Protoss.Workspace.store_graph_definition store graph_hash id))
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

let command_agent_graph_query source input = function
  | [] | [ "--summary" ] ->
      print_string (Protoss.Canonical_ir.agent_graph_summary_json ~source input)
  | [ "--stats" ] ->
      print_string (Protoss.Canonical_ir.agent_graph_stats_json ~source input)
  | [ "--roots" ] | [ "--defs" ] | [ "--definitions" ] ->
      print_string (Protoss.Canonical_ir.agent_graph_definitions_json ~source input)
  | [ "--deps" ] ->
      print_string (Protoss.Canonical_ir.agent_graph_dependencies_json ~source input None)
  | [ "--deps"; id ] ->
      print_string
        (Protoss.Canonical_ir.agent_graph_dependencies_json ~source input (Some id))
  | [ "--capabilities" ] ->
      print_string (Protoss.Canonical_ir.agent_graph_capabilities_json ~source input)
  | [ "--capability"; id ] ->
      print_string (Protoss.Canonical_ir.agent_graph_capability_json ~source input id)
  | [ "--capability-scopes" ] ->
      print_string
        (Protoss.Canonical_ir.agent_graph_capability_scopes_json ~source input None)
  | [ "--capability-scopes"; id ] ->
      print_string
        (Protoss.Canonical_ir.agent_graph_capability_scopes_json ~source input (Some id))
  | [ "--host-contract" ] ->
      print_string (Protoss.Canonical_ir.agent_graph_host_contract_json ~source input)
  | [ "--node"; node_ref ] ->
      print_string (Protoss.Canonical_ir.agent_graph_node_json ~source input node_ref)
  | [ "--def"; id ] ->
      print_string (Protoss.Canonical_ir.agent_graph_definition_json ~source input id)
  | [ "--explain"; id ] ->
      print_string
        (Protoss.Canonical_ir.agent_graph_definition_explanation_json ~source input id)
  | _ -> usage ()

let harness_args args =
  let rec loop acc = function
    | [] -> List.rev acc
    | "--harness" :: harness :: rest -> loop (harness :: acc) rest
    | _ -> usage ()
  in
  loop [] args

let command_agent = function
  | [ "protocol" ] -> print_string (Protoss.Agent_protocol.protocol_json ())
  | [ "guard-write"; path ] ->
      let guard = Protoss.Agent_protocol.guard_write path in
      print_string (Protoss.Agent_protocol.guard_write_result_json guard);
      if not guard.guard_allowed then exit 1
  | "commit" :: store :: patch :: args ->
      print_string
        (Protoss.Agent_protocol.commit_patch_json ~harnesses:(harness_args args)
           store patch)
  | [ "factor-identical"; project_or_store ] ->
      let store = Protoss.Workspace.store_of_arg project_or_store in
      let checked = Protoss.Store.load_program store |> Protoss.Kernel.check_program in
      print_string (Protoss.Agent_protocol.factor_identical_json checked)
  | [ "factor-identical"; project_or_store; "--out"; out ] ->
      let store = Protoss.Workspace.store_of_arg project_or_store in
      let checked = Protoss.Store.load_program store |> Protoss.Kernel.check_program in
      Protoss.Store.write_file_atomic out (Protoss.Agent_protocol.factor_identical_patch_json checked);
      print_string (Protoss.Agent_protocol.factor_identical_json checked)
  | [ "compare-candidates"; project_or_store; left; right ] ->
      let store = Protoss.Workspace.store_of_arg project_or_store in
      print_string (Protoss.Agent_protocol.compare_candidates_json store left right)
  | [ "synthesize-tests"; project_or_store ] ->
      let store = Protoss.Workspace.store_of_arg project_or_store in
      let checked = Protoss.Store.load_program store |> Protoss.Kernel.check_program in
      print_string (Protoss.Agent_protocol.synthesize_tests_json checked)
  | [ "generate-migration"; old_project_or_store; new_project_or_store ] ->
      let old_store = Protoss.Workspace.store_of_arg old_project_or_store in
      let new_store = Protoss.Workspace.store_of_arg new_project_or_store in
      print_string (Protoss.Agent_protocol.generate_migration_json old_store new_store)
  | "graph" :: "--store-graph" :: project_or_store :: graph_hash :: args ->
      let store = Protoss.Workspace.store_of_arg project_or_store in
      let source =
        Protoss.Canonical_ir.agent_graph_source "store-graph"
          (project_or_store ^ "#" ^ graph_hash)
      in
      command_agent_graph_query source (Protoss.Workspace.graph_store store graph_hash) args
  | "graph" :: file :: args ->
      let source = Protoss.Canonical_ir.agent_graph_source "graph-file" file in
      command_agent_graph_query source (Protoss.Store.read_file file) args
  | [ "explain"; "--store-graph"; project_or_store; graph_hash; id ] ->
      let store = Protoss.Workspace.store_of_arg project_or_store in
      let source =
        Protoss.Canonical_ir.agent_graph_source "store-graph"
          (project_or_store ^ "#" ^ graph_hash)
      in
      print_string
        (Protoss.Canonical_ir.agent_graph_definition_explanation_json ~source
           (Protoss.Workspace.graph_store store graph_hash) id)
  | [ "explain"; file; id ] ->
      let source = Protoss.Canonical_ir.agent_graph_source "graph-file" file in
      print_string
        (Protoss.Canonical_ir.agent_graph_definition_explanation_json ~source
           (Protoss.Store.read_file file) id)
  | _ -> usage ()

let command_mcp = function
  | [ "serve" ] -> Protoss.Mcp_server.serve_stdio ()
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
  | [ "--list" ] -> print_endline (Protoss.Public_error.list_text ())
  | [ code ] -> print_endline (Protoss.Public_error.explain code)
  | _ -> usage ()

let command_grammar = function
  | [ "kernel" ] -> print_string Protoss.Kernel.executable_grammar_text
  | [ "human" ] -> print_string Protoss.Surface_syntax.human_grammar_text
  | _ -> usage ()

let command_spec = function
  | [ "check" ] ->
      print_string (Protoss.Spec_audit.report_text (Protoss.Spec_audit.check_file "protoss-spec.md"))
  | [ "check"; file ] ->
      print_string (Protoss.Spec_audit.report_text (Protoss.Spec_audit.check_file file))
  | _ -> usage ()

(* ---- Self-hosted frontend bridge ----------------------------------------
   Runs the Protoss-implemented frontend (stdlib/prelude.protoss) through the
   normal evaluator: the target source is spliced into a driver definition that
   applies a stdlib function to it, the combined program is checked, and the
   result is evaluated. OCaml stays the trusted kernel (parse/check/normalize);
   the *report* is produced by Protoss code from the prelude. *)
let read_source path =
  let ic = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () -> really_input_string ic (in_channel_length ic))

let self_cache_enabled () =
  match Sys.getenv_opt "PROTOSS_SELF_CACHE" with
  | Some v ->
      let v = String.lowercase_ascii (String.trim v) in
      not (v = "0" || v = "false" || v = "off" || v = "no")
  | None -> true

let self_cache_root () =
  match Sys.getenv_opt "PROTOSS_SELF_CACHE_DIR" with
  | Some dir when String.trim dir <> "" -> dir
  | _ -> Filename.concat "target" "self-cache"

let self_binary_digest () =
  try
    if Sys.file_exists Sys.executable_name && not (Sys.is_directory Sys.executable_name)
    then Digest.to_hex (Digest.file Sys.executable_name)
    else Sys.executable_name
  with _ -> Sys.executable_name

let self_string_cache_key ~prelude ~fn ~source =
  Protoss.Kernel.hash_string
    ("self-string-cache-v1\nbinary=" ^ self_binary_digest () ^ "\nprelude="
   ^ Protoss.Kernel.hash_string prelude ^ "\nfn=" ^ fn ^ "\nsource="
   ^ Protoss.Kernel.hash_string source ^ "\n")

let self_checked_prelude_cache_key ~prelude =
  Protoss.Kernel.hash_string
    ("self-checked-prelude-v1\nbinary=" ^ self_binary_digest () ^ "\nprelude="
   ^ Protoss.Kernel.hash_string prelude ^ "\n")

let self_cache_path key = Filename.concat (self_cache_root ()) (key ^ ".txt")

let self_checked_prelude_cache_path key =
  Filename.concat (self_cache_root ()) (key ^ ".checked")

let self_driver_checked_from_prelude ~prelude ~typ driver_expr =
  let driver = prelude ^ "\n(def __self_result " ^ typ ^ " (" ^ driver_expr ^ "))\n" in
  Protoss.Parser.parse_string driver |> Protoss.Kernel.check_program

let self_driver_checked ~typ driver_expr =
  self_driver_checked_from_prelude ~prelude:(read_source (prelude_path ())) ~typ
    driver_expr

let self_eval ~typ driver_expr =
  let checked = self_driver_checked ~typ driver_expr in
  let value, _ = Protoss.Runtime.eval_entry checked "__self_result" in
  (checked, value)

let self_apply_string_with_prelude ~prelude fn source =
  let checked =
    let cache_key = self_checked_prelude_cache_key ~prelude in
    let cache_path = self_checked_prelude_cache_path cache_key in
    let build () = Protoss.Parser.parse_string prelude |> Protoss.Kernel.check_program in
    if self_cache_enabled () && Sys.file_exists cache_path then
      try Marshal.from_string (Protoss.Store.read_file cache_path) 0
      with _ ->
        let checked = build () in
        Protoss.Store.write_file_atomic cache_path (Marshal.to_string checked []);
        checked
    else
      let checked = build () in
      if self_cache_enabled () then
        Protoss.Store.write_file_atomic cache_path (Marshal.to_string checked []);
      checked
  in
  let fn_value, _ = Protoss.Runtime.eval_entry ~stdlib_fast_paths:true checked fn in
  (checked, Protoss.Runtime.apply ~stdlib_fast_paths:true checked fn_value
              (Protoss.Runtime.VString source))

(* String report produced by a Protoss [String -> String] frontend function. *)
let self_string fn file =
  let source = read_source file in
  let prelude = read_source (prelude_path ()) in
  let cache_key = self_string_cache_key ~prelude ~fn ~source in
  let cache_path = self_cache_path cache_key in
  if self_cache_enabled () && Sys.file_exists cache_path then Protoss.Store.read_file cache_path
  else
    let _, value =
      self_apply_string_with_prelude ~prelude fn source
    in
    let text =
      match value with
      | Protoss.Runtime.VString s -> s
      | other -> Protoss.Runtime.value_to_string other
    in
    if self_cache_enabled () then Protoss.Store.write_file_atomic cache_path text;
    text

(* Content-addressed DefId, computed by the trusted kernel, of a stdlib def. *)
let self_def_id name =
  let checked, _ = self_eval ~typ:"String" "(Json.render (variant JNull unit))" in
  match
    List.find_opt
      (fun (d : Protoss.Kernel.checked_def) -> String.equal d.def.name name)
      checked.defs
  with
  | Some d -> d.def_id
  | None -> "-"

let command_self_fmt check file =
  let source = read_source file in
  let _, value =
    self_eval ~typ:"(Result String String)"
      ("Protoss.formatText " ^ Protoss.Ast.quote source)
  in
  match value with
  | Protoss.Runtime.VVariant (_, "Ok", Protoss.Runtime.VString formatted) ->
      if check then
        if String.equal formatted source then print_endline (file ^ ": formatted")
        else (
          prerr_endline (file ^ ": needs formatting");
          exit 1)
      else print_string formatted
  | Protoss.Runtime.VVariant (_, "Err", Protoss.Runtime.VString msg) ->
      print_error "self fmt error" msg
  | other ->
      print_error "self fmt error"
        ("unexpected result: " ^ Protoss.Runtime.value_to_string other)

let command_self_static json file =
  if json then (
    let def_id = self_def_id "Protoss.staticReportText" in
    let _, value =
      self_eval ~typ:"String"
        ("((Protoss.selfStaticJson " ^ Protoss.Ast.quote def_id ^ ") "
        ^ Protoss.Ast.quote (read_source file)
        ^ ")")
    in
    match value with
    | Protoss.Runtime.VString s -> print_endline s
    | other -> print_endline (Protoss.Runtime.value_to_string other))
  else print_endline (self_string "Protoss.selfStaticText" file)

let json_field name json = Protoss.Json.field name json

let json_string_field name json =
  match json_field name json with
  | Some value -> (
      match Protoss.Json.string value with Some s -> s | None -> "")
  | None -> ""

let self_typecheck_json file = self_string "Protoss.tcTextJson" file

let self_typecheck_value file = Protoss.Json.parse (self_typecheck_json file)

let command_self_typecheck json file =
  let text = self_typecheck_json file in
  if json then (
    print_endline text;
    match json_string_field "status" (Protoss.Json.parse text) with
    | "error" -> exit 1
    | _ -> ())
  else
    let value = Protoss.Json.parse text in
    match json_string_field "status" value with
    | "ok" -> print_endline "Self typecheck OK"
    | "error" ->
        let msg =
          match json_field "error" value with
          | Some err ->
              let code = json_string_field "code" err in
              let message = json_string_field "message" err in
              if String.equal code "" then message else code ^ ": " ^ message
          | None -> text
        in
        print_error "self typecheck error" msg
    | _ -> print_endline text

let command_self_type_of file entry =
  let value = self_typecheck_value file in
  match json_string_field "status" value with
  | "error" -> print_error "self typecheck error" (self_typecheck_json file)
  | _ -> (
      match
        match json_field "definitions" value with
        | Some definitions -> Protoss.Json.array definitions
        | None -> None
      with
      | None -> print_error "self type-of error" "missing definitions in self report"
      | Some defs -> (
          let rec find = function
            | [] -> None
            | def :: rest ->
                if String.equal (json_string_field "name" def) entry then
                  Some (json_string_field "type" def)
                else find rest
          in
          match find defs with
          | Some typ when not (String.equal typ "") -> print_endline typ
          | _ -> print_error "self type-of error" ("unknown entry: " ^ entry)))

let json_string_array_field name json =
  match json_field name json with
  | Some value -> (
      match Protoss.Json.array value with
      | Some xs ->
          List.filter_map
            (fun item ->
              match Protoss.Json.string item with Some s -> Some s | None -> None)
            xs
      | None -> [])
  | None -> []

let json_definition_pairs json =
  let defs =
    match json_field "definitions" json with
    | Some value -> ( match Protoss.Json.array value with Some xs -> xs | None -> [] )
    | None -> []
  in
  defs
  |> List.filter_map (fun def ->
         let name = json_string_field "name" def in
         let typ = json_string_field "type" def in
         if String.equal name "" then None else Some (name, typ))
  |> List.sort (fun (a, _) (b, _) -> String.compare a b)

let ocaml_declared_definition_pairs file =
  let program = Protoss.Parser.parse_string (read_source file) in
  program.defs
  |> List.filter (fun (def : Protoss.Ast.def) ->
         def.type_params = [] && Option.is_none def.declared_capabilities)
  |> List.map (fun (def : Protoss.Ast.def) -> (def.name, Protoss.Ast.string_of_typ def.typ))
  |> List.sort (fun (a, _) (b, _) -> String.compare a b)

let render_definition_pairs pairs =
  pairs
  |> List.map (fun (name, typ) -> name ^ ":" ^ typ)
  |> String.concat ","

let command_self_compare_typecheck file =
  let ocaml_ok =
    try
      ignore (parse_and_check file);
      true
    with _ -> false
  in
  let value = self_typecheck_value file in
  let self_ok = String.equal (json_string_field "status" value) "ok" in
  if not (Bool.equal ocaml_ok self_ok) then (
    prerr_endline
      ("self compare-typecheck mismatch: ocaml="
      ^ string_of_bool ocaml_ok ^ " self=" ^ string_of_bool self_ok);
    exit 1)
  else if ocaml_ok then
    let unsupported = json_string_array_field "unsupported" value in
    if unsupported <> [] then
      print_endline
        ("Self typecheck parity OK (unsupported: "
        ^ String.concat "," unsupported ^ ")")
    else
      let ocaml_defs = ocaml_declared_definition_pairs file in
      let self_defs = json_definition_pairs value in
      if ocaml_defs = self_defs then print_endline "Self typecheck parity OK"
      else (
        prerr_endline
          ("self compare-typecheck definitions mismatch: ocaml="
          ^ render_definition_pairs ocaml_defs ^ " self="
          ^ render_definition_pairs self_defs);
        exit 1)
  else print_endline "Self typecheck parity OK"

(* The trusted kernel checks the program first and supplies every DefId:
   identity always comes from the kernel, while the Protoss component only
   produces a canonical-text candidate that --compare verifies byte-for-byte
   against [Kernel.serialize_checked_program]. *)
let command_self_canon compare file =
  let source = read_source file in
  let source =
    if Protoss.Elm_syntax.looks_like source then Protoss.Elm_syntax.to_sexp_source source
    else source
  in
  let program = Protoss.Parser.parse_string source in
  let checked = Protoss.Kernel.check_program program in
  let expected = Protoss.Kernel.serialize_checked_program checked in
  let def_ids =
    checked.defs
    |> List.map (fun (d : Protoss.Kernel.checked_def) ->
           "(" ^ d.def.name ^ " " ^ d.def_id ^ ")")
    |> String.concat " "
  in
  let _, value =
    self_eval ~typ:"(Result String String)"
      ("((Protoss.canonProgramText " ^ Protoss.Ast.quote def_ids ^ ") "
      ^ Protoss.Ast.quote source ^ ")")
  in
  match value with
  | Protoss.Runtime.VVariant (_, "Ok", Protoss.Runtime.VString text) ->
      if not compare then print_endline text
      else if String.equal text expected then
        print_endline "Self canonicalizer parity OK"
      else (
        prerr_endline "self canon parity mismatch:";
        prerr_endline ("kernel: " ^ expected);
        prerr_endline ("self:   " ^ text);
        exit 1)
  | Protoss.Runtime.VVariant (_, "Err", Protoss.Runtime.VString msg) ->
      print_error "self canon error" msg
  | other ->
      print_error "self canon error"
        ("unexpected result: " ^ Protoss.Runtime.value_to_string other)

(* The self-hosted patch validator (Protoss.patchValidate) on the store's
   program text + the patch JSON. --compare checks its accept/reject verdict
   against the trusted kernel's Patch.check on the supported op fragment. *)
let command_self_patch_check compare store_or_project patch_path =
  let store = Protoss.Workspace.store_of_arg store_or_project in
  let program_text = Protoss.Ast.string_of_program (Protoss.Store.load_program store) in
  let patch_text = read_source patch_path in
  let _, value =
    self_eval ~typ:"(Result String String)"
      ("((Protoss.patchValidate " ^ Protoss.Ast.quote program_text ^ ") "
      ^ Protoss.Ast.quote patch_text ^ ")")
  in
  let component_ok = match value with Protoss.Runtime.VVariant (_, "Ok", _) -> true | _ -> false in
  if not compare then
    match value with
    | Protoss.Runtime.VVariant (_, "Ok", Protoss.Runtime.VString s) -> print_endline ("valid: " ^ s)
    | Protoss.Runtime.VVariant (_, "Err", Protoss.Runtime.VString m) ->
        print_endline ("rejected: " ^ m)
    | other -> print_endline (Protoss.Runtime.value_to_string other)
  else
    let kernel_ok = try ignore (Protoss.Patch.check store patch_path); true with _ -> false in
    if Bool.equal kernel_ok component_ok then
      print_endline ("Self patch-validator parity OK (" ^ (if kernel_ok then "accepted" else "rejected") ^ ")")
    else (
      prerr_endline
        (Printf.sprintf "self patch-check parity mismatch: kernel=%b self=%b" kernel_ok component_ok);
      exit 1)

(* The self-hosted normalizer (Protoss.normalizeText) on the supported
   fold/lambda-free fragment. --compare checks byte-for-byte against the kernel
   normal form (Runtime.normalize_all). Outside the fragment it returns Err. *)
let command_self_nf compare file =
  let source = read_source file in
  let _, value =
    self_eval ~typ:"(Result String String)" ("Protoss.normalizeText " ^ Protoss.Ast.quote source)
  in
  match value with
  | Protoss.Runtime.VVariant (_, "Ok", Protoss.Runtime.VString text) ->
      if not compare then print_string text
      else
        let checked = parse_and_check file in
        let kernel_text =
          Protoss.Runtime.normalize_all checked
          |> List.map (fun (n, v) -> n ^ " = " ^ Protoss.Runtime.value_to_string v ^ "\n")
          |> String.concat ""
        in
        if String.equal text kernel_text then print_endline "Self normalizer parity OK"
        else (
          prerr_endline "self nf parity mismatch:";
          prerr_endline ("kernel: " ^ kernel_text);
          prerr_endline ("self:   " ^ text);
          exit 1)
  | Protoss.Runtime.VVariant (_, "Err", Protoss.Runtime.VString msg) ->
      print_error "self nf error" msg
  | other ->
      print_error "self nf error" ("unexpected result: " ^ Protoss.Runtime.value_to_string other)

let command_self = function
  | [ "parse"; file ] -> print_endline (self_string "Protoss.selfParseJson" file)
  | [ "fmt"; "--check"; file ] -> command_self_fmt true file
  | [ "fmt"; file ] -> command_self_fmt false file
  | [ "resolve"; file ] -> print_endline (self_string "Protoss.selfResolveJson" file)
  | [ "deps"; file ] -> print_endline (self_string "Protoss.selfDepsJson" file)
  | [ "capabilities"; file ] ->
      print_endline (self_string "Protoss.selfCapabilitiesJson" file)
  | [ "static"; file; "--json" ] | [ "static"; "--json"; file ] ->
      command_self_static true file
  | [ "static"; file ] -> command_self_static false file
  | [ "typecheck"; file; "--json" ] | [ "typecheck"; "--json"; file ] ->
      command_self_typecheck true file
  | [ "typecheck"; file ] -> command_self_typecheck false file
  | [ "type-of"; file; "--entry"; entry ] -> command_self_type_of file entry
  | [ "compare-typecheck"; file ] -> command_self_compare_typecheck file
  | [ "canon"; "--compare"; file ] | [ "canon"; file; "--compare" ] ->
      command_self_canon true file
  | [ "canon"; file ] -> command_self_canon false file
  | [ "patch-check"; "--compare"; store; patch ] | [ "patch-check"; store; patch; "--compare" ] ->
      command_self_patch_check true store patch
  | [ "patch-check"; store; patch ] -> command_self_patch_check false store patch
  | [ "nf"; "--compare"; file ] | [ "nf"; file; "--compare" ] -> command_self_nf true file
  | [ "nf"; file ] -> command_self_nf false file
  | _ -> usage ()

let command_bench = function
  | [ "build"; project ] ->
      let start = Unix.gettimeofday () in
      let manifest = Protoss.Workspace.parse_manifest (Protoss.Workspace.project_root project) in
      let result = Protoss.Workspace.build manifest in
      let elapsed = Unix.gettimeofday () -. start in
      let stats = Protoss.Workspace.stats_to_string result.stats in
      let content =
        Protoss.Benchmark.report_content ~kind:"build" ~subject:project
          ~build_id:result.build_id ~seconds:elapsed ~stats
      in
      let benchmark_ref = Protoss.Benchmark.write_report result.store content in
      Printf.printf "benchmark-ref=%s\n%s" benchmark_ref content
  | _ -> usage ()

(* Treat a text edit as a view: derive a structured patch candidate from the
   difference between two store/project versions (import), or explain that diff. *)
let command_edit = function
  | [ "import"; a; b ] ->
      print_string
        (Protoss.Workspace.patch_from_diff
           (Protoss.Workspace.store_of_arg a) (Protoss.Workspace.store_of_arg b))
  | [ "explain"; a; b ] ->
      print_string
        (Protoss.Workspace.diff_to_text
           (Protoss.Workspace.diff
              (Protoss.Workspace.store_of_arg a) (Protoss.Workspace.store_of_arg b)))
  | _ -> usage ()

(* Compile a program to VM bytecode (hash of the deterministic module), or run
   an entry on the VM — which executes at parity with the interpreter. *)
let command_bytecode = function
  | [ file ] ->
      let checked = parse_and_check file in
      print_endline (Protoss.Bytecode.hash_module (Protoss.Bytecode.compile_checked checked))
  | [ "run"; file; "--entry"; entry ] | [ "run"; "--entry"; entry; file ] ->
      let checked = parse_and_check file in
      print_endline (Protoss.Runtime.value_to_string (Protoss.Bytecode_vm.exec_checked checked entry))
  | [ "exec"; ptvm; "--entry"; entry ] | [ "exec"; "--entry"; entry; ptvm ] ->
      (* Execute a built `.ptvm` bytecode module directly, without the source:
         decode the module and run the named def on the VM. Globals resolve among
         the module's own defs; the capability scope starts empty, so this is
         exact on the pure fragment (use `bytecode run <src>` for effectful defs
         that need the checked program's declared capabilities). *)
      let m = Protoss.Bytecode.decode_module (Protoss.Store.read_file ptvm) in
      print_endline (Protoss.Runtime.value_to_string (Protoss.Bytecode_vm.exec_module m entry))
  | _ -> usage ()

let command_doctor = function
  | [ "--v1" ] -> exit (Protoss.Doctor.run ~json:false)
  | [ "--v1"; "--json" ] | [ "--v1"; "json" ] -> exit (Protoss.Doctor.run ~json:true)
  | _ -> usage ()

let () =
  protect (fun () ->
      match Array.to_list Sys.argv |> List.tl with
      | [ "parse"; file ] -> command_parse file
      | [ "check"; "--graph"; file ] -> command_check_graph file
      | [ "check"; "--store-graph"; project_or_store; graph_hash ] ->
          command_check_store_graph project_or_store graph_hash
      | [ "check"; file ] -> command_check file
      | [ "nf"; "--graph"; file ] -> command_nf_graph file
      | [ "nf"; "--store-graph"; project_or_store; graph_hash ] ->
          command_nf_store_graph project_or_store graph_hash
      | [ "nf"; file ] -> command_nf file
      | [ "hash"; "--graph"; file ] -> command_hash_graph file
      | [ "hash"; "--store-graph"; project_or_store; graph_hash ] ->
          command_hash_store_graph project_or_store graph_hash
      | [ "hash"; file ] -> command_hash file
      | "compare" :: args -> command_compare args
      | "capabilities" :: args -> command_capabilities args
      | "duplicates" :: args -> command_duplicates args
      | "termination" :: args -> command_termination args
      | [ "canon"; "--version" ] -> print_endline Protoss.Kernel.canonical_version
      | [ "canon"; "--ptb"; file ] -> command_canon_ptb file
      | [ "canon"; "--graph"; file ] -> command_canon_graph file
      | [ "canon"; "--from-graph"; file ] -> command_canon_from_graph file
      | [ "canon"; "--migrate-graph"; file ] -> command_canon_migrate_graph file
      | [ "canon"; file ] -> command_canon file
      | "convert" :: args -> command_convert args
      | "eval" :: "--graph" :: file :: args -> command_eval_graph file args
      | "eval" :: "--store-graph" :: project_or_store :: graph_hash :: args ->
          command_eval_store_graph project_or_store graph_hash args
      | "eval" :: file :: args -> command_eval file args
      | "run" :: "--graph" :: file :: args -> command_run_graph file args
      | "run" :: "--store-graph" :: project_or_store :: graph_hash :: args ->
          command_run_store_graph project_or_store graph_hash args
      | "run" :: file :: args -> command_run file args
      | "resume" :: "--graph" :: file :: args -> command_resume_graph file args
      | "resume" :: "--store-graph" :: project_or_store :: graph_hash :: args ->
          command_resume_store_graph project_or_store graph_hash args
      | "resume" :: file :: args -> command_resume file args
      | "world" :: args -> command_world args
      | "ledger" :: args -> command_ledger args
      | "app" :: args -> command_app args
      | "web" :: args -> command_web args
      | "live" :: args -> command_live args
      | "runtime" :: args -> command_runtime args
      | "harness" :: args -> command_harness args
      | "self" :: args -> command_self args
      | "project" :: args -> command_project args
      | "init" :: args -> command_project ("init" :: args)
      | "build" :: args -> command_project_build args
      | "patch" :: args -> command_patch args
      | "diff" :: args -> command_diff args
      | "audit" :: args -> command_audit args
      | "git" :: args -> command_git args
      | "invariants" :: args -> command_invariants args
      | "fmt" :: args -> command_fmt args
      | "graph" :: args -> command_graph args
      | "agent" :: args -> command_agent args
      | "mcp" :: args -> command_mcp args
      | [ "repl" ] -> command_repl ()
      | "explain" :: args -> command_explain args
      | "grammar" :: args -> command_grammar args
      | "spec" :: args -> command_spec args
      | "bytecode" :: args -> command_bytecode args
      | "edit" :: args -> command_edit args
      | "doctor" :: args -> command_doctor args
      | "bench" :: args -> command_bench args
      | "cache" :: args -> command_cache args
      | "store" :: args -> command_store args
      | _ -> usage ())
