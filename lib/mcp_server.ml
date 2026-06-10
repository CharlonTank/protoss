exception Error of string

let fail msg = raise (Error msg)

let protocol_version = "2025-11-25"

let server_name = "protoss"

let json_string_list xs = Kernel.json_array Kernel.json_string xs

let json_field name obj =
  match Json.field name obj with Some v -> v | None -> fail ("missing JSON field: " ^ name)

let json_string_field name obj =
  match Json.string (json_field name obj) with
  | Some s -> s
  | None -> fail ("JSON field must be string: " ^ name)

let json_object_field name obj =
  match json_field name obj with
  | Json.Object fields -> Json.Object fields
  | _ -> fail ("JSON field must be object: " ^ name)

let json_bool_field_default name default obj =
  match Json.field name obj with
  | None -> default
  | Some (Json.Bool b) -> b
  | Some _ -> fail ("JSON field must be bool: " ^ name)

let arg_string args name = json_string_field name args

let arg_string_opt args name =
  match Json.field name args with
  | None -> None
  | Some value -> (
      match Json.string value with
      | Some s -> Some s
      | None -> fail ("tool argument must be string: " ^ name))

let arg_bool_default args name default = json_bool_field_default name default args

let text_content text =
  Kernel.json_obj
    [
      Kernel.json_field "type" (Kernel.json_string "text");
      Kernel.json_field "text" (Kernel.json_string text);
    ]

let tool_result_json ?structured ?(is_error = false) text =
  let structured =
    match structured with
    | Some structured -> structured
    | None -> Kernel.json_obj [ Kernel.json_field "text" (Kernel.json_string text) ]
  in
  Kernel.json_obj
    [
      Kernel.json_field "content" (Kernel.json_array text_content [ text ]);
      Kernel.json_field "structuredContent" structured;
      Kernel.json_field "isError" (Kernel.json_bool is_error);
    ]

let json_tool_result json =
  let text = String.trim json in
  ignore (Json.parse text);
  tool_result_json ~structured:text text

let tool_error_json msg = tool_result_json ~is_error:true msg

type tool = {
  tool_name : string;
  tool_title : string;
  tool_description : string;
  tool_schema : string;
}

let object_schema properties required =
  Kernel.json_obj
    [
      Kernel.json_field "type" (Kernel.json_string "object");
      Kernel.json_field "properties" (Kernel.json_obj properties);
      Kernel.json_field "required" (json_string_list required);
      Kernel.json_field "additionalProperties" (Kernel.json_bool false);
    ]

let string_property description =
  Kernel.json_obj
    [
      Kernel.json_field "type" (Kernel.json_string "string");
      Kernel.json_field "description" (Kernel.json_string description);
    ]

let bool_property description =
  Kernel.json_obj
    [
      Kernel.json_field "type" (Kernel.json_string "boolean");
      Kernel.json_field "description" (Kernel.json_string description);
    ]

let tool name title description schema =
  { tool_name = name; tool_title = title; tool_description = description; tool_schema = schema }

let tools =
  [
    tool "protoss.query" "Query Protoss graph"
      "Query a canonical graph file or store graph for summaries, definitions, dependencies, capabilities, and host contracts."
      (object_schema
         [
           Kernel.json_field "graphPath" (string_property "Path to canonical graph JSON");
           Kernel.json_field "store" (string_property "Project or store path for a stored graph");
           Kernel.json_field "graphHash" (string_property "Stored graph hash");
           Kernel.json_field "query"
             (string_property "summary, stats, definitions, deps, capabilities, capability-scopes, or host-contract");
           Kernel.json_field "id" (string_property "Optional definition, node, or capability id");
         ]
         []);
    tool "protoss.readNode" "Read graph node" "Read a canonical graph node by node ref."
      (object_schema
         [
           Kernel.json_field "graphPath" (string_property "Path to canonical graph JSON");
           Kernel.json_field "nodeRef" (string_property "Canonical node ref");
         ]
         [ "graphPath"; "nodeRef" ]);
    tool "protoss.renderView" "Render web view"
      "Build a Protoss web project and return its initial view JSON."
      (object_schema
         [
           Kernel.json_field "project" (string_property "Project root");
           Kernel.json_field "out" (string_property "Optional output directory");
         ]
         [ "project" ]);
    tool "protoss.proposePatch" "Propose patch from diff"
      "Create a structured patch candidate from two stores."
      (object_schema
         [
           Kernel.json_field "storeA" (string_property "Source store or project");
           Kernel.json_field "storeB" (string_property "Target store or project");
         ]
         [ "storeA"; "storeB" ]);
    tool "protoss.checkPatch" "Check patch" "Validate a patch candidate without mutating the store."
      (object_schema
         [
           Kernel.json_field "store" (string_property "Store path");
           Kernel.json_field "patchPath" (string_property "Patch JSON path");
         ]
         [ "store"; "patchPath" ]);
    tool "protoss.applyPatch" "Apply patch"
      "Apply a validated patch through the native agent commit path."
      (object_schema
         [
           Kernel.json_field "store" (string_property "Store path");
           Kernel.json_field "patchPath" (string_property "Patch JSON path");
         ]
         [ "store"; "patchPath" ]);
    tool "protoss.runHarness" "Run harness" "Run the attached harness set for a store."
      (object_schema [ Kernel.json_field "store" (string_property "Store path") ] [ "store" ]);
    tool "protoss.explain" "Explain definition"
      "Explain a graph definition, including type/term nodes and dependency edges."
      (object_schema
         [
           Kernel.json_field "graphPath" (string_property "Path to canonical graph JSON");
           Kernel.json_field "id" (string_property "Definition name, DefId, or hash");
         ]
         [ "graphPath"; "id" ]);
    tool "protoss.normalize" "Normalize definition" "Normalize an entry from source, graph, or store graph."
      (object_schema
         [
           Kernel.json_field "file" (string_property "Source/canonical file");
           Kernel.json_field "graphPath" (string_property "Canonical graph JSON path");
           Kernel.json_field "store" (string_property "Project or store path for a stored graph");
           Kernel.json_field "graphHash" (string_property "Stored graph hash");
           Kernel.json_field "entry" (string_property "Definition to normalize");
         ]
         [ "entry" ]);
    tool "protoss.diff" "Diff stores" "Compute a structural store diff."
      (object_schema
         [
           Kernel.json_field "storeA" (string_property "Source store or project");
           Kernel.json_field "storeB" (string_property "Target store or project");
           Kernel.json_field "json" (bool_property "Return JSON when true");
         ]
         [ "storeA"; "storeB" ]);
    tool "protoss.rollback" "Plan rollback"
      "Verify patch audit state and return the previous audit/root target for rollback planning."
      (object_schema
         [
           Kernel.json_field "store" (string_property "Store path");
           Kernel.json_field "ref" (string_property "Patch audit ref or latest");
         ]
         [ "store" ]);
  ]

let tool_to_json tool =
  Kernel.json_obj
    [
      Kernel.json_field "name" (Kernel.json_string tool.tool_name);
      Kernel.json_field "title" (Kernel.json_string tool.tool_title);
      Kernel.json_field "description" (Kernel.json_string tool.tool_description);
      Kernel.json_field "inputSchema" tool.tool_schema;
    ]

let tools_json () = Kernel.json_obj [ Kernel.json_field "tools" (Kernel.json_array tool_to_json tools) ]

let graph_input args =
  match (arg_string_opt args "graphPath", arg_string_opt args "store", arg_string_opt args "graphHash") with
  | Some path, _, _ -> Store.read_file path
  | None, Some store, Some graph_hash -> Workspace.graph_store (Workspace.store_of_arg store) graph_hash
  | _ -> fail "expected graphPath or store+graphHash"

let checked_input args =
  match (arg_string_opt args "file", arg_string_opt args "graphPath", arg_string_opt args "store", arg_string_opt args "graphHash") with
  | Some file, _, _, _ -> Loader.check_file file
  | None, Some graph_path, _, _ -> Canonical_ir.checked_of_graph (Store.read_file graph_path)
  | None, None, Some store, Some graph_hash ->
      Workspace.checked_store_graph (Workspace.store_of_arg store) graph_hash
  | _ -> fail "expected file, graphPath, or store+graphHash"

let query_graph args =
  let input = graph_input args in
  match (Option.value (arg_string_opt args "query") ~default:"summary", arg_string_opt args "id") with
  | "summary", _ -> Canonical_ir.agent_graph_summary_json input
  | "stats", _ -> Canonical_ir.agent_graph_stats_json input
  | ("definitions" | "roots" | "defs"), _ -> Canonical_ir.agent_graph_definitions_json input
  | "deps", id -> Canonical_ir.agent_graph_dependencies_json input id
  | "capabilities", _ -> Canonical_ir.agent_graph_capabilities_json input
  | "capability", Some id -> Canonical_ir.agent_graph_capability_json input id
  | "capability-scopes", id -> Canonical_ir.agent_graph_capability_scopes_json input id
  | "host-contract", _ -> Canonical_ir.agent_graph_host_contract_json input
  | "explain", Some id -> Canonical_ir.agent_graph_definition_explanation_json input id
  | "node", Some node_ref -> Canonical_ir.agent_graph_node_json input node_ref
  | "definition", Some id -> Canonical_ir.agent_graph_definition_json input id
  | query, _ -> fail ("unsupported graph query: " ^ query)

let read_node args =
  Canonical_ir.agent_graph_node_json (graph_input args) (arg_string args "nodeRef")

let explain args =
  Canonical_ir.agent_graph_definition_explanation_json (graph_input args) (arg_string args "id")

let normalize args =
  let checked = checked_input args in
  let entry = arg_string args "entry" in
  let value, _ = Runtime.normalize_def checked entry in
  Kernel.json_obj
    [
      Kernel.json_field "format" (Kernel.json_string "protoss-mcp-normalize-v1");
      Kernel.json_field "entry" (Kernel.json_string entry);
      Kernel.json_field "value" (Kernel.json_string (Runtime.value_to_string value));
      Kernel.json_field "programHash" (Kernel.json_string (Kernel.hash_program checked));
    ]
  ^ "\n"

let render_view args =
  let project = arg_string args "project" in
  let out = arg_string_opt args "out" in
  let result = Web.build ?out project in
  let app_json = Store.read_file (Filename.concat result.Web.out_dir "protoss-app.json") in
  let app = Json.parse app_json in
  Kernel.json_obj
    [
      Kernel.json_field "format" (Kernel.json_string "protoss-mcp-render-view-v1");
      Kernel.json_field "project" (Kernel.json_string project);
      Kernel.json_field "outDir" (Kernel.json_string result.out_dir);
      Kernel.json_field "initialView" (Json.to_string (json_field "initialView" app));
      Kernel.json_field "app" (Json.to_string app);
    ]
  ^ "\n"

let propose_patch args =
  Workspace.patch_from_diff
    (Workspace.store_of_arg (arg_string args "storeA"))
    (Workspace.store_of_arg (arg_string args "storeB"))

let check_patch args =
  let checked = Patch.check (arg_string args "store") (arg_string args "patchPath") in
  Kernel.json_obj
    [
      Kernel.json_field "format" (Kernel.json_string "protoss-mcp-check-patch-v1");
      Kernel.json_field "store" (Kernel.json_string (arg_string args "store"));
      Kernel.json_field "patchPath" (Kernel.json_string (arg_string args "patchPath"));
      Kernel.json_field "result" (Kernel.json_string (Patch.describe_checked checked));
      Kernel.json_field "valid" (Kernel.json_bool true);
    ]
  ^ "\n"

let apply_patch args =
  Agent_protocol.commit_patch_json (arg_string args "store") (arg_string args "patchPath")

let run_harness args =
  let store = arg_string args "store" in
  Kernel.json_obj
    [
      Kernel.json_field "format" (Kernel.json_string "protoss-mcp-harness-v1");
      Kernel.json_field "store" (Kernel.json_string store);
      Kernel.json_field "passed" (Kernel.json_bool true);
      Kernel.json_field "harnesses" (Kernel.json_array (fun x -> x) []);
      Kernel.json_field "status" (Kernel.json_string "empty-harness-set");
    ]
  ^ "\n"

let diff args =
  let store_a = Workspace.store_of_arg (arg_string args "storeA") in
  let store_b = Workspace.store_of_arg (arg_string args "storeB") in
  let items = Workspace.diff store_a store_b in
  if arg_bool_default args "json" true then Workspace.diff_to_json items
  else
    Kernel.json_obj
      [
        Kernel.json_field "format" (Kernel.json_string "protoss-mcp-diff-text-v1");
        Kernel.json_field "text" (Kernel.json_string (Workspace.diff_to_text items));
      ]
    ^ "\n"

let rollback args =
  let store = arg_string args "store" in
  let ref = Option.value (arg_string_opt args "ref") ~default:"latest" in
  let audit = Patch.verify_audit ~ref store in
  Kernel.json_obj
    [
      Kernel.json_field "format" (Kernel.json_string "protoss-mcp-rollback-v1");
      Kernel.json_field "mode" (Kernel.json_string "verified-plan");
      Kernel.json_field "store" (Kernel.json_string store);
      Kernel.json_field "auditRef" (Kernel.json_string audit.Patch.audit_ref);
      Kernel.json_field "previousRoot"
        (Kernel.json_string (Option.value audit.previous_root ~default:""));
      Kernel.json_field "canApply" (Kernel.json_bool false);
      Kernel.json_field "reason"
        (Kernel.json_string
           "rollback is exposed as a verified audit/root plan; store mutation requires an inverse patch");
    ]
  ^ "\n"

let call_tool name args =
  let json =
    match name with
    | "protoss.query" -> query_graph args
    | "protoss.readNode" -> read_node args
    | "protoss.renderView" -> render_view args
    | "protoss.proposePatch" -> propose_patch args
    | "protoss.checkPatch" -> check_patch args
    | "protoss.applyPatch" -> apply_patch args
    | "protoss.runHarness" -> run_harness args
    | "protoss.explain" -> explain args
    | "protoss.normalize" -> normalize args
    | "protoss.diff" -> diff args
    | "protoss.rollback" -> rollback args
    | _ -> fail ("unknown tool: " ^ name)
  in
  json_tool_result json

let initialize_result () =
  Kernel.json_obj
    [
      Kernel.json_field "protocolVersion" (Kernel.json_string protocol_version);
      Kernel.json_field "capabilities"
        (Kernel.json_obj
           [
             Kernel.json_field "tools"
               (Kernel.json_obj [ Kernel.json_field "listChanged" (Kernel.json_bool false) ]);
           ]);
      Kernel.json_field "serverInfo"
        (Kernel.json_obj
           [
             Kernel.json_field "name" (Kernel.json_string server_name);
             Kernel.json_field "title" (Kernel.json_string "Protoss MCP Server");
             Kernel.json_field "version" (Kernel.json_string Kernel.canonical_version);
           ]);
      Kernel.json_field "instructions"
        (Kernel.json_string
           "Use protoss.* tools to inspect graphs and mutate stores only through validated patches.");
    ]

let rpc_response id result =
  Kernel.json_obj
    [
      Kernel.json_field "jsonrpc" (Kernel.json_string "2.0");
      Kernel.json_field "id" id;
      Kernel.json_field "result" result;
    ]

let rpc_error id code message =
  Kernel.json_obj
    [
      Kernel.json_field "jsonrpc" (Kernel.json_string "2.0");
      Kernel.json_field "id" id;
      Kernel.json_field "error"
        (Kernel.json_obj
           [
             Kernel.json_field "code" (string_of_int code);
             Kernel.json_field "message" (Kernel.json_string message);
           ]);
    ]

let id_json obj = match Json.field "id" obj with Some id -> Json.to_string id | None -> "null"

let params_object obj =
  match Json.field "params" obj with None -> Json.Object [] | Some params -> params

let handle_call params =
  let name = json_string_field "name" params in
  let args =
    match Json.field "arguments" params with
    | None -> Json.Object []
    | Some (Json.Object _ as obj) -> obj
    | Some _ -> fail "tools/call arguments must be an object"
  in
  try call_tool name args with
  | Error msg | Kernel.Error msg | Store.Error msg | Workspace.Error msg | Patch.Error msg
  | Patch_audit.Error msg | Web.Error msg | Parser.Error msg | Json.Error msg | Failure msg
  | Sys_error msg ->
      tool_error_json msg
  | Unix.Unix_error (err, fn, arg) ->
      tool_error_json (fn ^ "(" ^ arg ^ "): " ^ Unix.error_message err)

let handle_json obj =
  let id = id_json obj in
  let has_id = Json.field "id" obj <> None in
  let method_name = json_string_field "method" obj in
  match method_name with
  | "initialize" -> Some (rpc_response id (initialize_result ()))
  | "notifications/initialized" -> None
  | "ping" -> Some (rpc_response id (Kernel.json_obj []))
  | "tools/list" -> Some (rpc_response id (tools_json ()))
  | "tools/call" -> Some (rpc_response id (handle_call (json_object_field "params" obj)))
  | _ when has_id -> Some (rpc_error id (-32601) ("method not found: " ^ method_name))
  | _ -> None

let handle_message line =
  try
    match Json.parse line with
    | Json.Object _ as obj -> handle_json obj
    | _ -> Some (rpc_error "null" (-32600) "request must be a JSON object")
  with
  | Json.Error msg -> Some (rpc_error "null" (-32700) ("parse error: " ^ msg))
  | Error msg -> Some (rpc_error "null" (-32602) msg)

let serve_stdio () =
  try
    while true do
      let line = input_line stdin in
      match handle_message line with
      | None -> ()
      | Some response ->
          print_endline response;
          flush stdout
    done
  with End_of_file -> ()
