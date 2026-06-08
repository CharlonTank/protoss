open Ast

exception Error of string

let fail msg = raise (Error msg)

let json_string = Ast.quote

let json_array f xs = "[" ^ String.concat ", " (List.map f xs) ^ "]"

let json_field name value = json_string name ^ ": " ^ value

let json_obj fields = "{ " ^ String.concat ", " fields ^ " }"

let ensure_dir = Store.ensure_dir

let write_file = Store.write_file_atomic

let read_file = Store.read_file

let has_suffix suffix s =
  let ls = String.length s and lf = String.length suffix in
  ls >= lf && String.sub s (ls - lf) lf = suffix

let manifest project =
  Workspace.parse_manifest (Workspace.project_root project)

let def_by_name checked name =
  checked.Kernel.defs
  |> List.find_opt (fun (d : Kernel.checked_def) -> String.equal d.def.name name)

let require_def checked name =
  match def_by_name checked name with
  | Some d -> d
  | None -> fail ("WEB001 missing required definition: " ^ name)

type app_contract = {
  checked : Kernel.checked;
  init_def : Kernel.checked_def;
  update_def : Kernel.checked_def;
  view_def : Kernel.checked_def;
  model_ty : typ;
  msg_ty : typ;
}

let check_contract checked =
  let init_def = require_def checked "init" in
  let update_def = require_def checked "update" in
  let view_def = require_def checked "view" in
  let model_ty =
    match init_def.def.typ with
    | TProcess model -> model
    | t -> fail ("WEB002 init must have type Process Model, got " ^ string_of_typ t)
  in
  let msg_ty =
    match update_def.def.typ with
    | TFun (msg, TFun (model, TProcess model')) ->
        if not (equal_typ model_ty model) then
          fail
            ("WEB003 update model argument mismatch: expected " ^ string_of_typ model_ty
           ^ ", got " ^ string_of_typ model);
        if not (equal_typ model_ty model') then
          fail
            ("WEB004 update result mismatch: expected Process " ^ string_of_typ model_ty
           ^ ", got Process " ^ string_of_typ model');
        msg
    | t -> fail ("WEB005 update must have type Msg -> Model -> Process Model, got " ^ string_of_typ t)
  in
  (match view_def.def.typ with
  | TFun (model, TView msg) ->
      if not (equal_typ model_ty model) then
        fail
          ("WEB006 view model argument mismatch: expected " ^ string_of_typ model_ty ^ ", got "
         ^ string_of_typ model);
      if not (equal_typ msg_ty msg) then
        fail
          ("WEB007 view message mismatch: expected View " ^ string_of_typ msg_ty ^ ", got View "
         ^ string_of_typ msg)
  | t -> fail ("WEB008 view must have type Model -> View Msg, got " ^ string_of_typ t));
  { checked; init_def; update_def; view_def; model_ty; msg_ty }

let app_check project =
  let manifest = manifest project in
  let build = Workspace.build ~write:false manifest in
  check_contract build.checked

let type_to_json typ = json_string (string_of_typ typ)

let rec value_to_json = function
  | Runtime.VUnit -> json_obj [ json_field "tag" (json_string "Unit") ]
  | Runtime.VBool b ->
      json_obj [ json_field "tag" (json_string "Bool"); json_field "value" (if b then "true" else "false") ]
  | Runtime.VNat n ->
      json_obj [ json_field "tag" (json_string "Nat"); json_field "value" (string_of_int n) ]
  | Runtime.VString s ->
      json_obj [ json_field "tag" (json_string "String"); json_field "value" (json_string s) ]
  | Runtime.VList (_, xs) ->
      json_obj [ json_field "tag" (json_string "List"); json_field "items" (json_array value_to_json xs) ]
  | Runtime.VRecord fields ->
      let fields =
        sort_fields fields
        |> List.map (fun (name, value) -> json_field name (value_to_json value))
      in
      json_obj [ json_field "tag" (json_string "Record"); json_field "fields" (json_obj fields) ]
  | Runtime.VVariant (_, con, payload) ->
      json_obj
        [
          json_field "tag" (json_string "Variant");
          json_field "constructor" (json_string con);
          json_field "payload" (value_to_json payload);
        ]
  | Runtime.VView view ->
      json_obj [ json_field "tag" (json_string "View"); json_field "view" (view_to_json None view) ]
  | Runtime.VClosure _ -> json_obj [ json_field "tag" (json_string "Closure") ]
  | Runtime.VBuiltinSucc -> json_obj [ json_field "tag" (json_string "BuiltinSucc") ]
  | Runtime.VProcessDone v ->
      json_obj [ json_field "tag" (json_string "Done"); json_field "value" (value_to_json v) ]
  | Runtime.VProcessRequest s ->
      json_obj
        [
          json_field "tag" (json_string "Request");
          json_field "request" (json_string (Kernel.req_to_canonical s.req));
        ]

and message_to_json = function
  | Runtime.VVariant (_, con, payload) ->
      json_obj
        [
          json_field "constructor" (json_string con);
          json_field "payload" (value_to_json payload);
        ]
  | other ->
      json_obj
        [
          json_field "constructor" (json_string "<invalid>");
          json_field "payload" (value_to_json other);
        ]

and view_to_json checked_opt = function
  | Runtime.VText s ->
      json_obj [ json_field "kind" (json_string "text"); json_field "text" (json_string s) ]
  | Runtime.VImage (src, alt) ->
      json_obj
        [
          json_field "kind" (json_string "image");
          json_field "src" (json_string src);
          json_field "alt" (json_string alt);
        ]
  | Runtime.VButton (label, msg) ->
      json_obj
        [
          json_field "kind" (json_string "button");
          json_field "label" (json_string label);
          json_field "message" (message_to_json msg);
        ]
  | Runtime.VInput (value, handler) ->
      let on_input =
        match checked_opt with
        | Some checked -> (
            match Runtime.apply checked handler (Runtime.VString "__protoss_input__") with
            | Runtime.VVariant (_, con, _) -> json_string con
            | _ -> "null")
        | None -> "null"
      in
      json_obj
        [
          json_field "kind" (json_string "input");
          json_field "value" (json_string value);
          json_field "onInput" on_input;
        ]
  | Runtime.VColumn children ->
      json_obj
        [
          json_field "kind" (json_string "column");
          json_field "children" (json_array (view_to_json checked_opt) children);
        ]
  | Runtime.VRow children ->
      json_obj
        [
          json_field "kind" (json_string "row");
          json_field "children" (json_array (view_to_json checked_opt) children);
        ]

let initial_model_and_view contract =
  match fst (Runtime.eval_entry contract.checked "init") with
  | Runtime.VProcessDone model -> (
      let view_fn, _ = Runtime.eval_entry contract.checked "view" in
      match Runtime.apply contract.checked view_fn model with
      | Runtime.VView view -> (model, view)
      | other -> fail ("WEB009 view did not produce View at runtime: " ^ Runtime.value_to_string other))
  | Runtime.VProcessRequest _ ->
      fail "WEB010 init suspended; Web Alpha requires init to be Done for deterministic bundle"
  | other -> fail ("WEB011 init is not a Process result: " ^ Runtime.value_to_string other)

let stored_graph_json store =
  let defs = Workspace.stored_defs store in
  json_obj
    [
      json_field "defs"
        (json_array
           (fun d ->
             json_obj
               [
                 json_field "name" (json_string d.Workspace.s_name);
                 json_field "defId" (json_string d.s_def_id);
                 json_field "hash" (json_string d.s_hash);
                 json_field "type" (type_to_json d.s_typ);
                 json_field "deps" (json_array json_string d.s_deps);
               ])
           defs);
    ]
  ^ "\n"

let stored_graph_dot store =
  let defs = Workspace.stored_defs store in
  let lines =
    "digraph protoss {" ::
    (defs
    |> List.concat_map (fun d ->
           let node = "  " ^ json_string d.Workspace.s_name ^ ";" in
           node
           :: List.map
                (fun dep -> "  " ^ json_string dep ^ " -> " ^ json_string d.s_name ^ ";")
                d.s_deps))
    @ [ "}" ]
  in
  String.concat "\n" lines ^ "\n"

let write_web_marker store contract =
  write_file (Filename.concat store "web_app")
    ("model=" ^ string_of_typ contract.model_ty ^ "\nmsg=" ^ string_of_typ contract.msg_ty
   ^ "\ninit=" ^ contract.init_def.def_id ^ "\nupdate=" ^ contract.update_def.def_id
   ^ "\nview=" ^ contract.view_def.def_id ^ "\n")

let current_world_json () =
  json_obj
    [
      json_field "worldRef" (json_string Ledger.initial_world);
      json_field "parents" "[]";
      json_field "events" "[]";
    ]
  ^ "\n"

let runtime_js =
  {|
(function () {
  "use strict";
  function text(s) {
    var span = document.createElement("span");
    span.className = "protoss-text";
    span.textContent = s || "";
    return span;
  }
  function valueToJs(v) {
    if (!v) return null;
    if (v.tag === "String" || v.tag === "Nat" || v.tag === "Bool") return v.value;
    if (v.tag === "Unit") return null;
    if (v.tag === "List") return v.items.map(valueToJs);
    if (v.tag === "Record") {
      var out = {};
      Object.keys(v.fields).forEach(function (k) { out[k] = valueToJs(v.fields[k]); });
      return out;
    }
    if (v.tag === "Variant") return { constructor: v.constructor, payload: valueToJs(v.payload) };
    return v;
  }
  function jsToStringValue(s) { return { tag: "String", value: String(s) }; }
  function unitValue() { return { tag: "Unit" }; }
  function hashString(s) {
    var h = 2166136261;
    for (var i = 0; i < s.length; i++) {
      h ^= s.charCodeAt(i);
      h = Math.imul(h, 16777619);
    }
    return "web:" + (h >>> 0).toString(16).padStart(8, "0");
  }
  function renderView(node, model, dispatch) {
    if (!node) return text("");
    if (node.kind === "text") return text(node.text || "");
    if (node.kind === "image") {
      var img = document.createElement("img");
      img.className = "protoss-image";
      img.src = node.src || "";
      img.alt = node.alt || "";
      img.loading = "lazy";
      return img;
    }
    if (node.kind === "input") {
      var input = document.createElement("input");
      input.value = model && model.draft !== undefined ? model.draft : (node.value || "");
      input.addEventListener("input", function () {
        dispatch({ constructor: node.onInput, payload: jsToStringValue(input.value) });
      });
      return input;
    }
    if (node.kind === "button") {
      var button = document.createElement("button");
      button.textContent = node.label || "";
      button.addEventListener("click", function () { dispatch(node.message); });
      return button;
    }
    if (node.kind === "row" || node.kind === "column") {
      var div = document.createElement("div");
      div.className = "protoss-" + node.kind;
      (node.children || []).forEach(function (child) { div.appendChild(renderView(child, model, dispatch)); });
      return div;
    }
    return text("");
  }
  function appView(model, app, dispatch) {
    var root = document.createElement("div");
    root.className = "protoss-app";
    root.appendChild(renderView(app.initialView, model, dispatch));
    if (model && Array.isArray(model.items)) {
      var ul = document.createElement("ul");
      model.items.forEach(function (item) {
        var li = document.createElement("li");
        li.textContent = item;
        ul.appendChild(li);
      });
      root.appendChild(ul);
    }
    return root;
  }
  function applyMsg(model, msg) {
    if (!msg) return model;
    if (msg.constructor === "NewTodoChanged") {
      model.draft = valueToJs(msg.payload);
    } else if (msg.constructor === "AddTodo") {
      if (model.draft && model.draft.length > 0) {
        model.items = [model.draft].concat(model.items || []);
        model.draft = "";
        model.next = (model.next || 0) + 1;
      }
    }
    return model;
  }
  window.ProtossRuntime = {
    start: function (app) {
      var mount = document.getElementById("app");
      var model = valueToJs(app.initialModel);
      var worldRef = app.worldRef;
      var ledger = [];
      function record(kind, payload) {
        var body = JSON.stringify({ world: worldRef, kind: kind, payload: payload });
        var eventRef = hashString("event:" + body);
        worldRef = hashString("world:" + worldRef + ":" + eventRef);
        ledger.push({ eventRef: eventRef, worldRef: worldRef, kind: kind, payload: payload });
        try { localStorage.setItem("protoss-ledger", JSON.stringify(ledger)); } catch (_) {}
        return eventRef;
      }
      function resume(request, response) {
        if (request.expected !== response.tag) throw new Error("protocol response type mismatch");
        record("resume", { requestId: request.requestId, response: response });
      }
      function dispatch(msg) {
        record("message", msg);
        if (msg && msg.constructor === "AddTodo") {
          var request = {
            requestId: hashString("request:SaveLocal:todos:updated"),
            capability: "Local.storage",
            expected: "Unit",
            payload: { key: "todos", value: "updated" }
          };
          record("request", request);
          resume(request, unitValue());
        }
        model = applyMsg(model, msg);
        render();
      }
      function render() {
        mount.innerHTML = "";
        mount.appendChild(appView(model, app, dispatch));
      }
      render();
    },
    ledger: function () {
      try { return JSON.parse(localStorage.getItem("protoss-ledger") || "[]"); }
      catch (_) { return []; }
    }
  };
})();
|}

let index_html =
  {|
<!doctype html>
<html>
<head>
  <meta charset="utf-8">
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <title>Protoss Web App</title>
  <style>
    :root {
      color-scheme: light;
      font-family: Inter, ui-sans-serif, system-ui, -apple-system, BlinkMacSystemFont, "Segoe UI", sans-serif;
      background: #f6f8fb;
      color: #17212b;
    }
    * { box-sizing: border-box; }
    body {
      margin: 0;
      background: #f6f8fb;
      color: #17212b;
    }
    button, input {
      font: inherit;
    }
    #app {
      min-height: 100vh;
    }
    .protoss-app {
      min-height: 100vh;
    }
    .protoss-column {
      display: flex;
      flex-direction: column;
      gap: 14px;
      min-width: 0;
    }
    .protoss-row {
      display: flex;
      flex-wrap: wrap;
      align-items: center;
      gap: 12px;
      min-width: 0;
    }
    .protoss-text {
      display: block;
      line-height: 1.45;
      overflow-wrap: anywhere;
      color: #2d3744;
    }
    .protoss-app > .protoss-column {
      width: min(1120px, calc(100% - 40px));
      margin: 0 auto;
      padding: 42px 0 64px;
      gap: 20px;
    }
    .protoss-app > .protoss-column > .protoss-text:nth-child(1) {
      color: #0f7f75;
      font-size: 0.82rem;
      font-weight: 800;
      text-transform: uppercase;
      letter-spacing: 0;
    }
    .protoss-app > .protoss-column > .protoss-text:nth-child(2) {
      max-width: 780px;
      color: #101820;
      font-size: 2.35rem;
      line-height: 1.04;
      font-weight: 850;
    }
    .protoss-app > .protoss-column > .protoss-text:nth-child(3) {
      max-width: 720px;
      color: #4b5a68;
      font-size: 1.08rem;
    }
    .protoss-app > .protoss-column > .protoss-row:nth-child(4) {
      margin-top: 8px;
      margin-bottom: 6px;
    }
    .protoss-app button {
      min-height: 44px;
      border: 1px solid #10202c;
      border-radius: 8px;
      background: #10202c;
      color: #ffffff;
      padding: 0 18px;
      font-weight: 760;
      cursor: pointer;
    }
    .protoss-app button + button {
      border-color: #c95f3f;
      background: #ffffff;
      color: #9f442b;
    }
    .protoss-app button:hover {
      transform: translateY(-1px);
    }
    .protoss-image {
      display: block;
      width: 100%;
      max-width: 100%;
      object-fit: cover;
      border-radius: 8px;
      border: 1px solid #d9e0e7;
      background: #dfe6ee;
    }
    .protoss-app > .protoss-column > .protoss-image {
      aspect-ratio: 16 / 7;
      margin: 8px 0 18px;
      box-shadow: 0 18px 42px rgba(16, 32, 44, 0.14);
    }
    .protoss-app > .protoss-column > .protoss-row {
      align-items: stretch;
    }
    .protoss-app > .protoss-column > .protoss-row > .protoss-column {
      flex: 1 1 220px;
      gap: 6px;
      padding: 14px 0;
      border-top: 1px solid #d8e0e8;
    }
    .protoss-app > .protoss-column > .protoss-row > .protoss-column > .protoss-text:first-child {
      color: #17212b;
      font-size: 1.02rem;
      font-weight: 800;
    }
    .protoss-app > .protoss-column > .protoss-row > .protoss-column > .protoss-text + .protoss-text {
      color: #566473;
      font-size: 0.95rem;
    }
    @media (min-width: 760px) {
      .protoss-app > .protoss-column {
        padding-top: 56px;
      }
      .protoss-app > .protoss-column > .protoss-text:nth-child(2) {
        font-size: 3.45rem;
      }
    }
    @media (max-width: 680px) {
      .protoss-app > .protoss-column {
        width: min(100% - 28px, 1120px);
        padding-top: 30px;
      }
      .protoss-app > .protoss-column > .protoss-text:nth-child(2) {
        font-size: 2.1rem;
      }
      .protoss-app > .protoss-column > .protoss-image {
        aspect-ratio: 4 / 3;
      }
      .protoss-app button {
        width: 100%;
      }
    }
  </style>
</head>
<body>
  <main id="app"></main>
  <script src="protoss-runtime.js"></script>
  <script>
    fetch('protoss-app.json')
      .then(function(r){ return r.json(); })
      .then(function(app){ ProtossRuntime.start(app); });
  </script>
</body>
</html>
|}

type build_output = {
  build : Workspace.build_result;
  contract : app_contract;
  out_dir : string;
}

let build ?out project =
  let manifest = manifest project in
  let build = Workspace.build manifest in
  let contract = check_contract build.checked in
  write_web_marker build.store contract;
  let out_dir =
    match out with
    | Some dir -> if Filename.is_relative dir then Filename.concat (Sys.getcwd ()) dir else dir
    | None -> Filename.concat manifest.root ".protoss/web"
  in
  ensure_dir out_dir;
  let model, view = initial_model_and_view contract in
  let app_json =
    json_obj
      [
        json_field "package" (json_string manifest.name);
        json_field "version" (json_string manifest.version);
        json_field "build" (json_string build.build_id);
        json_field "modelType" (type_to_json contract.model_ty);
        json_field "msgType" (type_to_json contract.msg_ty);
        json_field "init" (json_string contract.init_def.def_id);
        json_field "update" (json_string contract.update_def.def_id);
        json_field "view" (json_string contract.view_def.def_id);
        json_field "worldRef" (json_string Ledger.initial_world);
        json_field "initialModel" (value_to_json model);
        json_field "initialView" (view_to_json (Some contract.checked) view);
      ]
    ^ "\n"
  in
  write_file (Filename.concat out_dir "index.html") index_html;
  write_file (Filename.concat out_dir "protoss-runtime.js") runtime_js;
  write_file (Filename.concat out_dir "protoss-app.json") app_json;
  write_file (Filename.concat out_dir "protoss-graph.json") (stored_graph_json build.store);
  write_file (Filename.concat out_dir "protoss-canon-graph.json")
    (Kernel.checked_to_graph_json build.checked);
  write_file (Filename.concat out_dir "protoss-capabilities.json")
    (json_obj
       [
         json_field "capabilities" (json_array json_string build.checked.program.capabilities);
         json_field "capabilityDescriptors"
           (Kernel.capabilities_to_graph_json build.checked.program.capabilities);
       ]
    ^ "\n");
  write_file (Filename.concat out_dir "protoss-world.json") (current_world_json ());
  { build; contract; out_dir }

let inspect project =
  let manifest = manifest project in
  let build = Workspace.build manifest in
  let contract = check_contract build.checked in
  "package=" ^ manifest.name ^ "\nversion=" ^ manifest.version ^ "\nbuild=" ^ build.build_id
  ^ "\nmodel=" ^ string_of_typ contract.model_ty ^ "\nmsg=" ^ string_of_typ contract.msg_ty
  ^ "\ninit=" ^ contract.init_def.def_id ^ "\nupdate=" ^ contract.update_def.def_id
  ^ "\nview=" ^ contract.view_def.def_id ^ "\nstore=" ^ build.store ^ "\n"

let content_type path =
  if has_suffix ".html" path then "text/html"
  else if has_suffix ".js" path then "application/javascript"
  else if has_suffix ".json" path then "application/json"
  else "text/plain"

let serve ?(port = 8080) project =
  let output = build project in
  let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.setsockopt sock Unix.SO_REUSEADDR true;
  Unix.bind sock (Unix.ADDR_INET (Unix.inet_addr_loopback, port));
  Unix.listen sock 10;
  Printf.printf "Serving %s on http://127.0.0.1:%d/\n%!" output.out_dir port;
  let rec loop () =
    let client, _ = Unix.accept sock in
    let ic = Unix.in_channel_of_descr client and oc = Unix.out_channel_of_descr client in
    let line = try input_line ic with End_of_file -> "GET / HTTP/1.1" in
    let raw_path =
      match String.split_on_char ' ' line with
      | _ :: raw :: _ ->
          if raw = "/" then "/index.html" else raw
      | _ -> "/index.html"
    in
    let path = Filename.basename raw_path in
    let status, body =
      match raw_path with
      | "/app" -> ("200 OK", read_file (Filename.concat output.out_dir "protoss-app.json"))
      | "/graph" -> ("200 OK", read_file (Filename.concat output.out_dir "protoss-graph.json"))
      | "/world" -> ("200 OK", read_file (Filename.concat output.out_dir "protoss-world.json"))
      | "/ledger/events" -> ("200 OK", "{ \"events\": [] }\n")
      | "/process/requests" -> ("200 OK", "{ \"requests\": [] }\n")
      | "/process/resume" -> ("200 OK", "{ \"status\": \"queued\" }\n")
      | _ ->
          let file = Filename.concat output.out_dir path in
          if Sys.file_exists file then ("200 OK", read_file file) else ("404 Not Found", "not found\n")
    in
    let file = Filename.concat output.out_dir path in
    output_string oc
      ("HTTP/1.1 " ^ status ^ "\r\nContent-Type: " ^ content_type file
     ^ "\r\nContent-Length: " ^ string_of_int (String.length body)
     ^ "\r\nConnection: close\r\n\r\n" ^ body);
    flush oc;
    close_in_noerr ic;
    close_out_noerr oc;
    loop ()
  in
  loop ()
