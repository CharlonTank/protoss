open Ast

exception Error of string

let fail msg = raise (Error msg)

let json_string = Ast.quote

let json_array f xs = "[" ^ String.concat ", " (List.map f xs) ^ "]"

let json_field name value = json_string name ^ ": " ^ value

let json_obj fields = "{ " ^ String.concat ", " fields ^ " }"

let ensure_dir = Store.ensure_dir_cached

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

(* Optional Lamdera-shaped backend half of a full-stack app (see
   docs/backend-architecture.md). Present iff the app defines `updateBackend`. *)
type backend_contract = {
  init_backend_def : Kernel.checked_def;
  update_backend_def : Kernel.checked_def;
  backend_model_ty : typ;
  to_backend_ty : typ;
  to_frontend_ty : typ;
}

type app_contract = {
  checked : Kernel.checked;
  init_def : Kernel.checked_def;
  update_def : Kernel.checked_def;
  view_def : Kernel.checked_def;
  model_ty : typ;
  msg_ty : typ;
  architecture : string;
  backend : backend_contract option;
}

let record_field name fields =
  List.find_opt (fun (field, _) -> String.equal field name) fields |> Option.map snd

let cmd_tuple_shape = function
  | TRecord fields -> (
      match (record_field "_1" fields, record_field "_2" fields) with
      | Some model, Some (TCmd (caps, msg)) -> Some (model, caps, msg)
      | _ -> None)
  | _ -> None

let require_cmd_capabilities expected actual context =
  if not (equal_process_capabilities expected actual) then
    fail
      (context ^ " command capability mismatch: expected " ^ string_of_typ (TCmd (expected, TUnit))
     ^ ", got " ^ string_of_typ (TCmd (actual, TUnit)))

(* Validate the optional backend half. Absent (`None`) iff no `updateBackend`
   def, so a frontend-only app is unaffected. updateBackend mirrors the cmd
   update shape, against the backend's own model/messages:
     initBackend   : BackendModel
     updateBackend : ToBackend -> BackendModel -> (Tuple BackendModel (Cmd caps ToFrontend)) *)
let check_backend_contract checked =
  match def_by_name checked "updateBackend" with
  | None -> None
  | Some update_backend_def ->
      let init_backend_def = require_def checked "initBackend" in
      let backend_model_ty = init_backend_def.def.typ in
      let to_backend_ty, to_frontend_ty =
        match update_backend_def.def.typ with
        | TFun (to_backend, TFun (backend_model, update_result)) -> (
            if not (equal_typ backend_model_ty backend_model) then
              fail
                ("WEB021 updateBackend model argument mismatch: expected "
               ^ string_of_typ backend_model_ty ^ ", got " ^ string_of_typ backend_model);
            match cmd_tuple_shape update_result with
            | Some (backend_model', _caps, to_frontend) ->
                if not (equal_typ backend_model_ty backend_model') then
                  fail
                    ("WEB022 updateBackend result model mismatch: expected "
                   ^ string_of_typ backend_model_ty ^ ", got " ^ string_of_typ backend_model');
                (to_backend, to_frontend)
            | None ->
                fail
                  ("WEB023 updateBackend must return (Tuple BackendModel (Cmd caps ToFrontend)), got "
                 ^ string_of_typ update_result))
        | t ->
            fail
              ("WEB024 updateBackend must have type ToBackend -> BackendModel -> (Tuple \
                BackendModel (Cmd caps ToFrontend)), got " ^ string_of_typ t)
      in
      Some { init_backend_def; update_backend_def; backend_model_ty; to_backend_ty; to_frontend_ty }

let check_contract checked =
  let init_def = require_def checked "init" in
  let update_def = require_def checked "update" in
  let view_def = require_def checked "view" in
  let model_ty, msg_ty, architecture =
    match init_def.def.typ with
    | TProcess (_, model_ty) ->
        let msg_ty =
          match update_def.def.typ with
          | TFun (msg, TFun (model, TProcess (_, model'))) ->
              if not (equal_typ model_ty model) then
                fail
                  ("WEB003 update model argument mismatch: expected " ^ string_of_typ model_ty
                 ^ ", got " ^ string_of_typ model);
              if not (equal_typ model_ty model') then
                fail
                  ("WEB004 update result mismatch: expected Process " ^ string_of_typ model_ty
                 ^ ", got Process " ^ string_of_typ model');
              msg
          | t ->
              fail
                ("WEB005 update must have type Msg -> Model -> Process Model, got "
               ^ string_of_typ t)
        in
        (model_ty, msg_ty, "process")
    | init_ty -> (
        match cmd_tuple_shape init_ty with
        | None ->
            fail
              ("WEB002 init must have type Process Model or (Tuple Model (Cmd caps Msg)), got "
             ^ string_of_typ init_ty)
        | Some (model_ty, init_caps, init_msg_ty) ->
            let msg_ty =
              match update_def.def.typ with
              | TFun (msg, TFun (model, update_result)) -> (
                  if not (equal_typ model_ty model) then
                    fail
                      ("WEB003 update model argument mismatch: expected "
                     ^ string_of_typ model_ty ^ ", got " ^ string_of_typ model);
                  if not (equal_typ init_msg_ty msg) then
                    fail
                      ("WEB007 update message mismatch: expected "
                     ^ string_of_typ init_msg_ty ^ ", got " ^ string_of_typ msg);
                  match cmd_tuple_shape update_result with
                  | Some (model', update_caps, update_msg_ty) ->
                      if not (equal_typ model_ty model') then
                        fail
                          ("WEB004 update result mismatch: expected "
                         ^ string_of_typ model_ty ^ ", got " ^ string_of_typ model');
                      if not (equal_typ msg update_msg_ty) then
                        fail
                          ("WEB007 update command message mismatch: expected "
                         ^ string_of_typ msg ^ ", got " ^ string_of_typ update_msg_ty);
                      require_cmd_capabilities init_caps update_caps "WEB012 update";
                      msg
                  | None ->
                      fail
                        ("WEB005 update must return (Tuple Model (Cmd caps Msg)), got "
                       ^ string_of_typ update_result))
              | t ->
                  fail
                    ("WEB005 update must have type Msg -> Model -> (Tuple Model (Cmd caps Msg)), got "
                   ^ string_of_typ t)
            in
            (model_ty, msg_ty, "cmd"))
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
  { checked; init_def; update_def; view_def; model_ty; msg_ty; architecture;
    backend = check_backend_contract checked }

let app_check project =
  let manifest = manifest project in
  let build = Workspace.build ~write:false manifest in
  check_contract build.checked

let type_to_json typ = json_string (string_of_typ typ)

let rec value_to_json = function
  | Runtime.VThunk thunk -> value_to_json (Runtime.force_value (Runtime.VThunk thunk))
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
  | Runtime.VAttribute attr ->
      json_obj [ json_field "tag" (json_string "Attribute"); json_field "attribute" (attr_to_json attr) ]
  | Runtime.VClosure _ -> json_obj [ json_field "tag" (json_string "Closure") ]
  | Runtime.VStream (_, item_ty, _, _) ->
      json_obj
        [
          json_field "tag" (json_string "Stream");
          json_field "itemType" (type_to_json item_ty);
        ]
  | Runtime.VAutomaton (_, output_ty, _, _) ->
      json_obj
        [
          json_field "tag" (json_string "Automaton");
          json_field "outputType" (type_to_json output_ty);
        ]
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
  | Runtime.VThunk thunk -> message_to_json (Runtime.force_value (Runtime.VThunk thunk))
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
  | Runtime.VNode (tag, attrs, children) ->
      json_obj
        [
          json_field "kind" (json_string "node");
          json_field "tag" (json_string tag);
          json_field "attributes" (json_array attr_to_json attrs);
          json_field "children" (json_array (view_to_json checked_opt) children);
        ]

and attr_to_json = function
  | Runtime.VAttr (name, value) ->
      json_obj
        [
          json_field "kind" (json_string "attr");
          json_field "name" (json_string name);
          json_field "value" (json_string value);
        ]
  | Runtime.VOn (event, msg) ->
      json_obj
        [
          json_field "kind" (json_string "on");
          json_field "event" (json_string event);
          json_field "message" (message_to_json msg);
        ]

let initial_model_and_view contract =
  match fst (Runtime.eval_entry contract.checked "init") with
  | Runtime.VProcessDone model when String.equal contract.architecture "process" -> (
      let view_fn, _ = Runtime.eval_entry contract.checked "view" in
      match Runtime.apply contract.checked view_fn model with
      | Runtime.VView view -> (model, view)
      | other -> fail ("WEB009 view did not produce View at runtime: " ^ Runtime.value_to_string other))
  | Runtime.VProcessRequest _ when String.equal contract.architecture "process" ->
      fail "WEB010 init suspended; Web Alpha requires init to be Done for deterministic bundle"
  | Runtime.VRecord fields when String.equal contract.architecture "cmd" -> (
      match record_field "_1" fields with
      | Some model -> (
          let view_fn, _ = Runtime.eval_entry contract.checked "view" in
          match Runtime.apply contract.checked view_fn model with
          | Runtime.VView view -> (model, view)
          | other ->
              fail ("WEB009 view did not produce View at runtime: " ^ Runtime.value_to_string other))
      | None -> fail "WEB013 init command tuple missing model")
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
   ^ "\narchitecture=" ^ contract.architecture ^ "\ninit=" ^ contract.init_def.def_id
   ^ "\nupdate=" ^ contract.update_def.def_id ^ "\nview=" ^ contract.view_def.def_id ^ "\n")

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
  function defMaps(program) {
    var byId = {};
    var byName = {};
    (program.defs || []).forEach(function (d) {
      byId[d.defId] = d;
      byName[d.name] = d;
    });
    return { byId: byId, byName: byName };
  }
  function variantValue(constructor, payload) {
    return { tag: "Variant", constructor: constructor, payload: payload || unitValue() };
  }
  function listValue(items) {
    return { tag: "List", items: items || [] };
  }
  function viewValue(view) {
    return { tag: "View", view: view };
  }
  function attrValue(attr) {
    return { tag: "Attribute", attr: attr };
  }
  function requestExpected(req) {
    if (!req) return "Unit";
    if (req.tag === "SaveLocal") return "Unit";
    return "String";
  }
  function requestPayload(req) {
    if (!req) return {};
    if (req.tag === "AskHuman") return { prompt: req.prompt };
    if (req.tag === "HttpGet") return { url: req.url };
    if (req.tag === "SaveLocal") return { key: req.key, value: req.value };
    if (req.tag === "LoadLocal") return { key: req.key };
    if (req.tag === "ServerRequest") return { route: req.route, payload: req.payload };
    return {};
  }
  function hostContractIndex(contract) {
    var bySignature = {};
    var byCapabilityTag = {};
    var hostCodecVersion = (contract && contract.hostCodecVersion) || "";
    ((contract && contract.capabilities) || []).forEach(function (cap) {
      (cap.requests || []).forEach(function (req) {
        var entry = {
          capability: cap.name,
          capabilityRef: cap.capabilityRef,
          requestTag: req.tag,
          requestSignatureRef: req.requestSignatureRef,
          payloadTypeCanonical: req.payloadTypeCanonical,
          responseTypeCanonical: req.responseTypeCanonical,
          hostCodecVersion: hostCodecVersion,
          requestCodecRef: req.requestCodec && req.requestCodec.codecRef,
          responseCodecRef: req.responseCodec && req.responseCodec.codecRef
        };
        bySignature[req.requestSignatureRef] = entry;
        byCapabilityTag[cap.name + ":" + req.tag] = entry;
      });
    });
    return { bySignature: bySignature, byCapabilityTag: byCapabilityTag };
  }
  function evalProgram(program) {
    var maps = defMaps(program);
    var values = {};
    var recurStack = [];
    function expectDef(ref) {
      var d = maps.byId[ref] || maps.byName[ref];
      if (!d) throw new Error("unknown Protoss definition: " + ref);
      return d;
    }
    function nth(env, index) {
      if (index < env.length) return env[index];
      throw new Error("unbound canonical variable #" + index);
    }
    function evalDef(ref) {
      var d = expectDef(ref);
      if (values[d.defId]) return values[d.defId];
      var v = evalTerm(d.term, []);
      values[d.defId] = v;
      return v;
    }
    function apply(fn, arg) {
      if (!fn) throw new Error("application of empty value");
      if (fn.tag === "BuiltinSucc") {
        if (!arg || arg.tag !== "Nat") throw new Error("succ expects Nat");
        return { tag: "Nat", value: arg.value + 1 };
      }
      if (fn.tag === "BuiltinPrim") {
        if (fn.name === "prim.Nat.eq") {
          if (!fn.args.length) return { tag: "BuiltinPrim", name: fn.name, args: [arg] };
          return { tag: "Bool", value: fn.args[0].value === arg.value };
        }
        if (fn.name === "prim.Nat.toString") {
          return { tag: "String", value: String(arg.value) };
        }
        if (fn.name === "prim.String.concat") {
          if (!fn.args.length) return { tag: "BuiltinPrim", name: fn.name, args: [arg] };
          return { tag: "String", value: String(fn.args[0].value) + String(arg.value) };
        }
        if (fn.name === "prim.String.eq") {
          if (!fn.args.length) return { tag: "BuiltinPrim", name: fn.name, args: [arg] };
          return { tag: "Bool", value: String(fn.args[0].value) === String(arg.value) };
        }
        if (fn.name === "prim.String.length") {
          return { tag: "Nat", value: Array.from(String(arg.value)).length };
        }
        if (fn.name === "prim.String.slice") {
          if (fn.args.length === 0) return { tag: "BuiltinPrim", name: fn.name, args: [arg] };
          if (fn.args.length === 1) return { tag: "BuiltinPrim", name: fn.name, args: fn.args.concat([arg]) };
          var chars = Array.from(String(fn.args[0].value));
          var start = Number(fn.args[1].value);
          var count = Number(arg.value);
          return { tag: "String", value: chars.slice(start, start + count).join("") };
        }
      }
      if (fn.tag === "Closure") return evalTerm(fn.body, [arg].concat(fn.env));
      throw new Error("application of non-function: " + JSON.stringify(fn));
    }
    function branchFor(branches, pred) {
      for (var i = 0; i < branches.length; i++) if (pred(branches[i])) return branches[i];
      throw new Error("missing case branch");
    }
    function foldVariant(term, env) {
      function fold(value) {
        if (!value || value.tag !== "Variant") throw new Error("foldVariant expects Variant");
        var b = branchFor(term.branches || [], function (br) {
          return br.tag === "VariantBranch" && br.constructor === value.constructor;
        });
        recurStack.push(fold);
        try { return evalTerm(b.body, [value.payload].concat(env)); }
        finally { recurStack.pop(); }
      }
      return fold(evalTerm(term.scrutinee, env));
    }
    function evalTerm(term, env) {
      if (!term) return unitValue();
      switch (term.tag) {
        case "Unit": return unitValue();
        case "Bool": return { tag: "Bool", value: !!term.value };
        case "Nat": return { tag: "Nat", value: term.value || 0 };
        case "String": return { tag: "String", value: term.value || "" };
        case "Var": return nth(env, term.index || 0);
        case "Builtin":
          if (term.name === "succ") return { tag: "BuiltinSucc" };
          return { tag: "BuiltinPrim", name: term.name, args: [] };
        case "Ref": return evalDef(term.defId);
        case "Lambda": return { tag: "Closure", body: term.body, env: env.slice() };
        case "App": return apply(evalTerm(term.fn, env), evalTerm(term.arg, env));
        case "Let": return evalTerm(term.body, [evalTerm(term.value, env)].concat(env));
        case "Record": {
          var fields = {};
          (term.fields || []).forEach(function (f) { fields[f.name] = evalTerm(f.value, env); });
          return { tag: "Record", fields: fields };
        }
        case "Field": return evalTerm(term.record, env).fields[term.field];
        case "Variant": return variantValue(term.constructor, evalTerm(term.payload, env));
        case "Inst": return evalDef(term.defId);
        case "Case": {
          var s = evalTerm(term.scrutinee, env);
          if (s.tag === "Bool") {
            return evalTerm(branchFor(term.branches || [], function (b) {
              return b.tag === "BoolBranch" && b.value === s.value;
            }).body, env);
          }
          if (s.tag === "Variant") {
            return evalTerm(branchFor(term.branches || [], function (b) {
              return b.tag === "VariantBranch" && b.constructor === s.constructor;
            }).body, [s.payload].concat(env));
          }
          throw new Error("case expects Bool or Variant");
        }
        case "FoldNat": {
          var count = evalTerm(term.index, env).value || 0;
          var acc = evalTerm(term.zero, env);
          var step = evalTerm(term.step, env);
          for (var i = 0; i < count; i++) acc = apply(step, acc);
          return acc;
        }
        case "FoldVariant": return foldVariant(term, env);
        case "Recur": {
          if (!recurStack.length) throw new Error("recur outside foldVariant");
          return recurStack[recurStack.length - 1](evalTerm(term.value, env));
        }
        case "Nil": return listValue([]);
        case "Cons": {
          var tail = evalTerm(term.tail, env);
          return listValue([evalTerm(term.head, env)].concat(tail.items || []));
        }
        case "FoldList": {
          var items = evalTerm(term.list, env).items || [];
          var zero = evalTerm(term.zero, env);
          var stepFn = evalTerm(term.step, env);
          for (var j = items.length - 1; j >= 0; j--) zero = apply(apply(stepFn, items[j]), zero);
          return zero;
        }
        case "CaseList": {
          var list = evalTerm(term.list, env);
          if (!list || list.tag !== "List") throw new Error("caseList expects List");
          var caseItems = list.items || [];
          if (!caseItems.length) return evalTerm(term.nil, env);
          return evalTerm(term.cons, [caseItems[0], listValue(caseItems.slice(1))].concat(env));
        }
        case "Text": return viewValue({ kind: "text", text: evalTerm(term.value, env).value || "" });
        case "Image": return viewValue({
          kind: "image",
          src: evalTerm(term.src, env).value || "",
          alt: evalTerm(term.alt, env).value || ""
        });
        case "Button": return viewValue({
          kind: "button",
          label: evalTerm(term.label, env).value || "",
          message: evalTerm(term.message, env)
        });
        case "Input": return viewValue({
          kind: "input",
          value: evalTerm(term.value, env).value || "",
          handler: evalTerm(term.handler, env)
        });
        case "Column":
        case "Row": {
          var children = (evalTerm(term.children, env).items || []).map(function (v) { return v.view; });
          return viewValue({ kind: term.tag === "Column" ? "column" : "row", children: children });
        }
        case "ListView": {
          var render = evalTerm(term.render, env);
          var listItems = evalTerm(term.items, env).items || [];
          return viewValue({
            kind: "column",
            children: listItems.map(function (item) { return apply(render, item).view; })
          });
        }
        case "WhenView":
          return evalTerm(term.condition, env).value
            ? evalTerm(term.view, env)
            : viewValue({ kind: "column", children: [] });
        case "Node": {
          var nodeAttrs = (evalTerm(term.attributes, env).items || []).map(function (a) { return a.attr; });
          var nodeChildren = (evalTerm(term.children, env).items || []).map(function (v) { return v.view; });
          return viewValue({
            kind: "node",
            tag: evalTerm(term.tagName, env).value || "",
            attributes: nodeAttrs,
            children: nodeChildren
          });
        }
        case "Attr": return attrValue({
          kind: "attr",
          name: evalTerm(term.name, env).value || "",
          value: evalTerm(term.value, env).value || ""
        });
        case "On": return attrValue({
          kind: "on",
          event: evalTerm(term.event, env).value || "",
          message: evalTerm(term.message, env)
        });
        case "Done": return { tag: "Done", value: evalTerm(term.value, env) };
        case "Request": {
          var request = term.request || {};
          return {
            tag: "Request",
            request: request,
            requestId: hashString("request:" + JSON.stringify(request)),
            expected: requestExpected(request),
            continuation: { tag: "Done" }
          };
        }
        case "Bind": {
          var p = evalTerm(term.process, env);
          if (p.tag === "Done") return evalTerm(term.body, [p.value].concat(env));
          if (p.tag === "Request") {
            return {
              tag: "Request",
              request: p.request,
              requestId: p.requestId,
              expected: p.expected,
              continuation: { tag: "Bind", previous: p.continuation, body: term.body, env: env.slice() }
            };
          }
          throw new Error("bind expects Process");
        }
        default: throw new Error("unsupported canonical term: " + term.tag);
      }
    }
    function resume(process, response) {
      var cont = process.continuation || { tag: "Done" };
      if (cont.tag === "Done") return { tag: "Done", value: response };
      if (cont.tag === "Bind") {
        var previousDone = resume({ continuation: cont.previous || { tag: "Done" } }, response);
        if (previousDone.tag === "Request") return previousDone;
        return evalTerm(cont.body, [previousDone.value].concat(cont.env || []));
      }
      throw new Error("unknown continuation");
    }
    return {
      def: evalDef,
      apply: apply,
      update: function (updateRef, msg, model) { return apply(apply(evalDef(updateRef), msg), model); },
      view: function (viewRef, model) { return apply(evalDef(viewRef), model); },
      resume: resume
    };
  }
  // Two vnodes patch in place only when their structural signature matches.
  // For "node" kind the tag participates in the signature so a div->button
  // change forces a replace rather than an in-place mutation.
  function vnodeKey(node) {
    if (!node) return "";
    if (node.kind === "node") return "node:" + (node.tag || "div");
    return String(node.kind || "");
  }
  // Listener bookkeeping: every event handler we attach is recorded under a
  // stable slot name on el.__protossListeners so a later patch can remove the
  // exact previous function before binding the next one (rebinding on change).
  function ensureListenerStore(el) {
    if (!el.__protossListeners) el.__protossListeners = {};
    return el.__protossListeners;
  }
  function setListener(el, slot, event, handler) {
    var store = ensureListenerStore(el);
    var prev = store[slot];
    if (prev) {
      el.removeEventListener(prev.event, prev.handler);
      delete store[slot];
    }
    if (handler) {
      el.addEventListener(event, handler);
      store[slot] = { event: event, handler: handler };
    }
  }
  function clearListeners(el) {
    if (!el || !el.__protossListeners) return;
    var store = el.__protossListeners;
    Object.keys(store).forEach(function (slot) {
      var rec = store[slot];
      if (rec) el.removeEventListener(rec.event, rec.handler);
    });
    el.__protossListeners = {};
  }
  function inputHandlerFor(el, node, dispatch) {
    return function () { dispatch({ inputHandler: node.handler, value: el.value }); };
  }
  function attrListenerSlot(attr) { return "on:" + (attr.event || ""); }
  // Bind/rebind the {kind:"on"} listeners declared on a "node" vnode, removing
  // any previously-bound "on:*" slots that the new attribute set no longer
  // declares. Inline on* attributes are handled separately (and blocked).
  function syncNodeListeners(el, attributes, dispatch) {
    var store = ensureListenerStore(el);
    var keep = {};
    (attributes || []).forEach(function (attr) {
      if (attr.kind !== "on") return;
      var slot = attrListenerSlot(attr);
      keep[slot] = true;
      setListener(el, slot, attr.event || "", function () { dispatch(attr.message); });
    });
    Object.keys(store).forEach(function (slot) {
      if (slot.indexOf("on:") === 0 && !keep[slot]) setListener(el, slot, null, null);
    });
  }
  function applyAttributes(el, attributes) {
    (attributes || []).forEach(function (attr) {
      if (attr.kind === "attr") {
        var name = String(attr.name || "");
        // Block inline event-handler attributes (onclick, onload, ...) to avoid script injection.
        if (/^on/i.test(name)) return;
        el.setAttribute(name, attr.value || "");
      }
    });
  }
  function renderView(node, dispatch) {
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
      input.value = node.value || "";
      setListener(input, "input", "input", inputHandlerFor(input, node, dispatch));
      return input;
    }
    if (node.kind === "button") {
      var button = document.createElement("button");
      button.textContent = node.label || "";
      setListener(button, "click", "click", function () { dispatch(node.message); });
      return button;
    }
    if (node.kind === "row" || node.kind === "column") {
      var div = document.createElement("div");
      div.className = "protoss-" + node.kind;
      (node.children || []).forEach(function (child) { div.appendChild(renderView(child, dispatch)); });
      return div;
    }
    if (node.kind === "node") {
      var el = document.createElement(node.tag || "div");
      applyAttributes(el, node.attributes);
      syncNodeListeners(el, node.attributes, dispatch);
      (node.children || []).forEach(function (child) { el.appendChild(renderView(child, dispatch)); });
      return el;
    }
    return text("");
  }
  // Diff a "node" vnode's attributes against the previous ones, mutating the
  // existing DOM element: drop attributes that disappeared, set/update the rest,
  // then rebind the {kind:"on"} listeners. Inline on* names stay blocked.
  function patchAttributes(el, oldAttributes, newAttributes, dispatch) {
    var oldNamed = {};
    (oldAttributes || []).forEach(function (attr) {
      if (attr.kind === "attr") {
        var name = String(attr.name || "");
        if (/^on/i.test(name)) return;
        oldNamed[name] = attr.value || "";
      }
    });
    var newNamed = {};
    (newAttributes || []).forEach(function (attr) {
      if (attr.kind === "attr") {
        var name = String(attr.name || "");
        if (/^on/i.test(name)) return;
        newNamed[name] = attr.value || "";
        if (oldNamed[name] !== newNamed[name]) el.setAttribute(name, newNamed[name]);
      }
    });
    Object.keys(oldNamed).forEach(function (name) {
      if (!(name in newNamed)) el.removeAttribute(name);
    });
    syncNodeListeners(el, newAttributes, dispatch);
  }
  // Opt-in keyed reconciliation. A child's key is the value of an
  // {kind:"attr", name:"key", value:K} attribute on a "node" vnode; any other
  // kind (or a node without that attribute) has no key. Keys make reordering
  // cheap and identity-preserving: an existing DOM node is MOVED (insertBefore)
  // to its new slot instead of being mutated in place at a fixed index.
  function keyOf(node) {
    if (!node || node.kind !== "node") return null;
    var attrs = node.attributes || [];
    for (var i = 0; i < attrs.length; i++) {
      var attr = attrs[i];
      if (attr && attr.kind === "attr" && String(attr.name || "") === "key") {
        return String(attr.value == null ? "" : attr.value);
      }
    }
    return null;
  }
  // A child list opts into keyed reconciliation only when it is non-empty and
  // EVERY child carries a non-null key AND all keys are distinct. Any missing or
  // duplicate key disqualifies the whole list, which then falls back to the
  // positional diff — so unkeyed lists behave exactly as before.
  function childrenAreKeyed(children) {
    if (!children || children.length === 0) return false;
    var seen = Object.create(null);
    for (var i = 0; i < children.length; i++) {
      var k = keyOf(children[i]);
      if (k === null) return false;
      var slot = "k:" + k;
      if (seen[slot]) return false;
      seen[slot] = true;
    }
    return true;
  }
  // Keyed list reconciliation. Pre-state: domEl.childNodes is parallel to
  // oldChildren (same length, same order — guaranteed by how the previous render
  // built/patched it). Strategy (simple and correct over optimal):
  //   1. Index oldKey -> { vnode, domNode } by walking both in lockstep.
  //   2. Walk newChildren left-to-right. For each new child, if its key was in
  //      the old set: patch the surviving DOM node in place, then MOVE it to the
  //      current target slot via insertBefore (so its object identity — and a
  //      focused <input>'s focus/caret — survives the reorder). Otherwise render
  //      a fresh node and insert it at the target slot.
  //   3. Whatever old keys went unconsumed are stale: clearDeep + remove them.
  // Building the target sequence by reusing/inserting and removing leftovers
  // avoids the off-by-one index bugs of in-place positional shuffles.
  function reconcileKeyed(domEl, oldChildren, newChildren, dispatch) {
    var oldByKey = Object.create(null);
    for (var i = 0; i < oldChildren.length; i++) {
      var ok = keyOf(oldChildren[i]);
      // childrenAreKeyed(oldChildren) held, so every key is present and unique.
      oldByKey["k:" + ok] = { vnode: oldChildren[i], domNode: domEl.childNodes[i] };
    }
    var consumed = Object.create(null);
    // Walk new children; after step j, domEl.childNodes[0..j] are the first j+1
    // target nodes in their final order. The node currently sitting at index j
    // (before we place this one) is our insertBefore reference.
    for (var j = 0; j < newChildren.length; j++) {
      var newChild = newChildren[j];
      var nk = keyOf(newChild);
      var slot = "k:" + nk;
      var prior = oldByKey[slot];
      var refNode = domEl.childNodes[j] || null;
      if (prior) {
        // Update the surviving node in place. patch() with a matching parent
        // reuses prior.domNode (same structural signature ⇒ in-place update);
        // it must NOT be clearDeep'd — we are moving, not discarding, it.
        var updated = patch(domEl, prior.domNode, prior.vnode, newChild, dispatch);
        var moveNode = updated || prior.domNode;
        // Move into the target slot. If it is already the ref node, this is a
        // no-op reposition; insertBefore(x, x) is a defined no-op in the DOM.
        if (moveNode !== refNode) domEl.insertBefore(moveNode, refNode);
        consumed[slot] = true;
      } else {
        var created = renderView(newChild, dispatch);
        domEl.insertBefore(created, refNode);
      }
    }
    // Remove any old DOM nodes whose key was not reused. They have been shifted
    // toward the tail by the insertBefore moves, so collect-then-remove rather
    // than relying on positions.
    var stale = [];
    for (var key in oldByKey) {
      if (!consumed[key]) stale.push(oldByKey[key].domNode);
    }
    for (var s = 0; s < stale.length; s++) {
      var deadNode = stale[s];
      if (deadNode && deadNode.parentNode === domEl) {
        clearDeep(deadNode);
        domEl.removeChild(deadNode);
      }
    }
  }
  // Child diff dispatcher. When BOTH the old and new child lists are fully keyed
  // (see childrenAreKeyed) we reconcile by key, moving existing DOM nodes. In
  // every other case we keep the original positional diff UNCHANGED: walk
  // old/new in parallel, patching the DOM child at each index. The positional
  // path is intentionally NOT keyed — a prepend re-patches every following node
  // rather than moving nodes — and stays byte-for-byte the legacy behaviour for
  // any list that does not fully opt in via per-child "key" attributes.
  function patchChildren(domEl, oldChildren, newChildren, dispatch) {
    oldChildren = oldChildren || [];
    newChildren = newChildren || [];
    if (childrenAreKeyed(oldChildren) && childrenAreKeyed(newChildren)) {
      reconcileKeyed(domEl, oldChildren, newChildren, dispatch);
      return;
    }
    var max = Math.max(oldChildren.length, newChildren.length);
    for (var i = 0; i < max; i++) {
      // domEl.childNodes[i] is the DOM node currently rendering oldChildren[i].
      patch(domEl, domEl.childNodes[i], oldChildren[i], newChildren[i], dispatch);
    }
  }
  // Core VDOM patch. Mutates the real DOM under `parent` to match newVNode,
  // reusing `domNode` (the node currently rendering oldVNode) wherever possible
  // so unchanged elements — notably a focused <input> — stay the same DOM object.
  //   oldVNode absent  -> create + insert.
  //   newVNode absent  -> remove domNode.
  //   signature differs -> replace domNode with a fresh render.
  //   signature matches -> update in place, then recurse into children.
  // Returns the DOM node now occupying the slot (or null when removed).
  function patch(parent, domNode, oldVNode, newVNode, dispatch) {
    if (!oldVNode && !newVNode) return domNode || null;
    if (!oldVNode) {
      var created = renderView(newVNode, dispatch);
      parent.appendChild(created);
      return created;
    }
    if (!newVNode) {
      if (domNode) {
        clearDeep(domNode);
        parent.removeChild(domNode);
      }
      return null;
    }
    if (!domNode || vnodeKey(oldVNode) !== vnodeKey(newVNode)) {
      var replacement = renderView(newVNode, dispatch);
      if (domNode) {
        clearDeep(domNode);
        parent.replaceChild(replacement, domNode);
      } else {
        parent.appendChild(replacement);
      }
      return replacement;
    }
    // Same structural signature: update the existing domNode in place.
    if (newVNode.kind === "text") {
      var nextText = newVNode.text || "";
      if (domNode.textContent !== nextText) domNode.textContent = nextText;
      return domNode;
    }
    if (newVNode.kind === "image") {
      if (domNode.src !== (newVNode.src || "")) domNode.src = newVNode.src || "";
      if (domNode.alt !== (newVNode.alt || "")) domNode.alt = newVNode.alt || "";
      return domNode;
    }
    if (newVNode.kind === "input") {
      // Overwrite the live value only when the *model* changed it (old vnode
      // value differs from new), not merely when it differs from the live DOM.
      // A controlled input whose model value is unchanged keeps the user's
      // in-progress text and caret instead of being stomped on every render.
      var oldValue = oldVNode.value || "";
      var newValue = newVNode.value || "";
      if (oldValue !== newValue && domNode.value !== newValue) domNode.value = newValue;
      setListener(domNode, "input", "input", inputHandlerFor(domNode, newVNode, dispatch));
      return domNode;
    }
    if (newVNode.kind === "button") {
      var nextLabel = newVNode.label || "";
      if (domNode.textContent !== nextLabel) domNode.textContent = nextLabel;
      setListener(domNode, "click", "click", function () { dispatch(newVNode.message); });
      return domNode;
    }
    if (newVNode.kind === "row" || newVNode.kind === "column") {
      patchChildren(domNode, oldVNode.children, newVNode.children, dispatch);
      return domNode;
    }
    if (newVNode.kind === "node") {
      patchAttributes(domNode, oldVNode.attributes, newVNode.attributes, dispatch);
      patchChildren(domNode, oldVNode.children, newVNode.children, dispatch);
      return domNode;
    }
    return domNode;
  }
  // Detach all listeners on a subtree before it leaves the DOM, so removed
  // nodes don't retain handler references.
  function clearDeep(domNode) {
    if (!domNode || domNode.nodeType !== 1) return;
    clearListeners(domNode);
    var kids = domNode.childNodes;
    if (!kids) return;
    for (var i = 0; i < kids.length; i++) clearDeep(kids[i]);
  }
  window.ProtossRuntime = {
    start: function (app) {
      var mount = document.getElementById("app");
      mount.className = "protoss-app";
      var machine = evalProgram(app.program);
      var hostContract = hostContractIndex(app.hostContract || {});
      // Previous virtual tree the mount's single child currently renders; null
      // before the first render so patch() builds the initial DOM from scratch.
      var prevVNode = null;
      var modelValue = app.initialModel;
      var worldRef = app.worldRef;
      var ledger = [];
      var pending = {};
      function record(kind, payload) {
        var body = JSON.stringify({ world: worldRef, kind: kind, payload: payload });
        var eventRef = hashString("event:" + body);
        worldRef = hashString("world:" + worldRef + ":" + eventRef);
        ledger.push({ eventRef: eventRef, worldRef: worldRef, kind: kind, payload: payload });
        try { localStorage.setItem("protoss-ledger", JSON.stringify(ledger)); } catch (_) {}
        return eventRef;
      }
      function typedResponse(request, response) {
        if (response && response.tag) return response;
        return request.expected === "Unit" ? unitValue() : jsToStringValue(response || "");
      }
      function resumeRequest(requestId, response) {
        var request = pending[requestId];
        if (!request) throw new Error("unknown pending request: " + requestId);
        var typed = typedResponse(request, response);
        if (request.expected !== typed.tag) throw new Error("protocol response type mismatch");
        delete pending[requestId];
        record("resume", {
          requestId: requestId,
          requestSignatureRef: request.requestSignatureRef,
          responseType: request.responseType,
          hostCodecVersion: request.hostCodecVersion,
          responseCodecRef: request.responseCodecRef,
          response: typed
        });
        handleProcess(machine.resume(request.process, typed));
      }
      function handleProcess(process) {
        if (process.tag === "Done") {
          modelValue = process.value;
          render();
          return;
        }
        if (process.tag === "Request") {
          var requestTerm = process.request || {};
          var contract =
            hostContract.bySignature[requestTerm.requestSignatureRef]
            || hostContract.byCapabilityTag[(requestTerm.capability || "") + ":" + (requestTerm.tag || "")]
            || {};
          var request = {
            requestId: process.requestId,
            capability: requestTerm.capability,
            capabilityRef: requestTerm.capabilityRef || contract.capabilityRef,
            requestTag: requestTerm.tag,
            requestSignatureRef: requestTerm.requestSignatureRef || contract.requestSignatureRef,
            hostCodecVersion: contract.hostCodecVersion || "",
            requestCodecRef: contract.requestCodecRef || "",
            responseCodecRef: contract.responseCodecRef || "",
            requestPayloadType: contract.payloadTypeCanonical || "",
            responseType: contract.responseTypeCanonical || process.expected,
            request: requestTerm,
            payload: requestPayload(requestTerm),
            expected: process.expected,
            process: process
          };
          pending[request.requestId] = request;
          record("request", request);
          window.dispatchEvent(new CustomEvent("protoss:request", { detail: request }));
          return;
        }
        throw new Error("update returned non-process");
      }
      function modelFromCommandResult(result) {
        if (result && result.tag === "Record" && result.fields && result.fields._1) {
          return result.fields._1;
        }
        throw new Error("update returned non-command tuple");
      }
      function dispatch(rawMsg) {
        var msg = rawMsg && rawMsg.inputHandler
          ? machine.apply(rawMsg.inputHandler, jsToStringValue(rawMsg.value))
          : rawMsg;
        record("message", msg);
        var result = machine.update(app.update, msg, modelValue);
        if (app.architecture === "cmd") {
          modelValue = modelFromCommandResult(result);
          render();
        } else {
          handleProcess(result);
        }
      }
      function render() {
        var view = machine.view(app.view, modelValue);
        var nextVNode = view.view || null;
        // Diff the new virtual tree against the previous one and mutate only the
        // changed parts of the real DOM, reusing existing nodes (so a focused
        // input keeps focus/caret). mount.firstChild is the DOM node currently
        // rendering prevVNode; on the first render it is null and patch() creates.
        patch(mount, mount.firstChild, prevVNode, nextVNode, dispatch);
        prevVNode = nextVNode;
      }
      // The dev server ships no pre-rendered model (prerender:false); evaluate
      // init in the browser instead. Production bundles carry initialModel.
      if (modelValue == null) { handleProcess(machine.def(app.init)); }
      else { render(); }
      window.ProtossRuntime._active = {
        pending: pending,
        resume: resumeRequest,
        model: function () { return modelValue; },
        world: function () { return worldRef; }
      };
    },
    resume: function (requestId, response) {
      if (!window.ProtossRuntime._active) throw new Error("Protoss runtime is not started");
      return window.ProtossRuntime._active.resume(requestId, response);
    },
    pending: function () {
      if (!window.ProtossRuntime._active) return {};
      return window.ProtossRuntime._active.pending;
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
  compiled_artifact : Workspace.compiled_artifact;
}

(* [prerender] embeds the evaluated initial model/view in the bundle. Production
   builds keep it (deterministic first paint). The dev server (`live`) passes
   [~prerender:false] to skip evaluating init/view — which wakes the interpreted
   self-hosted prelude and dominates rebuild time — because the browser runtime
   recomputes the initial model/view itself (see ProtossRuntime.start fallback). *)
let build ?(prerender = true) ?out project =
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
  let initial_model_json, initial_view_json =
    if prerender then
      let model, view = initial_model_and_view contract in
      (value_to_json model, view_to_json (Some contract.checked) view)
    else ("null", "null")
  in
  let canonical_graph_json = Kernel.checked_to_graph_json build.checked in
  (* Derive the host contract from the checked program (byte-identical to
     graph_host_contract) instead of re-parsing/re-validating the full graph
     JSON — the same ~1.5s round-trip the workspace build already dropped. *)
  let host_contract_json = Canonical_ir.host_contract_of_checked build.checked in
  let compiled_artifact =
    Workspace.write_compiled_artifact build.store ~universe_root:build.universe_root
      ~target:"web" ~optimization_policy:"web-default-v1"
  in
  let compiled_artifact_text =
    Workspace.compiled_artifact_content ~universe_root:compiled_artifact.compiled_universe_root
      ~target:compiled_artifact.compiled_target
      ~optimization_policy:compiled_artifact.compiled_optimization_policy
  in
  let app_json =
    json_obj
      [
        json_field "package" (json_string manifest.name);
        json_field "version" (json_string manifest.version);
        json_field "build" (json_string build.build_id);
        json_field "compiledArtifact" (json_string compiled_artifact.compiled_artifact_ref);
        json_field "modelType" (type_to_json contract.model_ty);
        json_field "msgType" (type_to_json contract.msg_ty);
        json_field "architecture" (json_string contract.architecture);
        json_field "init" (json_string contract.init_def.def_id);
        json_field "update" (json_string contract.update_def.def_id);
        json_field "view" (json_string contract.view_def.def_id);
        json_field "program" canonical_graph_json;
        json_field "hostContract" host_contract_json;
        json_field "worldRef" (json_string Ledger.initial_world);
        json_field "initialModel" initial_model_json;
        json_field "initialView" initial_view_json;
      ]
    ^ "\n"
  in
  write_file (Filename.concat out_dir "index.html") index_html;
  write_file (Filename.concat out_dir "protoss-runtime.js") runtime_js;
  write_file (Filename.concat out_dir "protoss-app.json") app_json;
  write_file (Filename.concat out_dir "protoss-graph.json") (stored_graph_json build.store);
  write_file (Filename.concat out_dir "protoss-canon-graph.json") canonical_graph_json;
  write_file (Filename.concat out_dir "protoss-host-contract.json") host_contract_json;
  write_file (Filename.concat out_dir "protoss-compiled-artifact.txt") compiled_artifact_text;
  write_file (Filename.concat out_dir "protoss-capabilities.json")
    (json_obj
       [
         json_field "capabilities" (json_array json_string build.checked.program.capabilities);
         json_field "capabilityDescriptors"
           (Kernel.capabilities_to_graph_json build.checked.program.capabilities);
       ]
    ^ "\n");
  write_file (Filename.concat out_dir "protoss-world.json") (current_world_json ());
  { build; contract; out_dir; compiled_artifact }

let inspect project =
  let manifest = manifest project in
  let build = Workspace.build manifest in
  let contract = check_contract build.checked in
  "package=" ^ manifest.name ^ "\nversion=" ^ manifest.version ^ "\nbuild=" ^ build.build_id
  ^ "\nmodel=" ^ string_of_typ contract.model_ty ^ "\nmsg=" ^ string_of_typ contract.msg_ty
  ^ "\narchitecture=" ^ contract.architecture ^ "\ninit=" ^ contract.init_def.def_id
  ^ "\nupdate=" ^ contract.update_def.def_id ^ "\nview=" ^ contract.view_def.def_id
  ^ "\nstore=" ^ build.store ^ "\n"

let content_type path =
  if has_suffix ".html" path then "text/html"
  else if has_suffix ".js" path then "application/javascript"
  else if has_suffix ".json" path then "application/json"
  else "text/plain"

(* Dev-server live reload. `web build` stays pure/deterministic; this script is
   injected only into the page that `serve`/`live` hands out. The browser opens
   an SSE stream (/livereload) and reloads when the server PUSHES an event after
   a rebuild — no client-side polling. *)
let livereload_script =
  "<script>\n\
  \  (function () {\n\
  \    var es = new EventSource('/livereload');\n\
  \    es.onmessage = function () { location.reload(); };\n\
  \  })();\n\
   </script>\n"

let inject_before_body html =
  let needle = "</body>" in
  let nl = String.length needle and sl = String.length html in
  let rec find i =
    if i + nl > sl then None else if String.equal (String.sub html i nl) needle then Some i else find (i + 1)
  in
  match find 0 with
  | Some i -> String.sub html 0 i ^ livereload_script ^ String.sub html i (sl - i)
  | None -> html ^ livereload_script

let serve ?(port = 8080) project =
  (* Every browser reload drops and reopens its SSE stream, so the next push
     writes to a closed socket. Ignore SIGPIPE so that write raises an EPIPE
     exception the notify loop catches and prunes, instead of killing the
     server (which made `live` die after a couple of saves). *)
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
  (* Dev server: skip the pre-render evaluation (the browser recomputes init),
     so each save's rebuild avoids waking the interpreted prelude. *)
  let output = ref (build ~prerender:false project) in
  (* Watch the project's .protoss sources by mtime. mtimes drive only the dev
     watch, never the deterministic store/bundle. *)
  let source_mtimes () =
    let manifest = Workspace.parse_manifest (Workspace.project_root project) in
    Workspace.project_source_files manifest
    |> List.filter_map (fun f -> try Some (f, (Unix.stat f).Unix.st_mtime) with _ -> None)
  in
  let watched = ref (try source_mtimes () with _ -> []) in
  let sources_changed () =
    match try Some (source_mtimes ()) with _ -> None with
    | Some now when now <> !watched ->
        watched := now;
        true
    | _ -> false
  in
  let rebuild () =
    try
      output := build ~prerender:false project;
      Printf.printf "live: rebuilt %s\n%!" (!output).build.Workspace.build_id
    with e -> Printf.eprintf "live: rebuild failed, keeping last good build: %s\n%!" (Printexc.to_string e)
  in
  let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.setsockopt sock Unix.SO_REUSEADDR true;
  Unix.bind sock (Unix.ADDR_INET (Unix.inet_addr_loopback, port));
  Unix.listen sock 10;
  Printf.printf "Serving %s on http://127.0.0.1:%d/  (live reload on .protoss save)\n%!"
    (!output).out_dir port;
  (* Open SSE streams. Both channels are retained so the fd isn't GC-closed. *)
  let sse_clients = ref [] in
  let notify_reload () =
    sse_clients :=
      List.filter
        (fun (_ic, oc) ->
          match (try output_string oc "data: reload\n\n"; flush oc; true with _ -> false) with
          | true -> true
          | false ->
              (try close_out_noerr oc with _ -> ());
              false)
        !sse_clients
  in
  let serve_file oc raw_path =
    let path = Filename.basename raw_path in
    let out_dir = (!output).out_dir in
    let status, body =
      match raw_path with
      | "/index.html" -> ("200 OK", inject_before_body (read_file (Filename.concat out_dir "index.html")))
      | "/app" -> ("200 OK", read_file (Filename.concat out_dir "protoss-app.json"))
      | "/graph" -> ("200 OK", read_file (Filename.concat out_dir "protoss-graph.json"))
      | "/world" -> ("200 OK", read_file (Filename.concat out_dir "protoss-world.json"))
      | "/ledger/events" -> ("200 OK", "{ \"events\": [] }\n")
      | "/process/requests" -> ("200 OK", "{ \"requests\": [] }\n")
      | "/process/resume" -> ("200 OK", "{ \"status\": \"queued\" }\n")
      | _ ->
          let file = Filename.concat out_dir path in
          if Sys.file_exists file then ("200 OK", read_file file) else ("404 Not Found", "not found\n")
    in
    let file = Filename.concat out_dir path in
    output_string oc
      ("HTTP/1.1 " ^ status ^ "\r\nContent-Type: " ^ content_type file
     ^ "\r\nContent-Length: " ^ string_of_int (String.length body)
     ^ "\r\nConnection: close\r\n\r\n" ^ body);
    flush oc
  in
  let handle_connection () =
    let client, _ = Unix.accept sock in
    let ic = Unix.in_channel_of_descr client and oc = Unix.out_channel_of_descr client in
    let line = try input_line ic with End_of_file -> "GET / HTTP/1.1" in
    let raw_path =
      match String.split_on_char ' ' line with
      | _ :: raw :: _ -> if raw = "/" then "/index.html" else raw
      | _ -> "/index.html"
    in
    if String.equal raw_path "/livereload" then (
      (* SSE stream: send headers, keep the connection open, push on rebuild. *)
      output_string oc
        "HTTP/1.1 200 OK\r\n\
         Content-Type: text/event-stream\r\n\
         Cache-Control: no-cache\r\n\
         Connection: keep-alive\r\n\r\n: connected\n\n";
      flush oc;
      sse_clients := (ic, oc) :: !sse_clients)
    else (
      serve_file oc raw_path;
      close_in_noerr ic;
      close_out_noerr oc)
  in
  (* Multiplex: wait up to 200ms for a connection; on timeout, check the sources
     and push a reload to open SSE streams if anything changed. The visible part
     (the browser) never polls — it just holds an SSE stream. *)
  let rec loop () =
    let ready = try let r, _, _ = Unix.select [ sock ] [] [] 0.2 in r with _ -> [] in
    if ready = [] then (if sources_changed () then (rebuild (); notify_reload ()))
    else handle_connection ();
    loop ()
  in
  loop ()
