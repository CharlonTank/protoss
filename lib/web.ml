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

type app_contract = {
  checked : Kernel.checked;
  init_def : Kernel.checked_def;
  update_def : Kernel.checked_def;
  view_def : Kernel.checked_def;
  model_ty : typ;
  msg_ty : typ;
  architecture : string;
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
  { checked; init_def; update_def; view_def; model_ty; msg_ty; architecture }

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
      input.addEventListener("input", function () {
        dispatch({ inputHandler: node.handler, value: input.value });
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
      (node.children || []).forEach(function (child) { div.appendChild(renderView(child, dispatch)); });
      return div;
    }
    if (node.kind === "node") {
      var el = document.createElement(node.tag || "div");
      (node.attributes || []).forEach(function (attr) {
        if (attr.kind === "attr") {
          var name = String(attr.name || "");
          // Block inline event-handler attributes (onclick, onload, ...) to avoid script injection.
          if (/^on/i.test(name)) return;
          el.setAttribute(name, attr.value || "");
        } else if (attr.kind === "on") {
          el.addEventListener(attr.event || "", function () { dispatch(attr.message); });
        }
      });
      (node.children || []).forEach(function (child) { el.appendChild(renderView(child, dispatch)); });
      return el;
    }
    return text("");
  }
  window.ProtossRuntime = {
    start: function (app) {
      var mount = document.getElementById("app");
      mount.className = "protoss-app";
      var machine = evalProgram(app.program);
      var hostContract = hostContractIndex(app.hostContract || {});
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
        mount.innerHTML = "";
        var view = machine.view(app.view, modelValue);
        mount.appendChild(renderView(view.view, dispatch));
      }
      render();
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
  let canonical_graph_json = Kernel.checked_to_graph_json build.checked in
  let host_contract_json = Canonical_ir.graph_host_contract canonical_graph_json in
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
        json_field "initialModel" (value_to_json model);
        json_field "initialView" (view_to_json (Some contract.checked) view);
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
   injected only into the page that `serve`/`live` hands out. The client polls
   /livereload for the current build id and reloads when it changes. *)
let livereload_script =
  "<script>\n\
  \  (function () {\n\
  \    var last = null;\n\
  \    function poll() {\n\
  \      fetch('/livereload').then(function (r) { return r.text(); }).then(function (v) {\n\
  \        if (last === null) { last = v; }\n\
  \        else if (v !== last) { location.reload(); }\n\
  \      }).catch(function () {}).then(function () { setTimeout(poll, 1000); });\n\
  \    }\n\
  \    poll();\n\
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
  let output = ref (build project) in
  (* Watch the project's .protoss sources by mtime; rebuild lazily when a poll
     arrives and something changed (mtimes drive only the dev watch, never the
     deterministic store/bundle). *)
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
      output := build project;
      Printf.printf "live: rebuilt %s\n%!" (!output).build.Workspace.build_id
    with e -> Printf.eprintf "live: rebuild failed, keeping last good build: %s\n%!" (Printexc.to_string e)
  in
  let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.setsockopt sock Unix.SO_REUSEADDR true;
  Unix.bind sock (Unix.ADDR_INET (Unix.inet_addr_loopback, port));
  Unix.listen sock 10;
  Printf.printf "Serving %s on http://127.0.0.1:%d/  (live reload on .protoss save)\n%!"
    (!output).out_dir port;
  let rec loop () =
    let client, _ = Unix.accept sock in
    let ic = Unix.in_channel_of_descr client and oc = Unix.out_channel_of_descr client in
    let line = try input_line ic with End_of_file -> "GET / HTTP/1.1" in
    let raw_path =
      match String.split_on_char ' ' line with
      | _ :: raw :: _ -> if raw = "/" then "/index.html" else raw
      | _ -> "/index.html"
    in
    let path = Filename.basename raw_path in
    let out_dir = (!output).out_dir in
    let status, body =
      match raw_path with
      | "/livereload" ->
          if sources_changed () then rebuild ();
          ("200 OK", (!output).build.Workspace.build_id ^ "\n")
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
    flush oc;
    close_in_noerr ic;
    close_out_noerr oc;
    loop ()
  in
  loop ()
