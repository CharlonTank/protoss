open Ast

let hash = Hashcons.hash

let initial_world = hash "world:initial"

let rec ensure_dir path =
  if path <> "" && not (Sys.file_exists path) then (
    let parent = Filename.dirname path in
    if parent <> path then ensure_dir parent;
    Unix.mkdir path 0o755)

let write_file_atomic path content =
  ensure_dir (Filename.dirname path);
  let tmp = path ^ ".tmp." ^ string_of_int (Unix.getpid ()) in
  let oc = open_out tmp in
  try
    output_string oc content;
    close_out oc;
    Sys.rename tmp path
  with exn ->
    close_out_noerr oc;
    if Sys.file_exists tmp then Sys.remove tmp;
    raise exn

let request_payload = function
  | AskHuman prompt -> "AskHuman:" ^ prompt
  | HttpGet url -> "HttpGet:" ^ url
  | ReadClock -> "ReadClock"
  | SaveLocal (key, value) -> "SaveLocal:" ^ key ^ ":" ^ value
  | LoadLocal key -> "LoadLocal:" ^ key
  | ServerRequest (route, payload) -> "ServerRequest:" ^ route ^ ":" ^ payload

type request_signature = {
  capability : string;
  tag : string;
  payload_type : typ;
  response_type : typ;
}

let has_prefix prefix s =
  let lp = String.length prefix in
  String.length s >= lp && String.sub s 0 lp = prefix

let request_payload_signature = function
  | "ReadClock" ->
      Some
        { capability = "Clock.read"; tag = "ReadClock"; payload_type = TUnit; response_type = TString }
  | payload when has_prefix "AskHuman:" payload ->
      Some
        {
          capability = "Human.ask";
          tag = "AskHuman";
          payload_type = TRecord [ ("prompt", TString) ];
          response_type = TString;
        }
  | payload when has_prefix "HttpGet:" payload ->
      Some
        {
          capability = "Http.get";
          tag = "HttpGet";
          payload_type = TRecord [ ("url", TString) ];
          response_type = TString;
        }
  | payload when has_prefix "SaveLocal:" payload ->
      Some
        {
          capability = "Local.storage";
          tag = "SaveLocal";
          payload_type = TRecord [ ("key", TString); ("value", TString) ];
          response_type = TUnit;
        }
  | payload when has_prefix "LoadLocal:" payload ->
      Some
        {
          capability = "Local.storage";
          tag = "LoadLocal";
          payload_type = TRecord [ ("key", TString) ];
          response_type = TString;
        }
  | payload when has_prefix "ServerRequest:" payload ->
      Some
        {
          capability = "Server.request";
          tag = "ServerRequest";
          payload_type = TRecord [ ("payload", TString); ("route", TString) ];
          response_type = TString;
        }
  | _ -> None

let split_cap_scope s =
  if String.equal s "" then []
  else
    String.split_on_char ',' s
    |> List.filter (fun cap -> not (String.equal cap ""))
    |> List.sort_uniq String.compare

let validate_cap_scope event request cap_scope =
  let caps = split_cap_scope cap_scope in
  (try Kernel.validate_capabilities caps
   with Kernel.Error msg -> failwith ("maltyped event " ^ event ^ ": " ^ msg));
  let required =
    match request_payload_signature request with
    | Some signature -> signature.capability
    | None -> failwith ("maltyped event " ^ event ^ ": unknown request payload " ^ request)
  in
  if not (List.exists (String.equal required) caps) then
    failwith
      ("maltyped event " ^ event ^ ": cap-scope missing required capability " ^ required)

let request_signature_of_req req =
  {
    capability = Kernel.req_capability req;
    tag = Kernel.req_tag req;
    payload_type = Kernel.req_payload_type req;
    response_type = Kernel.req_result_type req;
  }

let type_text typ = Ast.string_of_typ typ

let kernel_request_signature signature =
  {
    Kernel.request_tag = signature.tag;
    request_payload_type = signature.payload_type;
    response_type = signature.response_type;
  }

let capability_ref signature =
  match Kernel.capability_ref signature.capability with
  | Some ref -> ref
  | None -> failwith ("unknown capability: " ^ signature.capability)

let request_signature_ref signature =
  Kernel.capability_request_signature_ref signature.capability
    (kernel_request_signature signature)

let validate_request_signature_fields event fields request =
  let signature =
    match request_payload_signature request with
    | Some signature -> signature
    | None -> failwith ("maltyped event " ^ event ^ ": unknown request payload " ^ request)
  in
  let require_equal name expected =
    let actual =
      List.find_opt (fun (k, _) -> String.equal k name) fields |> Option.map snd
    in
    match actual with
    | Some actual when String.equal actual expected -> ()
    | Some actual ->
        failwith
          ("maltyped event " ^ event ^ ": " ^ name ^ " mismatch: expected " ^ expected
         ^ ", got " ^ actual)
    | None -> failwith ("maltyped event " ^ event ^ ": missing " ^ name)
  in
  require_equal "capability" signature.capability;
  require_equal "capability-ref" (capability_ref signature);
  require_equal "request-tag" signature.tag;
  require_equal "request-signature-ref" (request_signature_ref signature);
  require_equal "request-payload-type" (type_text signature.payload_type);
  require_equal "response-type" (type_text signature.response_type);
  signature

let add_event root world payload =
  ensure_dir root;
  let events = Filename.concat root "events" in
  let worlds = Filename.concat root "worlds" in
  ensure_dir events;
  ensure_dir worlds;
  let content = "world=" ^ world ^ "\n" ^ payload ^ "\n" in
  let event_hash = hash ("event:" ^ content) in
  let next_world = hash ("world:" ^ world ^ ":" ^ event_hash) in
  let path = Filename.concat events event_hash in
  if not (Sys.file_exists path) then write_file_atomic path content;
  let world_path = Filename.concat worlds next_world in
  if not (Sys.file_exists world_path) then
    write_file_atomic world_path ("previous=" ^ world ^ "\nevent=" ^ event_hash ^ "\n");
  (event_hash, next_world)

let init root =
  let worlds = Filename.concat root "worlds" in
  ensure_dir worlds;
  let path = Filename.concat worlds initial_world in
  if not (Sys.file_exists path) then write_file_atomic path "previous=\nevent=\n";
  initial_world

let record_request root world req suspended request_id continuation_id cap_scope =
  validate_cap_scope "<new>" (request_payload req) (String.concat "," cap_scope);
  let parsed =
    try Runtime.parse_suspended suspended with Kernel.Error msg ->
      failwith ("maltyped event <new>: invalid suspended process: " ^ msg)
  in
  let expected_request = request_payload parsed.Runtime.req in
  if not (String.equal (request_payload req) expected_request) then
    failwith
      ("maltyped event <new>: suspended request mismatch: expected " ^ request_payload req
     ^ ", got " ^ expected_request);
  let normalized_cap_scope = List.sort_uniq String.compare cap_scope in
  if normalized_cap_scope <> parsed.Runtime.cap_scope then
    failwith "maltyped event <new>: suspended cap-scope mismatch";
  let expected_request_id = Runtime.request_id parsed in
  if not (String.equal request_id expected_request_id) then
    failwith
      ("maltyped event <new>: request-id mismatch: expected " ^ expected_request_id
     ^ ", got " ^ request_id);
  let expected_continuation_id = Runtime.continuation_id parsed in
  if not (String.equal continuation_id expected_continuation_id) then
    failwith
      ("maltyped event <new>: continuation-id mismatch: expected " ^ expected_continuation_id
     ^ ", got " ^ continuation_id);
  let signature = request_signature_of_req req in
  ignore (init root);
  add_event root world
    ("kind=request\nrequest-id=" ^ request_id ^ "\nrequest=" ^ request_payload req
   ^ "\ncapability=" ^ signature.capability ^ "\ncapability-ref="
   ^ capability_ref signature ^ "\nrequest-tag=" ^ signature.tag
   ^ "\nrequest-signature-ref=" ^ request_signature_ref signature
   ^ "\nrequest-payload-type=" ^ type_text signature.payload_type
   ^ "\nresponse-type=" ^ type_text signature.response_type
   ^ "\ncontinuation-id=" ^ continuation_id ^ "\ncap-scope="
   ^ String.concat "," (List.sort_uniq String.compare cap_scope)
   ^ "\nsuspended=" ^ String.escaped suspended)

let read_file path =
  let ic = open_in path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () ->
      let len = in_channel_length ic in
      really_input_string ic len)

let event_path root event = Filename.concat (Filename.concat root "events") event

let world_path root world = Filename.concat (Filename.concat root "worlds") world

let read_event root event = read_file (event_path root event)

let read_world root world = read_file (world_path root world)

let parse_lines content =
  String.split_on_char '\n' content
  |> List.filter_map (fun line ->
         match String.index_opt line '=' with
         | None -> None
         | Some i ->
             let k = String.sub line 0 i in
             let v = String.sub line (i + 1) (String.length line - i - 1) in
             Some (k, v))

let field name fields =
  List.find_opt (fun (k, _) -> String.equal k name) fields |> Option.map snd

let validate_resume_response root event resume response =
  let target_fields = parse_lines (read_event root resume) in
  let target_need name =
    match field name target_fields with
    | Some value -> value
    | None ->
        failwith
          ("maltyped event " ^ event ^ ": resume target missing " ^ name)
  in
  (match field "kind" target_fields with
  | Some "request" -> ()
  | Some kind ->
      failwith
        ("maltyped event " ^ event ^ ": resume target is not a request event: " ^ kind)
  | None -> failwith ("maltyped event " ^ event ^ ": resume target missing kind"));
  ignore (target_need "request-id");
  ignore (target_need "continuation-id");
  let request = target_need "request" in
  validate_cap_scope event request (target_need "cap-scope");
  let signature = validate_request_signature_fields event target_fields request in
  let suspended =
    target_need "suspended" |> Scanf.unescaped
  in
  let suspended =
    try Runtime.parse_suspended suspended with Kernel.Error msg ->
      failwith ("maltyped event " ^ event ^ ": invalid suspended process: " ^ msg)
  in
  let suspended_request = request_payload suspended.Runtime.req in
  if not (String.equal request suspended_request) then
    failwith
      ("maltyped event " ^ event ^ ": suspended request mismatch: expected "
     ^ request ^ ", got " ^ suspended_request);
  if split_cap_scope (target_need "cap-scope") <> suspended.Runtime.cap_scope then
    failwith ("maltyped event " ^ event ^ ": suspended cap-scope mismatch");
  if not (String.equal (Runtime.request_id suspended) (target_need "request-id")) then
    failwith ("maltyped event " ^ event ^ ": suspended request-id mismatch");
  if not (String.equal (Runtime.continuation_id suspended) (target_need "continuation-id")) then
    failwith ("maltyped event " ^ event ^ ": suspended continuation-id mismatch");
  (try ignore (Runtime.response_value suspended.Runtime.req response)
   with Kernel.Error msg -> failwith ("maltyped event " ^ event ^ ": invalid response: " ^ msg));
  (type_text signature.response_type, request_signature_ref signature)

let record_resume root world event response result =
  let response_type, signature_ref = validate_resume_response root "<new>" event response in
  add_event root world
    ("kind=resume\nresume=" ^ event ^ "\nrequest-signature-ref=" ^ signature_ref
   ^ "\nresponse-type=" ^ response_type ^ "\nresponse="
   ^ String.escaped response ^ "\nresult=" ^ String.escaped result)

let validate_event root event content =
  let fields = parse_lines content in
  let need name =
    match field name fields with
    | Some _ -> ()
    | None -> failwith ("maltyped event " ^ event ^ ": missing " ^ name)
  in
  need "world";
  need "kind";
  (match field "kind" fields with
  | Some "request" ->
      List.iter need
        [
          "request-id";
          "request";
          "capability";
          "capability-ref";
          "request-tag";
          "request-signature-ref";
          "request-payload-type";
          "response-type";
          "continuation-id";
          "cap-scope";
          "suspended";
        ];
      let request = Option.value (field "request" fields) ~default:"" in
      validate_cap_scope event request (Option.value (field "cap-scope" fields) ~default:"");
      ignore (validate_request_signature_fields event fields request);
      let suspended =
        Option.value (field "suspended" fields) ~default:"" |> Scanf.unescaped
      in
      let suspended =
        try Runtime.parse_suspended suspended with Kernel.Error msg ->
          failwith ("maltyped event " ^ event ^ ": invalid suspended process: " ^ msg)
      in
      let suspended_request = request_payload suspended.Runtime.req in
      if not (String.equal request suspended_request) then
        failwith
          ("maltyped event " ^ event ^ ": suspended request mismatch: expected "
         ^ request ^ ", got " ^ suspended_request);
      if split_cap_scope (Option.value (field "cap-scope" fields) ~default:"")
         <> suspended.Runtime.cap_scope
      then failwith ("maltyped event " ^ event ^ ": suspended cap-scope mismatch");
      (match field "request-id" fields with
      | Some request_id when String.equal request_id (Runtime.request_id suspended) -> ()
      | Some request_id ->
          failwith
            ("maltyped event " ^ event ^ ": request-id mismatch: expected "
           ^ Runtime.request_id suspended ^ ", got " ^ request_id)
      | None -> assert false);
      (match field "continuation-id" fields with
      | Some continuation_id when String.equal continuation_id (Runtime.continuation_id suspended) -> ()
      | Some continuation_id ->
          failwith
            ("maltyped event " ^ event ^ ": continuation-id mismatch: expected "
           ^ Runtime.continuation_id suspended ^ ", got " ^ continuation_id)
      | None -> assert false)
  | Some "resume" ->
      List.iter need [ "resume"; "request-signature-ref"; "response-type"; "response"; "result" ];
      let resume = Option.value (field "resume" fields) ~default:"" in
      let response = Option.value (field "response" fields) ~default:"" |> Scanf.unescaped in
      let response_type, signature_ref = validate_resume_response root event resume response in
      (match field "response-type" fields with
      | Some declared when String.equal declared response_type -> ()
      | Some declared ->
          failwith
            ("maltyped event " ^ event ^ ": response-type mismatch: expected "
           ^ response_type ^ ", got " ^ declared)
      | None -> assert false);
      (match field "request-signature-ref" fields with
      | Some declared when String.equal declared signature_ref -> ()
      | Some declared ->
          failwith
            ("maltyped event " ^ event ^ ": request-signature-ref mismatch: expected "
           ^ signature_ref ^ ", got " ^ declared)
      | None -> assert false)
  | Some k -> failwith ("maltyped event " ^ event ^ ": unknown kind " ^ k)
  | None -> assert false);
  fields

let event_fields root event =
  let content = read_event root event in
  validate_event root event content

let inspect_event root event =
  let content = read_event root event in
  ignore (validate_event root event content);
  content

let inspect_world root world =
  let content = read_world root world in
  let fields = parse_lines content in
  if field "previous" fields = None || field "event" fields = None then
    failwith ("maltyped world " ^ world);
  content

let next_world world event = hash ("world:" ^ world ^ ":" ^ event)

let event_suspended root event =
  match field "suspended" (event_fields root event) with
  | Some s -> Scanf.unescaped s
  | None -> failwith ("event has no suspended process: " ^ event)

let inspect root ref_ =
  if Sys.file_exists (world_path root ref_) then inspect_world root ref_
  else inspect_event root ref_

let rec replay_events root world =
  if String.equal world initial_world then []
  else
    let content = inspect_world root world in
    let fields = parse_lines content in
    match (field "previous" fields, field "event" fields) with
    | Some previous, Some event when event <> "" ->
        replay_events root previous @ [ event ]
    | _ -> []

let replay root world =
  let events = replay_events root world in
  String.concat ""
    (List.map
       (fun event -> "Event " ^ event ^ "\n" ^ inspect_event root event)
       events)

let diff root world_a world_b =
  let a = replay_events root world_a and b = replay_events root world_b in
  let only_a = List.filter (fun e -> not (List.exists (String.equal e) b)) a in
  let only_b = List.filter (fun e -> not (List.exists (String.equal e) a)) b in
  "only_a=" ^ String.concat "," only_a ^ "\nonly_b=" ^ String.concat "," only_b ^ "\n"

let export root world =
  let events = replay_events root world in
  "world=" ^ world ^ "\nevents=" ^ String.concat "," events ^ "\n"
  ^ String.concat ""
      (List.map
         (fun event ->
           "----- event " ^ event ^ " -----\n" ^ inspect_event root event)
         events)

let import root payload =
  ignore (init root);
  let imported = hash ("import:" ^ payload) in
  let imports = Filename.concat root "imports" in
  ensure_dir imports;
  write_file_atomic (Filename.concat imports imported) payload;
  imported

let branches root =
  let worlds = Filename.concat root "worlds" in
  if not (Sys.file_exists worlds) then ""
  else
    Sys.readdir worlds |> Array.to_list |> List.sort String.compare
    |> List.map (fun world ->
           let fields = parse_lines (read_world root world) in
           world ^ " previous=" ^ Option.value (field "previous" fields) ~default:""
           ^ " event=" ^ Option.value (field "event" fields) ~default:"")
    |> String.concat "\n"
    |> fun s -> if s = "" then "" else s ^ "\n"
