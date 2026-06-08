open Ast

let hash s = "p1:" ^ Hashcons.digest s

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

let has_prefix prefix s =
  let lp = String.length prefix in
  String.length s >= lp && String.sub s 0 lp = prefix

let request_payload_capability = function
  | "ReadClock" -> Some "Clock.read"
  | payload when has_prefix "AskHuman:" payload -> Some "Human.ask"
  | payload when has_prefix "HttpGet:" payload -> Some "Http.get"
  | payload when has_prefix "SaveLocal:" payload -> Some "Local.storage"
  | payload when has_prefix "LoadLocal:" payload -> Some "Local.storage"
  | payload when has_prefix "ServerRequest:" payload -> Some "Server.request"
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
    match request_payload_capability request with
    | Some cap -> cap
    | None -> failwith ("maltyped event " ^ event ^ ": unknown request payload " ^ request)
  in
  if not (List.exists (String.equal required) caps) then
    failwith
      ("maltyped event " ^ event ^ ": cap-scope missing required capability " ^ required)

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
  ignore (init root);
  add_event root world
    ("kind=request\nrequest-id=" ^ request_id ^ "\nrequest=" ^ request_payload req
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
  (try ignore (Runtime.response_value suspended.Runtime.req response)
   with Kernel.Error msg -> failwith ("maltyped event " ^ event ^ ": invalid response: " ^ msg));
  Ast.string_of_typ (Kernel.req_result_type suspended.Runtime.req)

let record_resume root world event response result =
  let response_type = validate_resume_response root "<new>" event response in
  add_event root world
    ("kind=resume\nresume=" ^ event ^ "\nresponse-type=" ^ response_type ^ "\nresponse="
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
      List.iter need [ "request-id"; "request"; "continuation-id"; "cap-scope"; "suspended" ];
      validate_cap_scope event
        (Option.value (field "request" fields) ~default:"")
        (Option.value (field "cap-scope" fields) ~default:"")
  | Some "resume" ->
      List.iter need [ "resume"; "response"; "result" ];
      let resume = Option.value (field "resume" fields) ~default:"" in
      let response = Option.value (field "response" fields) ~default:"" |> Scanf.unescaped in
      let response_type = validate_resume_response root event resume response in
      (match field "response-type" fields with
      | None -> ()
      | Some declared when String.equal declared response_type -> ()
      | Some declared ->
          failwith
            ("maltyped event " ^ event ^ ": response-type mismatch: expected "
           ^ response_type ^ ", got " ^ declared))
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
