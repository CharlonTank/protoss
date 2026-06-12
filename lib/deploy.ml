(* `protoss deploy` — push a full-stack Protoss app to a Hetzner server and
   publish it as <name>.<domain> through Cloudflare (proxied A record, so TLS
   terminates at Cloudflare and the origin serves plain HTTP on :80).

   Model: one server per app, named protoss-<name>, fully idempotent — if the
   server already exists it is reused and the app redeployed onto it. The
   server is provisioned over SSH (hcloud's registered key): opam + an
   ocaml-system switch, the protoss sources rsynced and built remotely, the
   app rsynced (its .protoss state — ledger included — is EXCLUDED from sync
   and therefore preserved across deploys), and a systemd unit runs
   `protoss live <app> --port 80 --public`.

   External tools used deliberately (this is deployment glue, not kernel
   logic): hcloud (must be configured), ssh/rsync, and curl for the
   Cloudflare DNS upsert (needs CLOUDFLARE_API_TOKEN; without it the exact
   record to create is printed instead). *)

exception Error of string

let fail msg = raise (Error msg)

let quote = Filename.quote

(* Run a command, capture stdout, fail loudly with the command on error. *)
let run cmd =
  let ic = Unix.open_process_in (cmd ^ " 2>&1") in
  let buf = Buffer.create 256 in
  (try
     while true do
       Buffer.add_channel buf ic 1
     done
   with End_of_file -> ());
  let out = Buffer.contents buf in
  match Unix.close_process_in ic with
  | Unix.WEXITED 0 -> out
  | _ -> fail ("DEPLOY002 command failed: " ^ cmd ^ "\n" ^ out)

let run_status cmd =
  match Unix.system (cmd ^ " >/dev/null 2>&1") with Unix.WEXITED n -> n | _ -> 1

let sanitize_name name =
  String.map
    (fun c ->
      match c with 'a' .. 'z' | '0' .. '9' | '-' -> c | 'A' .. 'Z' -> Char.lowercase_ascii c | _ -> '-')
    name

let app_name project name_flag =
  match name_flag with
  | Some n -> sanitize_name n
  | None -> sanitize_name (Filename.basename (Workspace.project_root project))

let server_name name = "protoss-" ^ name

(* --- Hetzner ------------------------------------------------------------ *)

let hcloud_available () = run_status "command -v hcloud" = 0

let server_exists server = run_status ("hcloud server describe " ^ quote server) = 0

let first_ssh_key () =
  let out = run "hcloud ssh-key list -o noheader -o columns=name" in
  match String.split_on_char '\n' (String.trim out) with
  | key :: _ when String.trim key <> "" -> String.trim key
  | _ -> fail "DEPLOY003 no SSH key registered in the hcloud project (hcloud ssh-key create ...)"

let create_server server server_type location =
  let key = first_ssh_key () in
  ignore
    (run
       ("hcloud server create --name " ^ quote server ^ " --type " ^ quote server_type
      ^ " --image ubuntu-24.04 --location " ^ quote location ^ " --ssh-key " ^ quote key))

let server_ip server = String.trim (run ("hcloud server ip " ^ quote server))

(* --- Remote provisioning -------------------------------------------------- *)

let ssh_opts =
  "-o StrictHostKeyChecking=accept-new -o ConnectTimeout=10 -o BatchMode=yes"

let ssh ip script =
  run ("ssh " ^ ssh_opts ^ " root@" ^ ip ^ " " ^ quote ("set -e; " ^ script))

let wait_for_ssh ip =
  let rec retry n =
    if n = 0 then fail ("DEPLOY004 server unreachable over SSH: " ^ ip)
    else if run_status ("ssh " ^ ssh_opts ^ " root@" ^ ip ^ " true") = 0 then ()
    else (
      Unix.sleep 5;
      retry (n - 1))
  in
  retry 36 (* up to 3 minutes for first boot *)

let provision_script =
  String.concat "; "
    [
      "export DEBIAN_FRONTEND=noninteractive";
      "command -v rsync >/dev/null || (apt-get update -qq && apt-get install -y -qq rsync)";
      "command -v opam >/dev/null || (apt-get update -qq && apt-get install -y -qq opam ocaml \
       build-essential pkg-config)";
      "[ -d /root/.opam ] || opam init --bare -ya --disable-sandboxing >/dev/null";
      "opam switch list 2>/dev/null | grep -q ocaml-system || opam switch create default \
       ocaml-system >/dev/null || true";
      "eval $(opam env) && (command -v dune >/dev/null || opam install -y dune >/dev/null)";
      "mkdir -p /opt/protoss/src /opt/protoss/apps /opt/protoss/bin /opt/protoss/share";
    ]

let rsync ~excludes src dst =
  let ex = String.concat " " (List.map (fun e -> "--exclude " ^ quote e) excludes) in
  ignore (run ("rsync -az --delete " ^ ex ^ " -e " ^ quote ("ssh " ^ ssh_opts) ^ " " ^ src ^ " " ^ dst))

(* The protoss sources needed to build the CLI remotely. *)
let sync_sources ip =
  let root = Filename.dirname (Filename.dirname Sys.executable_name) in
  (* When running from a dune checkout, executable lives under _build; locate
     the checkout root by probing for dune-project upward from cwd instead. *)
  let checkout =
    let rec up dir =
      if Sys.file_exists (Filename.concat dir "dune-project") then Some dir
      else
        let parent = Filename.dirname dir in
        if String.equal parent dir then None else up parent
    in
    match up (Sys.getcwd ()) with Some dir -> dir | None -> root
  in
  List.iter
    (fun part ->
      let src = Filename.concat checkout part in
      if Sys.file_exists src then
        rsync ~excludes:[ "_build"; ".git"; ".claude"; "*.install" ]
          (quote (if Sys.is_directory src then src ^ "/" else src))
          ("root@" ^ ip ^ ":/opt/protoss/src/" ^ part ^ (if Sys.is_directory src then "/" else "")))
    [ "dune-project"; "lib"; "bin"; "stdlib" ]

let build_remote ip =
  ignore
    (ssh ip
       "cd /opt/protoss/src && eval $(opam env) && dune build bin/main.exe 2>&1 | tail -5; \
        install -m 755 /opt/protoss/src/_build/default/bin/main.exe /opt/protoss/bin/protoss; \
        install -m 644 /opt/protoss/src/stdlib/prelude.protoss /opt/protoss/share/prelude.protoss")

let sync_app ip name project =
  let root = Workspace.project_root project in
  (* Never overwrite the server's .protoss state: the production ledger (the
     backend's event log) lives there and must survive redeploys. *)
  rsync ~excludes:[ ".protoss" ]
    (quote (root ^ "/"))
    ("root@" ^ ip ^ ":/opt/protoss/apps/" ^ name ^ "/");
  (* The manifest's stdlib path is local-machine-absolute; point it at the
     server's prelude copy. *)
  ignore
    (ssh ip
       ("sed -i 's|^stdlib = .*|stdlib = \"/opt/protoss/share/prelude.protoss\"|' \
         /opt/protoss/apps/" ^ name ^ "/protoss.toml"))

let install_service ip name =
  let unit =
    String.concat "\\n"
      [
        "[Unit]";
        "Description=Protoss app " ^ name;
        "After=network.target";
        "[Service]";
        "ExecStart=/opt/protoss/bin/protoss live /opt/protoss/apps/" ^ name ^ " --port 80 --public";
        "WorkingDirectory=/opt/protoss/apps/" ^ name;
        "Restart=always";
        "RestartSec=2";
        "[Install]";
        "WantedBy=multi-user.target";
      ]
  in
  ignore
    (ssh ip
       ("printf '" ^ unit ^ "\\n' > /etc/systemd/system/protoss-" ^ name
      ^ ".service; systemctl daemon-reload; systemctl enable --now protoss-" ^ name
      ^ ".service; systemctl restart protoss-" ^ name ^ ".service"))

(* --- Cloudflare DNS ------------------------------------------------------- *)

let cloudflare_token () =
  match Sys.getenv_opt "CLOUDFLARE_API_TOKEN" with
  | Some t when String.trim t <> "" -> Some (String.trim t)
  | _ -> (
      match Sys.getenv_opt "CF_API_TOKEN" with
      | Some t when String.trim t <> "" -> Some (String.trim t)
      | _ -> None)

let cf_api token path body_opt =
  let body = match body_opt with Some b -> " --data " ^ quote b | None -> "" in
  run
    ("curl -sS -X " ^ (if body_opt = None then "GET" else "POST")
   ^ " https://api.cloudflare.com/client/v4" ^ path ^ " -H " ^ quote ("Authorization: Bearer " ^ token)
   ^ " -H 'Content-Type: application/json'" ^ body)

let json_result_field response path_fields =
  let json = Json.parse response in
  let rec walk json = function
    | [] -> Some json
    | f :: rest -> ( match Json.field f json with Some j -> walk j rest | None -> None)
  in
  walk json path_fields

let dns_upsert ~domain ~name ~ip =
  match cloudflare_token () with
  | None ->
      Printf.printf
        "DNS: no CLOUDFLARE_API_TOKEN in the environment.\n\
         Create this record on %s yourself (proxied for TLS):\n\
        \  A  %s.%s  ->  %s  (proxied)\n\
         or re-run with CLOUDFLARE_API_TOKEN set to do it automatically.\n"
        domain name domain ip
  | Some token -> (
      let zones = cf_api token ("/zones?name=" ^ domain) None in
      let zone_id =
        match json_result_field zones [ "result" ] with
        | Some (Json.Array (first :: _)) -> (
            match Json.field "id" first with
            | Some (Json.String id) -> id
            | _ -> fail ("DEPLOY005 cannot read zone id for " ^ domain))
        | _ -> fail ("DEPLOY005 zone not found on Cloudflare: " ^ domain ^ "\n" ^ zones)
      in
      let fqdn = name ^ "." ^ domain in
      let existing =
        cf_api token ("/zones/" ^ zone_id ^ "/dns_records?type=A&name=" ^ fqdn) None
      in
      let record_body =
        "{\"type\":\"A\",\"name\":\"" ^ fqdn ^ "\",\"content\":\"" ^ ip
        ^ "\",\"ttl\":1,\"proxied\":true}"
      in
      match json_result_field existing [ "result" ] with
      | Some (Json.Array (first :: _)) -> (
          match Json.field "id" first with
          | Some (Json.String record_id) ->
              ignore
                (run
                   ("curl -sS -X PUT https://api.cloudflare.com/client/v4/zones/" ^ zone_id
                  ^ "/dns_records/" ^ record_id ^ " -H "
                  ^ quote ("Authorization: Bearer " ^ token)
                  ^ " -H 'Content-Type: application/json' --data " ^ quote record_body));
              Printf.printf "DNS: updated A %s -> %s (proxied)\n" fqdn ip
          | _ -> fail "DEPLOY005 cannot read existing record id")
      | _ ->
          ignore (cf_api token ("/zones/" ^ zone_id ^ "/dns_records") (Some record_body));
          Printf.printf "DNS: created A %s -> %s (proxied)\n" fqdn ip)

(* --- The deploy ----------------------------------------------------------- *)

let deploy ?name:name_flag ?(domain = "charlon.dev") ?(server_type = "cax11")
    ?(location = "nbg1") project =
  if not (hcloud_available ()) then
    fail "DEPLOY001 hcloud CLI not found: install and configure it (hcloud context create ...)";
  (* Validate the app before touching any infrastructure. *)
  let contract = Web.app_check project in
  let name = app_name project name_flag in
  let server = server_name name in
  Printf.printf "App OK (%s architecture%s)\n" contract.Web.architecture
    (match contract.Web.backend with Some _ -> ", with backend" | None -> "");
  if server_exists server then Printf.printf "Server %s exists, redeploying\n%!" server
  else (
    Printf.printf "Creating server %s (%s, %s, ubuntu-24.04)...\n%!" server server_type location;
    create_server server server_type location);
  let ip = server_ip server in
  Printf.printf "Server IP %s\n%!" ip;
  wait_for_ssh ip;
  Printf.printf "Provisioning (opam/dune, first run takes a few minutes)...\n%!";
  ignore (ssh ip provision_script);
  Printf.printf "Syncing protoss sources + building remotely...\n%!";
  sync_sources ip;
  build_remote ip;
  Printf.printf "Syncing app (server-side .protoss state preserved)...\n%!";
  sync_app ip name project;
  install_service ip name;
  Printf.printf "Service protoss-%s running on :80\n%!" name;
  dns_upsert ~domain ~name ~ip;
  Printf.printf "Deployed: https://%s.%s/ (origin http://%s/)\n" name domain ip;
  Printf.printf "Server: %s (%s) — destroy with: hcloud server delete %s\n" server server_type server
