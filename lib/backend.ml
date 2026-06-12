(* Ledger-backed, Lamdera-shaped BackendModel — brick 2 of
   docs/backend-architecture.md.

   A ToBackend message is appended to the append-only ledger as a
   content-addressed `to-backend` event; the BackendModel is NOT stored
   mutable state but the deterministic fold of [updateBackend] over the
   replayed event log, starting from [initBackend]:

     model(world) = foldl step initBackend (toBackend events of world)
     step model msg = fst (updateBackend msg model)

   Everything is kernel-verified: each message text is typed by splicing it
   into a one-def program against the structural ToBackend type (a maltyped
   message fails with the normal located type error, and is rejected BEFORE
   being appended), and each event records the hash of the typed message's
   canonical value, which replay re-checks (integrity independent of the
   source spelling). Determinism of the kernel + runtime makes the fold
   reproducible from any copy of the ledger; snapshots/caching are later
   perf bricks and can never change the result. *)

exception Error of string

let fail msg = raise (Error msg)

let backend_branch = "backend"

let contract checked =
  match Web.check_backend_contract checked with
  | Some b -> b
  | None ->
      fail
        "BACKEND001 app has no backend half: define updateBackend (and initBackend) -- see \
         docs/backend-architecture.md"

(* Type a ToBackend message through the kernel: structural types make the
   one-def program self-contained. Returns the runtime value. *)
let message_value (b : Web.backend_contract) text =
  let source =
    "(def __toBackend " ^ Ast.string_of_typ b.Web.to_backend_ty ^ " " ^ text ^ ")\n"
  in
  let checked =
    try Parser.parse_string source |> Kernel.check_program with
    | Kernel.Error msg -> fail ("BACKEND002 maltyped ToBackend message: " ^ msg)
    | Parser.Error msg -> fail ("BACKEND002 unparsable ToBackend message: " ^ msg)
  in
  fst (Runtime.normalize_def checked "__toBackend")

let message_ref value = Hashcons.hash (Runtime.value_to_canonical value)

(* Render an already-typed ToBackend value back to parseable Protoss source so
   the typed [send_value] path stores the SAME canonical [to-backend] event text
   the text path does (re-parseable by [message_value] on replay). Type-directed
   via each value's carried [Ast.typ]; only the ToBackend data fragment is
   supported (Unit/Bool/Nat/String/List/Record/Variant). *)
let rec value_to_source (v : Runtime.value) : string =
  match Runtime.force_value v with
  | Runtime.VUnit -> "unit"
  | Runtime.VBool true -> "true"
  | Runtime.VBool false -> "false"
  | Runtime.VNat n -> string_of_int n
  | Runtime.VString s -> Ast.quote s
  | Runtime.VList (item_ty, []) -> "(Nil " ^ Ast.string_of_typ item_ty ^ ")"
  | Runtime.VList (item_ty, xs) ->
      List.fold_right
        (fun x acc -> "(Cons " ^ Ast.string_of_typ item_ty ^ " " ^ value_to_source x ^ " " ^ acc ^ ")")
        xs
        ("(Nil " ^ Ast.string_of_typ item_ty ^ ")")
  | Runtime.VRecord fields ->
      "(record "
      ^ String.concat " "
          (List.map (fun (n, fv) -> "(" ^ n ^ " " ^ value_to_source fv ^ ")") (Ast.sort_fields fields))
      ^ ")"
  | Runtime.VVariant (_, con, payload) ->
      (* Short constructor form `(Con payload)`: it re-checks against the expected
         ToBackend variant in [message_value] EXACTLY as the explicit
         `(variant TYPE Con payload)` form, and matches the conventional source
         spelling so the typed and text transport paths write byte-identical
         to-backend events for the same message. *)
      "(" ^ con ^ " " ^ value_to_source payload ^ ")"
  | other -> fail ("BACKEND006 cannot render ToBackend value as source: " ^ Runtime.value_to_string other)

let initial_model checked (b : Web.backend_contract) =
  fst (Runtime.normalize_def checked b.Web.init_backend_def.Kernel.def.Ast.name)

(* One deterministic step: updateBackend msg model = (model', Cmd ToFrontend). *)
let step checked (b : Web.backend_contract) model msg =
  let update_v =
    fst (Runtime.normalize_def checked b.Web.update_backend_def.Kernel.def.Ast.name)
  in
  let result = Runtime.apply checked (Runtime.apply checked update_v msg) model in
  match result with
  | Runtime.VRecord fields -> (
      let get name =
        List.find_opt (fun (f, _) -> String.equal f name) fields |> Option.map snd
      in
      match (get "_1", get "_2") with
      | Some model', Some cmd -> (model', cmd)
      | _ ->
          fail
            ("BACKEND003 updateBackend returned a non-tuple value: "
           ^ Runtime.value_to_string result))
  | v -> fail ("BACKEND003 updateBackend returned a non-tuple value: " ^ Runtime.value_to_string v)

(* Extract the ToFrontend value to broadcast from a [Cmd cmd_caps ToFrontend]
   value returned by [updateBackend], if any. [Cmd.none] is [VUnit] => no push;
   [broadcast tf] is [VBroadcast (toFrontendTy, tf)] => push [tf]. This is the
   ONLY place the transport reads the cmd slot; the broadcast is an ephemeral
   OUTPUT effect — it is deliberately NOT recorded in the ledger (see
   docs/backend-architecture.md / [send] below), so the fold stays
   reconstructible from to-backend events alone and the broadcasts re-derive
   from the fold. *)
let broadcast_of_cmd (cmd : Runtime.value) : Runtime.value option =
  match Runtime.force_value cmd with
  | Runtime.VBroadcast (_, payload) -> Some payload
  | Runtime.VUnit -> None
  | other ->
      fail
        ("BACKEND007 updateBackend command slot must be Cmd.none (unit) or (broadcast tf), got "
       ^ Runtime.value_to_string other)

(* The to-backend events of a world, oldest first, with replay-time integrity:
   each stored message must re-type to the canonical value hash recorded at
   append time. *)
let world_messages root (b : Web.backend_contract) world =
  Ledger.replay_events root world
  |> List.filter_map (fun event ->
         let fields = Ledger.event_fields root event in
         match Ledger.field "kind" fields with
         | Some "to-backend" ->
             let text =
               match Ledger.field "to-backend" fields with
               | Some escaped -> Scanf.unescaped escaped
               | None -> fail ("BACKEND004 to-backend event missing payload: " ^ event)
             in
             let value = message_value b text in
             let expected = message_ref value in
             (match Ledger.field "message-ref" fields with
             | Some declared when String.equal declared expected -> ()
             | Some declared ->
                 fail
                   ("BACKEND005 message-ref mismatch for event " ^ event ^ ": expected "
                  ^ expected ^ ", got " ^ declared)
             | None -> fail ("BACKEND004 to-backend event missing message-ref: " ^ event));
             Some value
         | _ -> None)

(* --- Content-addressed fold snapshots (pure cache) ------------------------
   `state`/`send` would otherwise refold the WHOLE event log on every call
   (O(history)). A snapshot stores, per world ref, the folded BackendModel as
   re-parseable source plus the hash of its canonical form. It is ONLY a cache:
   determinism of the fold means a snapshot can never disagree with the refold;
   any read failure (absent, corrupt, ref mismatch, value outside the data
   fragment, stale model type after a program change) silently falls back to
   the full refold, which rewrites it. Deleting `snapshots/` is always safe. *)

let snapshot_path root world = Filename.concat (Filename.concat root "snapshots") world

(* Reload a BackendModel value from snapshot source, kernel-typed against the
   program's CURRENT BackendModel type (a snapshot written before a model-type
   change re-types or is discarded -- never trusted blindly). *)
let model_value (b : Web.backend_contract) text =
  let source =
    "(def __backendModel " ^ Ast.string_of_typ b.Web.backend_model_ty ^ " " ^ text ^ ")\n"
  in
  let checked = Parser.parse_string source |> Kernel.check_program in
  fst (Runtime.normalize_def checked "__backendModel")

let read_snapshot root (b : Web.backend_contract) world =
  match
    if Sys.file_exists (snapshot_path root world) then
      Some (Ledger.read_file (snapshot_path root world))
    else None
  with
  | None -> None
  | Some content -> (
      try
        match String.split_on_char '\n' content with
        | ref_line :: model_line :: _
          when String.length ref_line > 10
               && String.equal (String.sub ref_line 0 10) "model-ref="
               && String.length model_line > 6
               && String.equal (String.sub model_line 0 6) "model=" ->
            let declared = String.sub ref_line 10 (String.length ref_line - 10) in
            let source =
              Scanf.unescaped (String.sub model_line 6 (String.length model_line - 6))
            in
            let value = model_value b source in
            if String.equal declared (Hashcons.hash (Runtime.value_to_canonical value)) then
              Some value
            else None
        | _ -> None
      with _ -> None)

let write_snapshot root (_b : Web.backend_contract) world model =
  try
    let source = value_to_source model in
    let dir = Filename.concat root "snapshots" in
    if not (Sys.file_exists dir) then Unix.mkdir dir 0o755;
    Store.write_file_atomic (snapshot_path root world)
      ("model-ref=" ^ Hashcons.hash (Runtime.value_to_canonical model) ^ "\nmodel="
      ^ String.escaped source ^ "\n")
  with _ -> () (* best-effort cache: a model outside the data fragment just isn't snapshotted *)

(* BackendModel of a world: the deterministic fold, served from the snapshot
   cache on hit (O(1)) and refolded + re-snapshotted on miss. *)
let state root checked (b : Web.backend_contract) world =
  match read_snapshot root b world with
  | Some model -> model
  | None ->
      let model =
        List.fold_left
          (fun model msg -> fst (step checked b model msg))
          (initial_model checked b)
          (world_messages root b world)
      in
      write_snapshot root b world model;
      model

let branch_world root =
  let path = Ledger.branch_path root backend_branch in
  if Sys.file_exists path then String.trim (Ledger.read_file path) else Ledger.initial_world

(* Welcome push (Lamdera onConnect): if the app defines
   [onConnect : BackendModel -> ToFrontend] (validated by the backend contract,
   WEB042), evaluate it against the world's current folded model. The dev
   server pushes the result to each client right when it subscribes to
   /__events, so every (re)connection resynchronizes the page from the fold.
   Like broadcasts this is an ephemeral OUTPUT effect — never recorded in the
   ledger; it re-derives from the deterministic fold. *)
let connect_value root checked (b : Web.backend_contract) world =
  match b.Web.on_connect_def with
  | None -> None
  | Some d ->
      let f = fst (Runtime.normalize_def checked d.Kernel.def.Ast.name) in
      Some (Runtime.apply checked f (state root checked b world))

(* Type-check the message, fold it on top of the world's current model, and
   only then append the event and advance the `backend` branch. Returns
   (event, next_world, model', cmd). *)
let send root checked (b : Web.backend_contract) world text =
  let msg = message_value b text in
  let model = state root checked b world in
  let model', cmd = step checked b model msg in
  let event, next_world =
    Ledger.record_to_backend root world ~message:text
      ~message_type:(Ast.string_of_typ b.Web.to_backend_ty) ~message_ref:(message_ref msg)
  in
  ignore (Ledger.fork root backend_branch next_world);
  (* Snapshot the new world so the next send starts O(1) from here. *)
  write_snapshot root b next_world model';
  (event, next_world, model', cmd)

(* Typed transport entry (sendToBackend): the ToBackend message arrives as an
   already-typed Runtime value (decoded from the browser's value-JSON). Render it
   to source and route through [send] so the `to-backend` ledger event keeps its
   exact form (canonical message text + message-ref) — replay is identical
   whether the message came in as text or as a typed value. Re-typing in [send]
   re-validates the value against ToBackend as defense-in-depth. *)
let send_value root checked (b : Web.backend_contract) world value =
  send root checked b world (value_to_source value)
