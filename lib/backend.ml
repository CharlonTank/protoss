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

(* BackendModel of a world: the deterministic fold. *)
let state root checked (b : Web.backend_contract) world =
  List.fold_left
    (fun model msg -> fst (step checked b model msg))
    (initial_model checked b)
    (world_messages root b world)

let branch_world root =
  let path = Ledger.branch_path root backend_branch in
  if Sys.file_exists path then String.trim (Ledger.read_file path) else Ledger.initial_world

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
  (event, next_world, model', cmd)
