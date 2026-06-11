(* protoss doctor --v1: executable acceptance proofs for the V1.0 ship
   checklist (protoss_v1_ship_checklist.md).

   Design decisions (see docs/v1-goals.md, G1):
   - The doctor is self-sufficient: its proofs run over Protoss *source
     embedded in this module*, so it is independent of the working directory
     and validates a Protoss installation rather than a checkout. Proofs that
     need an on-disk artifact (the spec file) are best-effort: located via
     [find_up] and reported [Not_yet] when absent.
   - Every proof is one [check]. A check is [Pass], [Fail] (an available proof
     is broken -> the doctor exits non-zero), or [Not_yet] (the proof is not
     wired up yet -> reported with its checklist item, does not fail the run).
     This keeps the report honest: "not yet proven" is never silently "ok".
   - Proofs requiring a project/store/ledger/prelude are reported [Not_yet]
     until the corresponding queue goals wire them in (they replace the
     matching [Not_yet] entry here). *)

type status =
  | Pass
  | Fail of string
  | Not_yet of string  (* checklist item / reason the proof is not wired yet *)

type check = {
  id : string;
  section : string;  (* ship-checklist section, e.g. "6.5" *)
  description : string;
  run : unit -> status;
}

(* Checks that build the full prelude (slow): run by [protoss doctor --v1] but
   skipped by the fast dev-loop test sweep, which must stay near-instant. *)
let heavy_ids = [ "priority-demo"; "benchmarks-thresholds" ]

let is_heavy (c : check) = List.mem c.id heavy_ids

(* ----- proof helpers (over embedded source) ------------------------------ *)

let check_source src = Kernel.check_program (Parser.parse_string src)

let hash_of src = Kernel.hash_program (check_source src)

let pass_if cond detail = if cond then Pass else Fail detail

(* A source that must be accepted: any exception bubbles up to [run_one] and
   becomes a Fail. *)
let expect_accept src =
  ignore (hash_of src);
  Pass

(* A source that must be rejected by a *structured* kernel error. A raw
   exception (non-structured) bubbles up to [run_one] -> Fail, which is the
   right outcome: a rejection must be structured. *)
let expect_reject ~what src =
  match (try `Accepted (hash_of src) with Kernel.Error _ -> `Rejected) with
  | `Rejected -> Pass
  | `Accepted _ -> Fail (what ^ ": expected a structured rejection, but it was accepted")

let same_hash a b = String.equal (hash_of a) (hash_of b)

let contains needle haystack =
  let nl = String.length needle and hl = String.length haystack in
  let rec at i = i + nl <= hl && (String.equal (String.sub haystack i nl) needle || at (i + 1)) in
  nl = 0 || at 0

let read_file path =
  let ic = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () -> really_input_string ic (in_channel_length ic))

let write_file path contents =
  let oc = open_out_bin path in
  Fun.protect ~finally:(fun () -> close_out_noerr oc) (fun () -> output_string oc contents)

(* ----- embedded programs ------------------------------------------------- *)

let rich_program =
  String.concat "\n"
    [
      "(record Counter (label String) (value Nat))";
      "(variant Status (Idle Unit) (Running Nat))";
      "(def start Status (variant Running 1))";
      "(def double (-> Nat Nat) (lambda (x Nat) (foldNat x x (lambda (a Nat) (succ a)))))";
      "(def model Counter (record (label \"n\") (value 2)))";
      "(def step (-> Counter Nat) (lambda (c Counter) (double (get c value))))";
    ]

let double_sexp =
  "(def double (-> Nat Nat) (lambda (x Nat) (foldNat x x (lambda (a Nat) (succ a)))))"

let double_elm = "double : Nat -> Nat\ndouble x = foldNat x x (\\a -> succ a)\n"

let alpha_x = "(def f (-> Nat Nat) (lambda (x Nat) (succ x)))"

let alpha_y = "(def f (-> Nat Nat) (lambda (y Nat) (succ y)))"

let cap_missing = "(def leak (Process String) (Human.ask \"?\"))"

let cap_declared = "(capabilities Human.ask)\n(def ask (Process String) (Human.ask \"?\"))"

let non_structural_rec =
  "(defrec loop (-> Nat Nat) (nat n) (zero 0) (step acc (loop acc)))"

let structural_rec = "(defrec count (-> Nat Nat) (nat n) (zero 0) (step acc (succ acc)))"

let ill_typed = "(def bad Nat true)"

(* Hostile inputs that once crashed the parser with a raw int_of_string; they
   must now fail through the structured error layer, never a bare exception. *)
let hostile_inputs =
  [
    "(def m (TVar abc) 0)";
    "(def m (Forall x Nat) (lambda (y Nat) y))";
  ]

(* ----- proofs ------------------------------------------------------------ *)

let ptc_roundtrip src =
  let checked = check_source src in
  let canonical = Kernel.serialize_checked_program checked in
  let caps, defs = Kernel.parse_serialized_program canonical in
  let rechecked = Kernel.checked_of_canonical caps defs in
  pass_if
    (String.equal (Kernel.hash_program checked) (Kernel.hash_program rechecked)
    && String.equal canonical (Kernel.serialize_checked_program rechecked))
    "Protoss/C round-trip changed the canonical hash"

let ptb_roundtrip src =
  let checked = check_source src in
  let binary = Canonical_binary.checked_to_binary checked in
  let back = Canonical_binary.checked_of_binary binary in
  pass_if
    (String.equal (Kernel.hash_program checked) (Kernel.hash_program back)
    && String.equal binary (Canonical_binary.checked_to_binary back))
    "Protoss/B round-trip changed the canonical hash or is non-deterministic"

(* A hostile input must be rejected through the structured error layer
   (Kernel.Error / Parser.Error), not by a raw exception (Failure, Not_found,
   …) escaping to the user. *)
let structured_rejection src =
  match check_source src with
  | _ -> Fail ("hostile input was accepted: " ^ src)
  | exception (Kernel.Error _ | Parser.Error _) -> Pass
  | exception e -> Fail ("hostile input crashed unstructured: " ^ Printexc.to_string e)

(* The MCP server speaks the JSON-RPC contract and never crashes on a bad
   call. Mutation always flows through validated patches (applyPatch ->
   Agent_protocol.commit_patch_json), so check cannot be bypassed. *)
let mcp_contract () =
  let resp req = match Mcp_server.handle_message req with Some s -> s | None -> "" in
  let init = resp {|{"jsonrpc":"2.0","id":1,"method":"initialize","params":{}}|} in
  let tools = resp {|{"jsonrpc":"2.0","id":2,"method":"tools/list","params":{}}|} in
  let bad =
    resp
      {|{"jsonrpc":"2.0","id":3,"method":"tools/call","params":{"name":"protoss.nope","arguments":{}}}|}
  in
  let core_tools = [ "protoss.checkPatch"; "protoss.applyPatch"; "protoss.query"; "protoss.runHarness" ] in
  if not (contains "protocolVersion" init) then Fail "MCP initialize lacks protocolVersion"
  else if not (List.for_all (fun t -> contains t tools) core_tools) then
    Fail "MCP tools/list is missing a core tool"
  else if not (contains "isError" bad || contains "error" bad) then
    Fail "MCP rejected call did not return a structured error"
  else Pass

let bytecode_roundtrip src =
  let checked = check_source src in
  let m = Bytecode.compile_checked checked in
  let bytes1 = Bytecode.encode_module m in
  let bytes2 = Bytecode.encode_module (Bytecode.decode_module bytes1) in
  pass_if
    (String.equal bytes1 bytes2
    && String.equal (Bytecode.hash_module m)
         (Bytecode.hash_module (Bytecode.compile_checked (check_source src))))
    "bytecode encode/decode is non-deterministic or not round-trip stable"

let bytecode_parity src =
  let checked = check_source src in
  let diverged =
    List.filter_map
      (fun (d : Kernel.checked_def) ->
        let name = d.def.name in
        let vm = Bytecode_vm.vm_canonical checked name in
        let interp = Runtime.value_to_canonical (fst (Runtime.normalize_def checked name)) in
        if String.equal vm interp then None
        else Some (name ^ "\n    vm=" ^ vm ^ "\n    interp=" ^ interp))
      checked.defs
  in
  pass_if (diverged = [])
    ("bytecode VM diverges from the interpreter on: " ^ String.concat ", " diverged)

let human_roundtrip src =
  let program = Parser.parse_string src in
  let rendered = Surface_syntax.render_program program in
  let reparsed = Parser.parse_string rendered in
  pass_if
    (String.equal
       (Kernel.hash_program (Kernel.check_program program))
       (Kernel.hash_program (Kernel.check_program reparsed)))
    "Protoss/H projection changed the canonical hash"

(* best-effort: locate a repo file from the current directory upward. *)
let rec find_up dir rel =
  let candidate = Filename.concat dir rel in
  if Sys.file_exists candidate then Some candidate
  else
    let parent = Filename.dirname dir in
    if String.equal parent dir then None else find_up parent rel

let spec_audit () =
  match find_up (Sys.getcwd ()) "protoss-spec.md" with
  | None -> Not_yet "checklist §22: protoss-spec.md not found from CWD (best-effort)"
  | Some path -> (
      match Spec_audit.check_file path with
      | _ -> Pass
      | exception Failure msg ->
          Fail ("spec audit reported missing evidence:\n" ^ String.trim msg))

(* Copy a project tree (minus any .protoss store) so a build can run in a
   throwaway location, leaving the repo untouched — the test suite must never
   inherit a generated store (see CLAUDE.md). *)
let rec copy_tree src dst =
  if Sys.is_directory src then (
    (try Unix.mkdir dst 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
    Sys.readdir src |> Array.to_list |> List.sort String.compare
    |> List.iter (fun name ->
           if not (String.equal name ".protoss") then
             copy_tree (Filename.concat src name) (Filename.concat dst name)))
  else
    let ic = open_in_bin src and oc = open_out_bin dst in
    Fun.protect
      ~finally:(fun () -> close_in_noerr ic; close_out_noerr oc)
      (fun () -> output_string oc (really_input_string ic (in_channel_length ic)))

let rec rm_rf path =
  if Sys.file_exists path then
    if Sys.is_directory path then (
      Sys.readdir path |> Array.iter (fun n -> rm_rf (Filename.concat path n));
      try Unix.rmdir path with _ -> ())
    else try Sys.remove path with _ -> ()

(* Build a golden project from a pid-qualified throwaway copy, leaving no
   .protoss store behind in the repo. The temp path never enters any hash, so
   output determinism is preserved. *)
let golden_build base name =
  let tmp =
    Filename.concat (Filename.get_temp_dir_name ())
      (Printf.sprintf "protoss-doctor-golden-%d-%s" (Unix.getpid ()) name)
  in
  rm_rf tmp;
  copy_tree (Filename.concat base name) tmp;
  Fun.protect
    ~finally:(fun () -> rm_rf tmp)
    (fun () ->
      ignore (Workspace.build ~write:false (Workspace.parse_manifest (Workspace.project_root tmp))))

let golden_valid =
  [ "hello-world"; "pure-library"; "process-clock"; "human-ask"; "migration-demo"; "patch-demo" ]

(* Build a golden project for real (write:true) in a pid-qualified temp copy and
   run [f store tmp]; the temp tree (and its store) is always removed. Golden
   projects are stdlib="none", so this is fast (no prelude eval). *)
let with_golden_store base name f =
  let tmp =
    Filename.concat (Filename.get_temp_dir_name ())
      (Printf.sprintf "protoss-doctor-store-%d-%s" (Unix.getpid ()) name)
  in
  rm_rf tmp;
  copy_tree (Filename.concat base name) tmp;
  Fun.protect
    ~finally:(fun () -> rm_rf tmp)
    (fun () ->
      let manifest = Workspace.parse_manifest (Workspace.project_root tmp) in
      ignore (Workspace.build manifest);
      f (Workspace.store_root manifest) tmp manifest)

let with_golden_base f =
  match find_up (Sys.getcwd ()) "examples/golden" with
  | None -> Not_yet "examples/golden not found from CWD (best-effort)"
  | Some base -> f base

(* §11: a passing harness reports pass and a failing one reports fail. *)
let harness_proof () =
  let checked = check_source "(def two Nat 2)" in
  let status content =
    match Json.field "status" (Json.parse (Harness.run_json checked ~source:"<doctor>" content)) with
    | Some v -> ( match Json.string v with Some s -> s | None -> "")
    | None -> ""
  in
  if not (String.equal (status "harness ok = unit two == 2\n") "pass") then
    Fail "a passing harness did not report pass"
  else if String.equal (status "harness bad = unit two == 3\n") "pass" then
    Fail "a failing harness reported pass"
  else Pass

(* §10.4: a text edit is a view — the difference between two built versions
   derives a structured patch candidate (a changed def becomes a ReplaceDef),
   and that derived patch is itself valid against the original version. *)
let edit_import () =
  let mk dir body =
    rm_rf dir;
    (try Unix.mkdir dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
    (try Unix.mkdir (Filename.concat dir "src") 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
    write_file (Filename.concat dir "protoss.toml")
      "name = \"edit\"\n\
       version = \"0.0.0\"\n\
       entrypoints = [\"src/main.protoss\"]\n\
       stdlib = \"none\"\n\
       source_dirs = [\"src\"]\n\
       store_dir = \".protoss/store\"\n\
       cache_dir = \".protoss/cache\"\n\
       capabilities = []\n";
    write_file (Filename.concat dir "src/main.protoss") body;
    let m = Workspace.parse_manifest (Workspace.project_root dir) in
    ignore (Workspace.build m);
    Workspace.store_root m
  in
  let tmp suffix =
    Filename.concat (Filename.get_temp_dir_name ())
      (Printf.sprintf "protoss-doctor-edit%s-%d" suffix (Unix.getpid ()))
  in
  let a = tmp "a" and b = tmp "b" in
  Fun.protect
    ~finally:(fun () -> rm_rf a; rm_rf b)
    (fun () ->
      let store_a = mk a "(def n Nat 1)\n" in
      let store_b = mk b "(def n Nat 2)\n" in
      let patch = Workspace.patch_from_diff store_a store_b in
      if not (contains "ReplaceDef" patch) then
        Fail "edit import did not derive a ReplaceDef for a changed def"
      else (
        let pf = Filename.concat a "derived.json" in
        write_file pf patch;
        ignore (Patch.check store_a pf);
        Pass))

(* §7.1/§6.5: the effect cache key is partitioned by capability scope and
   world ref (no cross-capability or cross-world cache leak) and stable for
   identical inputs; the pure key is stable and partitions by def. *)
let cache_partitioning () =
  let k caps world =
    Runtime.process_eval_key ~def_id:"d" ~world_ref:world ~cap_scope:caps ~runtime_policy:"p"
  in
  let base = k [ "Http.get" ] "w" in
  if String.equal base (k [ "Local.storage" ] "w") then
    Fail "effect cache key does not partition by capability scope (leak risk)"
  else if String.equal base (k [ "Http.get" ] "w2") then
    Fail "effect cache key does not partition by world ref"
  else if not (String.equal base (k [ "Http.get" ] "w")) then
    Fail "effect cache key is not stable for identical inputs"
  else
    let p a = Runtime.eval_key ~def_id:"d" ~args_hash:a ~runtime_policy:"p" in
    if String.equal (p "a1") (p "a2") then Fail "pure cache key does not partition by arguments"
    else if not (String.equal (p "a1") (p "a1")) then Fail "pure cache key is not stable"
    else Pass

(* §8.3: a Process suspends on a request, then the ledger resumes it
   deterministically with the supplied response. *)
let ledger_replay () =
  let checked =
    check_source "(capabilities Human.ask)\n(def ask (Process String) (Human.ask \"Name?\"))"
  in
  let dir =
    Filename.concat (Filename.get_temp_dir_name ())
      (Printf.sprintf "protoss-doctor-ledger-%d" (Unix.getpid ()))
  in
  rm_rf dir;
  Fun.protect
    ~finally:(fun () -> rm_rf dir)
    (fun () ->
      let r =
        Invariants.check_ledger_process_checked ~ledger:dir "<doctor>" checked "ask" "\"Ada\""
      in
      if contains "Ada" r.Invariants.result then Pass
      else Fail ("ledger replay did not resume with the response: " ^ r.Invariants.result))

(* §5.3: a real build writes a UniverseRoot and the project audits clean. *)
let store_universe_root () =
  with_golden_base (fun base ->
      with_golden_store base "hello-world" (fun store _ manifest ->
          if not (Sys.file_exists (Filename.concat store "universe.root")) then
            Fail "build did not write universe.root"
          else if not (String.equal "Audit OK\n" (Workspace.audit manifest)) then
            Fail "freshly built project does not audit clean"
          else Pass))

(* §10.3: a structured patch is checked, applied, and the audit chain verifies. *)
let patch_check_audit () =
  with_golden_base (fun base ->
      with_golden_store base "patch-demo" (fun store tmp _ ->
          let patch = Filename.concat tmp "patches/add_total.json" in
          if not (Sys.file_exists patch) then Not_yet "patch-demo add_total.json missing"
          else (
            ignore (Patch.check store patch);
            ignore (Patch.apply store patch);
            ignore (Patch_audit.verify_latest_matches_store store);
            Pass)))

(* §12: a built package locks, and check_package/check_lock agree. *)
let packages_lock () =
  with_golden_base (fun base ->
      with_golden_store base "pure-library" (fun _ _ manifest ->
          let _, lock_hash = Workspace.write_lock manifest in
          let relocked = Workspace.check_lock manifest in
          if not (String.equal lock_hash relocked) then
            Fail "lock hash is not stable across lock/check"
          else (
            ignore (Workspace.write_package manifest);
            ignore (Workspace.check_package manifest);
            Pass)))

let golden_projects () =
  match find_up (Sys.getcwd ()) "examples/golden" with
  | None -> Not_yet "checklist §19: examples/golden not found from CWD (best-effort)"
  | Some base -> (
      let failures =
        List.filter_map
          (fun name ->
            match golden_build base name with
            | () -> None
            | exception Kernel.Error m -> Some (name ^ ": " ^ m)
            | exception Workspace.Error m -> Some (name ^ ": " ^ m)
            | exception Failure m -> Some (name ^ ": " ^ m))
          golden_valid
      in
      if failures <> [] then Fail ("golden project(s) failed to build: " ^ String.concat "; " failures)
      else
        (* capability-denied-demo must be rejected for a missing capability. *)
        match
          try `Built (golden_build base "capability-denied-demo")
          with Kernel.Error m | Workspace.Error m | Failure m -> `Rejected m
        with
        | `Built () -> Fail "capability-denied-demo built but must be rejected"
        | `Rejected m ->
            if contains "capability" m then Pass
            else Fail ("capability-denied-demo rejected for the wrong reason: " ^ m))

(* Rewrite a project's relative stdlib path to absolute so the project still
   builds after being copied out of the repo tree. *)
let absolutize_stdlib toml_path prelude_abs =
  let lines = String.split_on_char '\n' (read_file toml_path) in
  let lines =
    List.map
      (fun line ->
        let t = String.trim line in
        if String.length t >= 6 && String.equal (String.sub t 0 6) "stdlib" then
          "stdlib = " ^ Ast.quote prelude_abs
        else line)
      lines
  in
  write_file toml_path (String.concat "\n" lines)

(* HEAVY (full-prelude build): the §14.4 demo. The todo app is migrated to add a
   per-item `priority` through a structured patch — built, checked, and applied
   from a pid-qualified copy (stdlib path absolutized), leaving the repo clean. *)
let priority_demo () =
  match
    (find_up (Sys.getcwd ()) "examples/web/todo_app", find_up (Sys.getcwd ()) "stdlib/prelude.protoss")
  with
  | None, _ | _, None -> Not_yet "checklist §14: todo_app or prelude not found (best-effort)"
  | Some app, Some prelude ->
      let tmp =
        Filename.concat (Filename.get_temp_dir_name ())
          (Printf.sprintf "protoss-doctor-priority-%d" (Unix.getpid ()))
      in
      rm_rf tmp;
      copy_tree app tmp;
      absolutize_stdlib (Filename.concat tmp "protoss.toml") prelude;
      Fun.protect
        ~finally:(fun () -> rm_rf tmp)
        (fun () ->
          let patch = Filename.concat tmp "patches/add_priority.json" in
          if not (Sys.file_exists patch) then Not_yet "checklist §14: add_priority.json missing"
          else
            let manifest = Workspace.parse_manifest (Workspace.project_root tmp) in
            ignore (Workspace.build manifest);
            let store = Workspace.store_root manifest in
            (* check must accept the patch, then apply must commit it. *)
            ignore (Patch.check store patch);
            ignore (Patch.apply store patch);
            Pass)

(* §20 HEAVY: a full-prelude project build stays under a generous wall-clock
   ceiling — a coarse "did it get catastrophically slower" gate (the precise,
   machine-comparable numbers live in benchmarks/run.sh). *)
let benchmarks_thresholds () =
  match
    (find_up (Sys.getcwd ()) "examples/web/todo_app", find_up (Sys.getcwd ()) "stdlib/prelude.protoss")
  with
  | None, _ | _, None -> Not_yet "checklist §20: todo_app or prelude not found (best-effort)"
  | Some app, Some prelude ->
      let tmp =
        Filename.concat (Filename.get_temp_dir_name ())
          (Printf.sprintf "protoss-doctor-bench-%d" (Unix.getpid ()))
      in
      rm_rf tmp;
      copy_tree app tmp;
      absolutize_stdlib (Filename.concat tmp "protoss.toml") prelude;
      Fun.protect
        ~finally:(fun () -> rm_rf tmp)
        (fun () ->
          let manifest = Workspace.parse_manifest (Workspace.project_root tmp) in
          let t0 = Unix.gettimeofday () in
          ignore (Workspace.build manifest);
          let secs = Unix.gettimeofday () -. t0 in
          pass_if (secs < 20.0)
            (Printf.sprintf "full-prelude build took %.1fs (ceiling 20s)" secs))

(* ----- the check list ---------------------------------------------------- *)

let checks : check list =
  [
    {
      id = "kernel-grammar";
      section = "3.1";
      description = "executable kernel grammar is published and a multi-form program checks";
      run =
        (fun () ->
          if String.length Kernel.executable_grammar_text = 0 then
            Fail "Kernel.executable_grammar_text is empty"
          else expect_accept rich_program);
    };
    {
      id = "hash-determinism";
      section = "3.2";
      description = "the same source canonicalizes to the same hash on repeated checks";
      run = (fun () -> pass_if (same_hash rich_program rich_program) "non-deterministic hash");
    };
    {
      id = "alpha-stability";
      section = "3.2";
      description = "alpha-equivalent programs share one canonical hash";
      run = (fun () -> pass_if (same_hash alpha_x alpha_y) "binder names changed the hash");
    };
    {
      id = "syntax-equivalence";
      section = "6.5";
      description = "S-expression and Elm-like sources hash identically";
      run = (fun () -> pass_if (same_hash double_sexp double_elm) "syntaxes diverge in hash");
    };
    {
      id = "ptc-roundtrip";
      section = "6.3";
      description = "Protoss/C serialize -> parse -> hash is stable";
      run = (fun () -> ptc_roundtrip rich_program);
    };
    {
      id = "ptb-roundtrip";
      section = "6.4";
      description = "Protoss/B encode -> decode is deterministic and hash-stable";
      run = (fun () -> ptb_roundtrip rich_program);
    };
    {
      id = "human-projection";
      section = "6.2";
      description = "Protoss/H render -> parse preserves the canonical hash";
      run = (fun () -> human_roundtrip double_sexp);
    };
    {
      id = "totality-rejects-general-recursion";
      section = "3.3";
      description = "general recursion is rejected, structural recursion is accepted";
      run =
        (fun () ->
          match expect_reject ~what:"general recursion" non_structural_rec with
          | Pass -> expect_accept structural_rec
          | other -> other);
    };
    {
      id = "typecheck-rejects-ill-typed";
      section = "4";
      description = "an ill-typed definition is rejected";
      run = (fun () -> expect_reject ~what:"ill-typed definition" ill_typed);
    };
    {
      id = "cache-partitioning";
      section = "7.1";
      description = "the eval cache key partitions by capability scope and world (no leak)";
      run = cache_partitioning;
    };
    {
      id = "capability-enforcement";
      section = "9";
      description = "an undeclared effect is rejected, a declared one is accepted";
      run =
        (fun () ->
          match expect_reject ~what:"undeclared capability" cap_missing with
          | Pass -> expect_accept cap_declared
          | other -> other);
    };
    {
      id = "spec-audit";
      section = "22";
      description = "protoss-spec.md checked claims all carry evidence markers";
      run = spec_audit;
    };
    (* Proofs wired by later queue goals; honest placeholders, never silent. *)
    {
      id = "mcp-contract";
      section = "10.1";
      description = "MCP server speaks JSON-RPC, exposes core tools, rejects bad calls structurally";
      run = mcp_contract;
    };
    {
      id = "store-universe-root";
      section = "5.3";
      description = "a real build writes a UniverseRoot and the project audits clean";
      run = store_universe_root;
    };
    {
      id = "ledger-replay";
      section = "8.3";
      description = "a Process suspends on a request and the ledger resumes it deterministically";
      run = ledger_replay;
    };
    {
      id = "patch-check-audit";
      section = "10.3";
      description = "a structured patch is checked, applied, and its audit chain verifies";
      run = patch_check_audit;
    };
    {
      id = "edit-import";
      section = "10.4";
      description = "a text edit (diff of two versions) derives a valid structured patch";
      run = edit_import;
    };
    {
      id = "harness";
      section = "11";
      description = "a passing harness reports pass and a failing one reports fail";
      run = harness_proof;
    };
    {
      id = "packages-lock-registries";
      section = "12";
      description = "a built package locks deterministically and check_package agrees";
      run = packages_lock;
    };
    {
      id = "golden-projects";
      section = "19";
      description = "golden projects build, and capability-denied is rejected";
      run = golden_projects;
    };
    {
      id = "priority-demo";
      section = "14";
      description = "todo app gains a per-item priority via a checked+applied structured patch";
      run = priority_demo;
    };
    {
      id = "bytecode-encoding";
      section = "15";
      description = "graph compiles to bytecode with deterministic, round-trip-stable encoding";
      run = (fun () -> bytecode_roundtrip rich_program);
    };
    {
      id = "bytecode-parity";
      section = "15";
      description = "bytecode VM executes at parity with the reference interpreter";
      run = (fun () -> bytecode_parity rich_program);
    };
    {
      id = "self-hosted-canonicalizer-parity";
      section = "17";
      description = "Protoss canonicalizer matches the kernel byte-for-byte";
      run =
        (fun () ->
          Not_yet
            "checklist §17: proven by the @selftest __canon_parity_* sweep; not duplicated \
             in the doctor (full prelude eval)");
    };
    {
      id = "self-hosted-patch-validator-parity";
      section = "17";
      description = "Protoss patch validator matches Patch.check verdicts";
      run = (fun () -> Not_yet "checklist §17: wired by goal G8");
    };
    {
      id = "structured-errors-on-hostile-input";
      section = "21";
      description = "malformed input fails through the structured error layer, never a raw crash";
      run =
        (fun () ->
          let rec all = function
            | [] -> Pass
            | src :: rest -> ( match structured_rejection src with Pass -> all rest | other -> other)
          in
          all hostile_inputs);
    };
    {
      id = "benchmarks-thresholds";
      section = "20";
      description = "a full-prelude build stays under a generous wall-clock ceiling";
      run = benchmarks_thresholds;
    };
  ]

(* ----- run + report ------------------------------------------------------ *)

let run_one (c : check) : status =
  try c.run () with
  | Kernel.Error msg -> Fail ("kernel error: " ^ msg)
  | Failure msg -> Fail ("failure: " ^ msg)
  | e -> Fail ("unexpected exception: " ^ Printexc.to_string e)

(* Exit code policy: a single broken *available* proof fails the run; Not_yet
   never fails. Exposed for the panic-injection test. *)
let aggregate_exit (statuses : status list) : int =
  if List.exists (function Fail _ -> true | _ -> false) statuses then 1 else 0

let status_label = function Pass -> "PASS" | Fail _ -> "FAIL" | Not_yet _ -> " -- "

let status_word = function Pass -> "pass" | Fail _ -> "fail" | Not_yet _ -> "not-yet"

let status_detail = function Pass -> "" | Fail m -> m | Not_yet m -> m

let render_text results =
  let buf = Buffer.create 1024 in
  Buffer.add_string buf "protoss doctor --v1\n\n";
  List.iter
    (fun (c, st) ->
      Buffer.add_string buf
        (Printf.sprintf "[%s] %-6s %s — %s\n" (status_label st) c.section c.id c.description);
      let detail = status_detail st in
      if String.length detail > 0 then
        Buffer.add_string buf
          (Printf.sprintf "        %s\n"
             (String.concat "\n        " (String.split_on_char '\n' detail))))
    results;
  let count p = List.length (List.filter (fun (_, st) -> p st) results) in
  let passed = count (function Pass -> true | _ -> false) in
  let failed = count (function Fail _ -> true | _ -> false) in
  let not_yet = count (function Not_yet _ -> true | _ -> false) in
  Buffer.add_string buf
    (Printf.sprintf "\nsummary: %d pass, %d fail, %d not-yet\n" passed failed not_yet);
  Buffer.add_string buf
    (if failed = 0 then "V1.0 doctor: OK (no available proof is broken)\n"
     else "V1.0 doctor: FAIL (an available proof is broken)\n");
  Buffer.contents buf

let render_json results =
  let entry (c, st) =
    "{ " ^ Ast.quote "id" ^ ": " ^ Ast.quote c.id ^ ", " ^ Ast.quote "section" ^ ": "
    ^ Ast.quote c.section ^ ", " ^ Ast.quote "status" ^ ": " ^ Ast.quote (status_word st)
    ^ ", " ^ Ast.quote "description" ^ ": " ^ Ast.quote c.description ^ ", "
    ^ Ast.quote "detail" ^ ": " ^ Ast.quote (status_detail st) ^ " }"
  in
  let count p = List.length (List.filter (fun (_, st) -> p st) results) in
  let failed = count (function Fail _ -> true | _ -> false) in
  "{ " ^ Ast.quote "status" ^ ": " ^ Ast.quote (if failed = 0 then "ok" else "fail") ^ ", "
  ^ Ast.quote "summary" ^ ": { " ^ Ast.quote "pass" ^ ": "
  ^ string_of_int (count (function Pass -> true | _ -> false)) ^ ", " ^ Ast.quote "fail"
  ^ ": " ^ string_of_int failed ^ ", " ^ Ast.quote "not_yet" ^ ": "
  ^ string_of_int (count (function Not_yet _ -> true | _ -> false)) ^ " }, "
  ^ Ast.quote "checks" ^ ": [" ^ String.concat ", " (List.map entry results) ^ "] }"

(* Runs every check, prints the report, returns the process exit code. *)
let run ~json : int =
  let results = List.map (fun c -> (c, run_one c)) checks in
  print_string (if json then render_json results ^ "\n" else render_text results);
  aggregate_exit (List.map snd results)
