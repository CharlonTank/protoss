type entry = {
  code : string;
  name : string;
  description : string;
}

let catalog =
  [
    {
      code = "SYN001";
      name = "SyntaxError";
      description = "Source text is not valid Protoss syntax.";
    };
    {
      code = "HUMAN001";
      name = "AmbiguousHumanSyntax";
      description = "Human Protoss/H syntax is ambiguous or unsupported.";
    };
    {
      code = "TYPE001";
      name = "TypeMismatch";
      description = "A value has a different type from the expected type.";
    };
    {
      code = "REF001";
      name = "UnknownReference";
      description = "A definition, type, field, constructor, package, or node reference is unknown.";
    };
    {
      code = "CAP001";
      name = "CapabilityDenied";
      description = "An effect requires a capability that is not declared in scope.";
    };
    {
      code = "CAPABILITY";
      name = "CapabilityDeniedLegacy";
      description = "Effects require explicit capabilities in the project or source.";
    };
    {
      code = "TERM001";
      name = "NonTerminatingRecursion";
      description = "Recursion is not accepted by the structural termination checker.";
    };
    {
      code = "PROC001";
      name = "NonProductiveProcess";
      description = "A process is not accepted as a productive external-effect program.";
    };
    {
      code = "HARNESS001";
      name = "HarnessRegression";
      description = "A proposed change regresses an attached harness.";
    };
    {
      code = "MIGRATION001";
      name = "UnsafeMigration";
      description = "A model or data migration is missing or unsafe.";
    };
    {
      code = "POLICY001";
      name = "PolicyViolation";
      description = "A package, runtime, or patch policy rejects the operation.";
    };
    {
      code = "SECRET001";
      name = "SecretLeakRisk";
      description = "A secret value may escape its declared scope.";
    };
    {
      code = "LOAD001";
      name = "LoadFailure";
      description = "A source, graph, canonical file, or project input could not be loaded.";
    };
    {
      code = "CHECK001";
      name = "CheckFailure";
      description = "Kernel checking rejected the program for a reason without a narrower code.";
    };
    {
      code = "PATCH001";
      name = "PatchRejected";
      description = "A structural patch is invalid or cannot be applied atomically.";
    };
    {
      code = "PATCH_DEPS";
      name = "PatchDependencyMismatch";
      description = "Patch deps must exactly match canonical definition dependencies.";
    };
    {
      code = "AUDIT001";
      name = "AuditFailure";
      description = "A content-addressed audit or invariant check failed.";
    };
    {
      code = "STORE001";
      name = "StoreFailure";
      description = "A content-addressed store operation failed.";
    };
    {
      code = "WORKSPACE001";
      name = "WorkspaceFailure";
      description = "A project workspace, lockfile, package, or interface operation failed.";
    };
    {
      code = "WEB001";
      name = "WebMissingDefinition";
      description = "Missing init, update, or view definition in a web app.";
    };
    {
      code = "WEB007";
      name = "WebViewMessageMismatch";
      description = "view returns a View whose message type does not match update.";
    };
    {
      code = "WEB030";
      name = "WebPortInUse";
      description = "The dev server port is already taken; the error names the holder and a free port.";
    };
    {
      code = "BACKEND001";
      name = "BackendMissing";
      description =
        "The app defines no backend half (updateBackend/initBackend) for a backend operation.";
    };
    {
      code = "BACKEND002";
      name = "BackendMessageInvalid";
      description =
        "A ToBackend message failed to parse or type-check against the app's ToBackend type.";
    };
    {
      code = "BACKEND003";
      name = "BackendUpdateShape";
      description = "updateBackend returned a value that is not a (Tuple BackendModel (Cmd ...)).";
    };
    {
      code = "BACKEND004";
      name = "BackendEventMalformed";
      description = "A ledger to-backend event is missing its payload or message-ref field.";
    };
    {
      code = "BACKEND005";
      name = "BackendEventIntegrity";
      description =
        "A replayed to-backend event's message does not re-type to its recorded canonical ref.";
    };
    {
      code = "DEPLOY001";
      name = "DeployToolingMissing";
      description = "A required deployment tool (hcloud) is not installed or configured.";
    };
    {
      code = "DEPLOY002";
      name = "DeployCommandFailed";
      description = "A deployment step (hcloud/ssh/rsync/curl) exited non-zero.";
    };
    {
      code = "DEPLOY003";
      name = "DeployNoSshKey";
      description = "No SSH key is registered in the hcloud project.";
    };
    {
      code = "DEPLOY004";
      name = "DeployServerUnreachable";
      description = "The provisioned server did not become reachable over SSH.";
    };
    {
      code = "DEPLOY005";
      name = "DeployDnsFailed";
      description = "The Cloudflare zone or DNS record could not be read or written.";
    };
    {
      code = "RUNTIME001";
      name = "RuntimeFailure";
      description = "A runtime store, world, or suspended-process operation failed.";
    };
    {
      code = "SELF_FMT001";
      name = "SelfHostedFormatFailure";
      description = "The self-hosted formatter rejected the source.";
    };
    {
      code = "SELF_CANON001";
      name = "SelfHostedCanonicalizerFailure";
      description =
        "The self-hosted canonicalizer rejected the source or an unsupported form.";
    };
    {
      code = "SELF_TC000";
      name = "SelfHostedTypecheckFailure";
      description = "The self-hosted typechecker rejected the source outside a narrower code.";
    };
    {
      code = "SELF_TC001";
      name = "SelfHostedUnknownVariable";
      description = "Self-hosted typechecker: unknown variable.";
    };
    {
      code = "SELF_TC002";
      name = "SelfHostedTypeMismatch";
      description = "Self-hosted typechecker: expected and actual types differ.";
    };
    {
      code = "SELF_TC003";
      name = "SelfHostedNonFunctionApplication";
      description = "Self-hosted typechecker: application target is not a function.";
    };
    {
      code = "SELF_TC004";
      name = "SelfHostedUnsupportedConstruct";
      description = "Self-hosted typechecker: construct is outside the supported subset.";
    };
    {
      code = "SELF_TC005";
      name = "SelfHostedBranchMismatch";
      description = "Self-hosted typechecker: case branch types differ.";
    };
    {
      code = "SELF_TC006";
      name = "SelfHostedRecordFieldError";
      description = "Self-hosted typechecker: record field is missing or unknown.";
    };
    {
      code = "SELF_TC007";
      name = "SelfHostedVariantError";
      description = "Self-hosted typechecker: variant constructor or payload is invalid.";
    };
    {
      code = "SELF_TC008";
      name = "SelfHostedParseFailure";
      description = "Self-hosted typechecker: text parsing failed.";
    };
    {
      code = "SELF_TC009";
      name = "SelfHostedNonExhaustiveCase";
      description = "Self-hosted typechecker: case expression is not exhaustive.";
    };
    {
      code = "SELF_TC010";
      name = "SelfHostedStaticFailure";
      description = "Self-hosted typechecker: shared static frontend rejected the declarations.";
    };
    {
      code = "SELF_TC011";
      name = "SelfHostedPolymorphicInstantiationError";
      description = "Self-hosted typechecker: polymorphic instantiation is invalid.";
    };
    {
      code = "INPUT001";
      name = "InputFailure";
      description = "Input ended unexpectedly or could not be read as requested.";
    };
    {
      code = "SYSTEM001";
      name = "SystemFailure";
      description = "The host operating system rejected an operation.";
    };
    {
      code = "ERROR001";
      name = "GenericFailure";
      description = "A public command failed without a narrower stable code.";
    };
    {
      code = "INTERNAL001";
      name = "InternalFailure";
      description = "Protoss hit an internal implementation failure.";
    };
  ]

let find code = List.find_opt (fun entry -> String.equal entry.code code) catalog

let explain code =
  match find code with
  | Some entry -> entry.description
  | None -> "Unknown error code."

let list_text () =
  catalog
  |> List.map (fun entry -> entry.code ^ " " ^ entry.name ^ " - " ^ entry.description)
  |> String.concat "\n"

let taxonomy_names () =
  catalog |> List.map (fun entry -> entry.name) |> List.sort_uniq String.compare

let contains haystack needle =
  let haystack = String.lowercase_ascii haystack in
  let needle = String.lowercase_ascii needle in
  let lh = String.length haystack and ln = String.length needle in
  let rec loop i =
    i + ln <= lh && (String.sub haystack i ln = needle || loop (i + 1))
  in
  ln = 0 || loop 0

let first_word s =
  let len = String.length s in
  let rec stop i = if i >= len || s.[i] = ' ' || s.[i] = ':' then i else stop (i + 1) in
  if len = 0 then "" else String.sub s 0 (stop 0)

let code_prefix msg =
  let candidate = first_word msg in
  match find candidate with Some _ -> Some candidate | None -> None

let code_for_check_message msg =
  match code_prefix msg with
  | Some code -> code
  | None ->
      if contains msg "capability" || contains msg "capabilities" then "CAP001"
      else if
        contains msg "recursion" || contains msg "recursive" || contains msg "recur"
      then "TERM001"
      else if
        contains msg "unknown" || contains msg "unbound" || contains msg "missing"
        || contains msg "not found"
      then "REF001"
      else if contains msg "process" && contains msg "productive" then "PROC001"
      else if contains msg "expected" || contains msg "type" || contains msg "got" then
        "TYPE001"
      else "CHECK001"

let code_for_load_message msg =
  match code_prefix msg with
  | Some code -> code
  | None ->
      let check_code = code_for_check_message msg in
      if not (String.equal check_code "CHECK001") then check_code
      else if
        contains msg "unterminated" || contains msg "unexpected"
        || contains msg "syntax"
      then "SYN001"
      else "LOAD001"

let code_for_cli_kind kind msg =
  match code_prefix msg with
  | Some code -> code
  | None -> (
      match kind with
      | "parse error" ->
          if contains msg "ambiguous" then "HUMAN001" else "SYN001"
      | "load error" -> code_for_load_message msg
      | "check error" -> code_for_check_message msg
      | "patch error" ->
          if contains msg "migration" then "MIGRATION001"
          else if contains msg "harness" then "HARNESS001"
          else if contains msg "policy" then "POLICY001"
          else "PATCH001"
      | "harness error" -> "HARNESS001"
      | "store error" -> "STORE001"
      | "workspace error" ->
          if contains msg "policy" then "POLICY001" else "WORKSPACE001"
      | "web error" -> "WEB001"
      | "backend error" -> "BACKEND001"
      | "deploy error" -> "DEPLOY002"
      | "runtime error" -> "RUNTIME001"
      | "self fmt error" -> "SELF_FMT001"
      | "self canon error" -> "SELF_CANON001"
      | "self typecheck error" | "self type-of error" -> "SELF_TC000"
      | "input error" -> "INPUT001"
      | "system error" -> "SYSTEM001"
      | "internal error" -> "INTERNAL001"
      | "audit error" -> "AUDIT001"
      | "error" -> "ERROR001"
      | _ -> "ERROR001")
