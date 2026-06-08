type term = Kernel.cterm

type branch = Kernel.cbranch

type def = Kernel.canonical_def

let version = Kernel.canonical_version

let graph_version = Kernel.canonical_graph_version

let serialize_def = Kernel.serialize_def

let serialize_program = Kernel.serialize_program

let serialize_graph = Kernel.checked_to_graph_json

let fail = Kernel.fail

let json_field name obj =
  match Json.field name obj with Some v -> v | None -> fail ("canonical graph missing field: " ^ name)

let json_string_field name obj =
  match Json.string (json_field name obj) with
  | Some s -> s
  | None -> fail ("canonical graph field must be string: " ^ name)

let json_array_field name obj =
  match Json.array (json_field name obj) with
  | Some xs -> xs
  | None -> fail ("canonical graph field must be array: " ^ name)

let json_bool_field name obj =
  match json_field name obj with
  | Json.Bool b -> b
  | _ -> fail ("canonical graph field must be bool: " ^ name)

let json_nat_field name obj =
  match json_field name obj with
  | Json.Num n when n >= 0 -> n
  | _ -> fail ("canonical graph field must be natural number: " ^ name)

let json_string_array_field name obj =
  json_array_field name obj
  |> List.map (function
       | Json.String s -> s
       | _ -> fail ("canonical graph field must be string array: " ^ name))

let validate_hash_metadata context obj =
  let algorithm = json_string_field "hashAlgorithm" obj in
  if not (String.equal algorithm Kernel.hash_algorithm) then
    fail (context ^ " hashAlgorithm mismatch: " ^ algorithm);
  let prefix = json_string_field "hashPrefix" obj in
  if not (String.equal prefix Kernel.hash_prefix) then
    fail (context ^ " hashPrefix mismatch: " ^ prefix)

let ensure_unique what names =
  let seen = Hashtbl.create 16 in
  List.iter
    (fun name ->
      if Hashtbl.mem seen name then fail ("duplicate " ^ what ^ " in canonical graph: " ^ name);
      Hashtbl.add seen name ())
    names

let rec type_of_graph_json obj =
  match json_string_field "tag" obj with
  | "Unit" -> Ast.TUnit
  | "Bool" -> Ast.TBool
  | "Nat" -> Ast.TNat
  | "String" -> Ast.TString
  | "Fun" -> Ast.TFun (type_of_graph_json (json_field "from" obj), type_of_graph_json (json_field "to" obj))
  | "Record" ->
      let fields =
        json_array_field "fields" obj
        |> List.map (fun field ->
               (json_string_field "name" field, type_of_graph_json (json_field "type" field)))
      in
      ensure_unique "record field" (List.map fst fields);
      Ast.TRecord (Ast.sort_fields fields)
  | "Variant" ->
      let cases =
        json_array_field "cases" obj
        |> List.map (fun case ->
               (json_string_field "name" case, type_of_graph_json (json_field "type" case)))
      in
      ensure_unique "variant constructor" (List.map fst cases);
      Ast.TVariant (Ast.sort_fields cases)
  | "List" -> Ast.TList (type_of_graph_json (json_field "item" obj))
  | "View" -> Ast.TView (type_of_graph_json (json_field "message" obj))
  | "Process" -> Ast.TProcess (type_of_graph_json (json_field "result" obj))
  | "TypeVar" -> Ast.TVar (json_nat_field "index" obj)
  | "Forall" ->
      Ast.TForall (json_nat_field "arity" obj, type_of_graph_json (json_field "body" obj))
  | "Named" ->
      Ast.TNamed
        ( json_string_field "name" obj,
          List.map type_of_graph_json (json_array_field "args" obj) )
  | tag -> fail ("unknown canonical graph type tag: " ^ tag)

let req_of_graph_json obj =
  let req =
    match json_string_field "tag" obj with
    | "AskHuman" -> Ast.AskHuman (json_string_field "prompt" obj)
    | "HttpGet" -> Ast.HttpGet (json_string_field "url" obj)
    | "ReadClock" -> Ast.ReadClock
    | "SaveLocal" -> Ast.SaveLocal (json_string_field "key" obj, json_string_field "value" obj)
    | "LoadLocal" -> Ast.LoadLocal (json_string_field "key" obj)
    | "ServerRequest" ->
        Ast.ServerRequest (json_string_field "route" obj, json_string_field "payload" obj)
    | tag -> fail ("unknown canonical graph request tag: " ^ tag)
  in
  let capability = json_string_field "capability" obj in
  if not (String.equal capability (Kernel.req_capability req)) then
    fail ("canonical graph request capability mismatch: " ^ capability);
  let capability_ref = json_string_field "capabilityRef" obj in
  let expected_capability_ref =
    match Kernel.req_capability_ref req with
    | Some ref -> ref
    | None -> fail ("canonical graph request unknown capability: " ^ Kernel.req_capability req)
  in
  if not (String.equal capability_ref expected_capability_ref) then
    fail ("canonical graph request capabilityRef mismatch: " ^ Kernel.req_tag req);
  let signature_ref = json_string_field "requestSignatureRef" obj in
  if not (String.equal signature_ref (Kernel.req_signature_ref req)) then
    fail ("canonical graph requestSignatureRef mismatch: " ^ Kernel.req_tag req);
  req

let rec term_of_graph_json obj =
  match json_string_field "tag" obj with
  | "Unit" -> Kernel.CUnit
  | "Bool" -> Kernel.CBool (json_bool_field "value" obj)
  | "Nat" -> Kernel.CNat (json_nat_field "value" obj)
  | "String" -> Kernel.CString (json_string_field "value" obj)
  | "Var" -> Kernel.CVar (json_nat_field "index" obj)
  | "Builtin" ->
      let name = json_string_field "name" obj in
      if not (Kernel.is_builtin name) then fail ("unknown canonical graph builtin: " ^ name);
      Kernel.CGlobal name
  | "Ref" -> Kernel.CGlobal (json_string_field "defId" obj)
  | "Lambda" ->
      Kernel.CLambda
        (type_of_graph_json (json_field "paramType" obj), term_of_graph_json (json_field "body" obj))
  | "App" ->
      Kernel.CApp (term_of_graph_json (json_field "fn" obj), term_of_graph_json (json_field "arg" obj))
  | "Let" ->
      Kernel.CLet
        (term_of_graph_json (json_field "value" obj), term_of_graph_json (json_field "body" obj))
  | "Record" ->
      let fields =
        json_array_field "fields" obj
        |> List.map (fun field ->
               (json_string_field "name" field, term_of_graph_json (json_field "value" field)))
      in
      ensure_unique "record field" (List.map fst fields);
      Kernel.CRecord (Ast.sort_fields fields)
  | "Field" ->
      Kernel.CField (term_of_graph_json (json_field "record" obj), json_string_field "field" obj)
  | "Variant" ->
      Kernel.CVariant
        ( type_of_graph_json (json_field "type" obj),
          json_string_field "constructor" obj,
          term_of_graph_json (json_field "payload" obj) )
  | "Inst" ->
      Kernel.CInst
        ( json_string_field "defId" obj,
          List.map type_of_graph_json (json_array_field "typeArgs" obj) )
  | "Case" ->
      Kernel.CCase
        ( term_of_graph_json (json_field "scrutinee" obj),
          List.map branch_of_graph_json (json_array_field "branches" obj) )
  | "FoldNat" ->
      Kernel.CFoldNat
        ( term_of_graph_json (json_field "index" obj),
          term_of_graph_json (json_field "zero" obj),
          term_of_graph_json (json_field "step" obj) )
  | "FoldVariant" ->
      Kernel.CFoldVariant
        ( type_of_graph_json (json_field "targetType" obj),
          type_of_graph_json (json_field "resultType" obj),
          term_of_graph_json (json_field "scrutinee" obj),
          List.map branch_of_graph_json (json_array_field "branches" obj) )
  | "Recur" -> Kernel.CRecur (term_of_graph_json (json_field "value" obj))
  | "Nil" -> Kernel.CNil (type_of_graph_json (json_field "type" obj))
  | "Cons" ->
      Kernel.CCons
        ( type_of_graph_json (json_field "type" obj),
          term_of_graph_json (json_field "head" obj),
          term_of_graph_json (json_field "tail" obj) )
  | "FoldList" ->
      Kernel.CFoldList
        ( term_of_graph_json (json_field "list" obj),
          term_of_graph_json (json_field "zero" obj),
          term_of_graph_json (json_field "step" obj) )
  | "CaseList" ->
      Kernel.CCaseList
        ( term_of_graph_json (json_field "list" obj),
          term_of_graph_json (json_field "nil" obj),
          term_of_graph_json (json_field "cons" obj) )
  | "Text" -> Kernel.CText (term_of_graph_json (json_field "value" obj))
  | "Image" ->
      Kernel.CImage (term_of_graph_json (json_field "src" obj), term_of_graph_json (json_field "alt" obj))
  | "Button" ->
      Kernel.CButton
        (term_of_graph_json (json_field "label" obj), term_of_graph_json (json_field "message" obj))
  | "Input" ->
      Kernel.CInput
        (term_of_graph_json (json_field "value" obj), term_of_graph_json (json_field "handler" obj))
  | "Column" -> Kernel.CColumn (term_of_graph_json (json_field "children" obj))
  | "Row" -> Kernel.CRow (term_of_graph_json (json_field "children" obj))
  | "ListView" ->
      Kernel.CListView
        (term_of_graph_json (json_field "items" obj), term_of_graph_json (json_field "render" obj))
  | "WhenView" ->
      Kernel.CWhenView
        (term_of_graph_json (json_field "condition" obj), term_of_graph_json (json_field "view" obj))
  | "Done" -> Kernel.CDone (term_of_graph_json (json_field "value" obj))
  | "Request" -> Kernel.CRequest (req_of_graph_json (json_field "request" obj))
  | "Bind" ->
      Kernel.CBind
        ( term_of_graph_json (json_field "process" obj),
          type_of_graph_json (json_field "valueType" obj),
          term_of_graph_json (json_field "body" obj) )
  | tag -> fail ("unknown canonical graph term tag: " ^ tag)

and branch_of_graph_json obj =
  match json_string_field "tag" obj with
  | "BoolBranch" -> Kernel.CBBool (json_bool_field "value" obj, term_of_graph_json (json_field "body" obj))
  | "VariantBranch" ->
      Kernel.CBVariant (json_string_field "constructor" obj, term_of_graph_json (json_field "body" obj))
  | tag -> fail ("unknown canonical graph branch tag: " ^ tag)

let def_of_graph_json obj =
  let name = json_string_field "name" obj in
  let def_id = json_string_field "defId" obj in
  let typ = type_of_graph_json (json_field "type" obj) in
  let body = term_of_graph_json (json_field "term" obj) in
  let type_ref = json_string_field "typeRef" obj in
  let term_ref = json_string_field "termRef" obj in
  if not (String.equal type_ref (Kernel.type_node_id typ)) then
    fail ("canonical graph typeRef mismatch: " ^ name);
  if not (String.equal term_ref (Kernel.term_node_id (fun x -> x) body)) then
    fail ("canonical graph termRef mismatch: " ^ name);
  let type_canonical = json_string_field "typeCanonical" obj in
  let term_canonical = json_string_field "termCanonical" obj in
  if not (String.equal type_canonical (Kernel.type_to_canonical typ)) then
    fail ("canonical graph typeCanonical mismatch: " ^ name);
  if not (String.equal term_canonical (Kernel.cterm_to_canonical_v2 (fun x -> x) body)) then
    fail ("canonical graph termCanonical mismatch: " ^ name);
  let canonical = Kernel.serialize_def name def_id typ body (fun x -> x) in
  let hash = json_string_field "hash" obj in
  if not (String.equal hash (Kernel.hash_string canonical)) then
    fail ("canonical graph def hash mismatch: " ^ name);
  { Kernel.cname = name; cdef_id = def_id; ctyp = typ; cbody = body }

let validate_definition_deps def_objs defs =
  let name_to_def_id = Hashtbl.create 32 in
  let def_ids = Hashtbl.create 32 in
  List.iter
    (fun (d : Kernel.canonical_def) ->
      Hashtbl.add name_to_def_id d.cname d.cdef_id;
      Hashtbl.replace def_ids d.cdef_id ())
    defs;
  List.iter2
    (fun obj (d : Kernel.canonical_def) ->
      let declared = json_string_array_field "deps" obj in
      let canonical_declared = List.sort_uniq String.compare declared in
      if declared <> canonical_declared then
        fail ("canonical graph deps not canonical: " ^ d.cname);
      let declared_refs =
        declared
        |> List.map (fun dep ->
               match Hashtbl.find_opt name_to_def_id dep with
               | Some def_id -> def_id
               | None -> fail ("canonical graph deps unknown definition in " ^ d.cname ^ ": " ^ dep))
        |> List.sort_uniq String.compare
      in
      let actual_refs =
        Kernel.cterm_global_refs d.cbody
        |> List.map (fun ref ->
               if Hashtbl.mem def_ids ref then ref
               else fail ("canonical graph deps reference missing definition in " ^ d.cname ^ ": " ^ ref))
        |> List.sort_uniq String.compare
      in
      if declared_refs <> actual_refs then fail ("canonical graph deps mismatch: " ^ d.cname))
    def_objs defs

let validate_capability_scopes caps def_objs defs =
  let declared_program_cap cap =
    List.exists (String.equal cap) caps
  in
  let defs_by_id = Hashtbl.create 32 in
  List.iter (fun (d : Kernel.canonical_def) -> Hashtbl.add defs_by_id d.cdef_id d) defs;
  let memo = Hashtbl.create 32 in
  let visiting = Hashtbl.create 32 in
  let rec capabilities_of_ref ref =
    match Hashtbl.find_opt defs_by_id ref with
    | Some d -> capabilities_of_def d
    | None -> fail ("canonical graph references missing definition for capability scope: " ^ ref)
  and capabilities_of_def (d : Kernel.canonical_def) =
    match Hashtbl.find_opt memo d.cdef_id with
    | Some caps -> caps
    | None ->
        if Hashtbl.mem visiting d.cdef_id then
          fail ("canonical graph cyclic capability dependency: " ^ d.cname);
        Hashtbl.add visiting d.cdef_id ();
        let direct = Kernel.cterm_direct_capabilities d.cbody in
        let inherited =
          Kernel.cterm_global_refs d.cbody |> List.concat_map capabilities_of_ref
        in
        let actual = List.sort_uniq String.compare (direct @ inherited) in
        List.iter
          (fun cap ->
            if not (declared_program_cap cap) then
              fail
                ("canonical graph capabilityScope uses undeclared capability in " ^ d.cname ^ ": "
               ^ cap))
          actual;
        Hashtbl.remove visiting d.cdef_id;
        Hashtbl.add memo d.cdef_id actual;
        actual
  in
  List.iter2
    (fun obj d ->
      let declared = json_string_array_field "capabilityScope" obj in
      let canonical_declared = List.sort_uniq String.compare declared in
      if declared <> canonical_declared then
        fail ("canonical graph capabilityScope not canonical: " ^ d.Kernel.cname);
      let declared_refs = json_string_array_field "capabilityScopeRefs" obj in
      let expected_refs =
        declared
        |> List.map (fun cap ->
               match Kernel.capability_ref cap with
               | Some ref -> ref
               | None -> fail ("canonical graph capabilityScope unknown capability: " ^ cap))
      in
      if declared_refs <> expected_refs then
        fail ("canonical graph capabilityScopeRefs mismatch: " ^ d.Kernel.cname);
      let actual = capabilities_of_def d in
      if declared <> actual then
        fail
          ("canonical graph capabilityScope mismatch: " ^ d.Kernel.cname ^ " declared ["
         ^ String.concat "," declared ^ "], actual [" ^ String.concat "," actual ^ "]"))
    def_objs defs

let validate_node_payload id kind canonical payload =
  match kind with
  | "Type" ->
      let typ = Kernel.type_of_canonical_sexp (Kernel.single_sexp canonical) in
      let expected = Json.parse (Kernel.type_to_graph_json typ) in
      if payload <> expected then fail ("canonical node payload mismatch: " ^ id);
      Kernel.type_node_edges typ
  | "Term" ->
      let term = Kernel.cterm_of_canonical_sexp (Kernel.single_sexp canonical) in
      let expected = Json.parse (Kernel.cterm_to_graph_json (fun x -> x) term) in
      if payload <> expected then fail ("canonical node payload mismatch: " ^ id);
      Kernel.cterm_node_edges (fun x -> x) term
  | _ -> fail ("unknown canonical node kind: " ^ kind)

let validate_node_graph graph program_hash defs =
  let node_graph = json_field "nodeGraph" graph in
  let version = json_string_field "version" node_graph in
  if not (String.equal version Kernel.canonical_node_graph_version) then
    fail ("canonical node graph version mismatch: " ^ version);
  validate_hash_metadata "canonical node graph" node_graph;
  let root_hash = json_string_field "rootProgramHash" node_graph in
  if not (String.equal root_hash program_hash) then
    fail ("canonical node graph rootProgramHash mismatch: " ^ root_hash);
  let node_defs = json_array_field "defs" node_graph in
  let nodes = json_array_field "nodes" node_graph in
  let node_ids = List.map (json_string_field "id") nodes in
  ensure_unique "node" node_ids;
  let node_by_id id =
    List.find_opt (fun node -> String.equal (json_string_field "id" node) id) nodes
  in
  List.iter
    (fun node ->
      let id = json_string_field "id" node in
      let kind = json_string_field "kind" node in
      let canonical = json_string_field "canonical" node in
      let expected_id = Kernel.canonical_node_id kind canonical in
      if not (String.equal id expected_id) then fail ("canonical node id mismatch: " ^ id);
      let expected_edges =
        validate_node_payload id kind canonical (json_field "payload" node)
        |> List.sort_uniq String.compare
      in
      let edge_refs = json_string_array_field "edgeRefs" node |> List.sort_uniq String.compare in
      if edge_refs <> expected_edges then
        fail ("canonical node edgeRefs mismatch: " ^ id);
      List.iter
        (fun edge ->
          if node_by_id edge = None then fail ("canonical node missing edge target: " ^ edge))
        edge_refs)
    nodes;
  ensure_unique "node def" (List.map (json_string_field "name") node_defs);
  if List.length node_defs <> List.length defs then
    fail "canonical node graph def count mismatch";
  List.iter
    (fun (d : Kernel.canonical_def) ->
      let node_def =
        match
          List.find_opt (fun obj -> String.equal (json_string_field "name" obj) d.cname)
            node_defs
        with
        | Some obj -> obj
        | None -> fail ("canonical node graph missing def ref: " ^ d.cname)
      in
      if not (String.equal (json_string_field "defId" node_def) d.cdef_id) then
        fail ("canonical node graph defId mismatch: " ^ d.cname);
      let type_ref = json_string_field "typeRef" node_def in
      let term_ref = json_string_field "termRef" node_def in
      if not (String.equal type_ref (Kernel.type_node_id d.ctyp)) then
        fail ("canonical node graph typeRef mismatch: " ^ d.cname);
      if not (String.equal term_ref (Kernel.term_node_id (fun x -> x) d.cbody)) then
        fail ("canonical node graph termRef mismatch: " ^ d.cname);
      if node_by_id type_ref = None then
        fail ("canonical node graph missing type node: " ^ type_ref);
      if node_by_id term_ref = None then
        fail ("canonical node graph missing term node: " ^ term_ref))
    defs;
  let reachable = Hashtbl.create 32 in
  let rec mark id =
    if not (Hashtbl.mem reachable id) then (
      Hashtbl.add reachable id ();
      match node_by_id id with
      | None -> fail ("canonical node graph missing edge target: " ^ id)
      | Some node -> List.iter mark (json_string_array_field "edgeRefs" node))
  in
  node_defs
  |> List.iter (fun node_def ->
         mark (json_string_field "typeRef" node_def);
         mark (json_string_field "termRef" node_def));
  List.iter
    (fun node ->
      let id = json_string_field "id" node in
      if not (Hashtbl.mem reachable id) then
        fail ("canonical node graph unreachable node: " ^ id))
    nodes

let validate_exact_serialization graph input caps defs =
  let checked = Kernel.checked_of_canonical caps defs in
  let graph_hash = json_string_field "graphHash" graph in
  let expected_graph_hash = Kernel.checked_to_graph_content_hash checked in
  if not (String.equal graph_hash expected_graph_hash) then
    fail ("canonical graph graphHash mismatch: " ^ graph_hash);
  let expected = String.trim (Kernel.checked_to_graph_json checked) in
  if not (String.equal (String.trim input) expected) then
    fail "canonical graph serialization mismatch"

let parse_graph input =
  let graph = try Json.parse input with Json.Error msg -> fail ("invalid canonical graph JSON: " ^ msg) in
  let version = json_string_field "version" graph in
  if not (String.equal version Kernel.canonical_graph_version) then
    fail ("canonical graph version mismatch: " ^ version);
  let canonical_version = json_string_field "canonicalVersion" graph in
  if not (String.equal canonical_version Kernel.canonical_version) then
    fail ("canonical graph canonicalVersion mismatch: " ^ canonical_version);
  validate_hash_metadata "canonical graph" graph;
  let declared_caps =
    json_array_field "capabilities" graph
    |> List.map (function Json.String s -> s | _ -> fail "canonical graph capability must be string")
  in
  let caps = List.sort_uniq String.compare declared_caps in
  if declared_caps <> caps then fail "canonical graph capabilities not canonical";
  Kernel.validate_capabilities caps;
  let declared_capability_refs = json_string_array_field "capabilityRefs" graph in
  let expected_capability_refs =
    caps
    |> List.map (fun cap ->
           match Kernel.capability_ref cap with
           | Some ref -> ref
           | None -> fail ("canonical graph capabilityRefs unknown capability: " ^ cap))
  in
  if declared_capability_refs <> expected_capability_refs then
    fail "canonical graph capabilityRefs mismatch";
  let expected_descriptors = Json.parse (Kernel.capabilities_to_graph_json caps) in
  let descriptors = json_field "capabilityDescriptors" graph in
  if descriptors <> expected_descriptors then fail "canonical graph capabilityDescriptors mismatch";
  let def_objs = json_array_field "defs" graph in
  let defs = List.map def_of_graph_json def_objs in
  ensure_unique "definition" (List.map (fun d -> d.Kernel.cname) defs);
  validate_definition_deps def_objs defs;
  validate_capability_scopes caps def_objs defs;
  let canonical = Kernel.serialize_program caps defs in
  let program_hash = json_string_field "programHash" graph in
  if not (String.equal program_hash (Kernel.hash_string canonical)) then
    fail ("canonical graph programHash mismatch: " ^ program_hash);
  validate_node_graph graph program_hash defs;
  validate_exact_serialization graph input caps defs;
  (caps, defs)

let graph_to_program input =
  let caps, defs = parse_graph input in
  Kernel.serialize_program caps defs

let checked_of_graph input =
  let caps, defs = parse_graph input in
  Kernel.checked_of_canonical caps defs

type graph_stats = {
  version : string;
  canonical_version : string;
  node_graph_version : string;
  program_hash : string;
  graph_hash : string;
  defs : int;
  capabilities : int;
  capability_descriptors : int;
  nodes : int;
  type_nodes : int;
  term_nodes : int;
  edges : int;
}

let graph_stats input =
  ignore (checked_of_graph input);
  let graph = Json.parse input in
  let node_graph = json_field "nodeGraph" graph in
  let nodes = json_array_field "nodes" node_graph in
  let type_nodes, term_nodes =
    nodes
    |> List.fold_left
         (fun (types, terms) node ->
           match json_string_field "kind" node with
           | "Type" -> (types + 1, terms)
           | "Term" -> (types, terms + 1)
           | kind -> fail ("unknown canonical node kind: " ^ kind))
         (0, 0)
  in
  {
    version = json_string_field "version" graph;
    canonical_version = json_string_field "canonicalVersion" graph;
    node_graph_version = json_string_field "version" node_graph;
    program_hash = json_string_field "programHash" graph;
    graph_hash = json_string_field "graphHash" graph;
    defs = List.length (json_array_field "defs" graph);
    capabilities = List.length (json_array_field "capabilities" graph);
    capability_descriptors = List.length (json_array_field "capabilityDescriptors" graph);
    nodes = List.length nodes;
    type_nodes;
    term_nodes;
    edges =
      nodes
      |> List.fold_left
           (fun count node -> count + List.length (json_string_array_field "edgeRefs" node))
           0;
  }

let describe_graph_stats stats =
  "Graph stats\nversion=" ^ stats.version ^ "\ncanonical_version="
  ^ stats.canonical_version ^ "\nnode_graph_version=" ^ stats.node_graph_version
  ^ "\nprogram_hash=" ^ stats.program_hash ^ "\ngraph_hash=" ^ stats.graph_hash
  ^ "\ndefs=" ^ string_of_int stats.defs ^ "\ncapabilities="
  ^ string_of_int stats.capabilities ^ "\ncapability_descriptors="
  ^ string_of_int stats.capability_descriptors ^ "\nnodes="
  ^ string_of_int stats.nodes ^ "\ntype_nodes=" ^ string_of_int stats.type_nodes
  ^ "\nterm_nodes=" ^ string_of_int stats.term_nodes ^ "\nedges="
  ^ string_of_int stats.edges ^ "\n"

type graph_node = {
  node_id : string;
  node_kind : string;
  node_canonical : string;
  node_payload_tag : string;
  node_edge_refs : string list;
}

let graph_node input node_ref =
  ignore (checked_of_graph input);
  let graph = Json.parse input in
  let nodes = json_array_field "nodes" (json_field "nodeGraph" graph) in
  match List.find_opt (fun node -> String.equal (json_string_field "id" node) node_ref) nodes with
  | None -> fail ("canonical graph node not found: " ^ node_ref)
  | Some node ->
      let payload = json_field "payload" node in
      let payload_tag =
        match Json.string (json_field "tag" payload) with Some tag -> tag | None -> "-"
      in
      {
        node_id = json_string_field "id" node;
        node_kind = json_string_field "kind" node;
        node_canonical = json_string_field "canonical" node;
        node_payload_tag = payload_tag;
        node_edge_refs = json_string_array_field "edgeRefs" node;
      }

let describe_graph_node node =
  "Graph node\nid=" ^ node.node_id ^ "\nkind=" ^ node.node_kind
  ^ "\npayload_tag=" ^ node.node_payload_tag ^ "\ncanonical="
  ^ node.node_canonical ^ "\nedges=" ^ string_of_int (List.length node.node_edge_refs)
  ^ "\nedge_refs="
  ^ (match node.node_edge_refs with [] -> "" | refs -> String.concat "," refs)
  ^ "\n"

type graph_definition = {
  graph_def_name : string;
  graph_def_id : string;
  graph_def_hash : string;
  graph_def_type_ref : string;
  graph_def_term_ref : string;
  graph_def_type_canonical : string;
  graph_def_term_canonical : string;
  graph_def_deps : string list;
  graph_def_capability_scope : string list;
  graph_def_capability_scope_refs : string list;
}

let graph_definition_of_json def =
  {
    graph_def_name = json_string_field "name" def;
    graph_def_id = json_string_field "defId" def;
    graph_def_hash = json_string_field "hash" def;
    graph_def_type_ref = json_string_field "typeRef" def;
    graph_def_term_ref = json_string_field "termRef" def;
    graph_def_type_canonical = json_string_field "typeCanonical" def;
    graph_def_term_canonical = json_string_field "termCanonical" def;
    graph_def_deps = json_string_array_field "deps" def;
    graph_def_capability_scope = json_string_array_field "capabilityScope" def;
    graph_def_capability_scope_refs = json_string_array_field "capabilityScopeRefs" def;
  }

let graph_definitions input =
  ignore (checked_of_graph input);
  let graph = Json.parse input in
  json_array_field "defs" graph |> List.map graph_definition_of_json

let graph_definition_in defs id =
  match
    List.find_opt
      (fun def ->
        String.equal id def.graph_def_name
        || String.equal id def.graph_def_id
        || String.equal id def.graph_def_hash)
      defs
  with
  | None -> fail ("canonical graph definition not found: " ^ id)
  | Some def -> def

let graph_definition input id = graph_definition_in (graph_definitions input) id

let describe_graph_definition def =
  "Graph def\nname=" ^ def.graph_def_name ^ "\ndef_id=" ^ def.graph_def_id
  ^ "\nhash=" ^ def.graph_def_hash ^ "\ntype_ref=" ^ def.graph_def_type_ref
  ^ "\nterm_ref=" ^ def.graph_def_term_ref ^ "\ndeps="
  ^ string_of_int (List.length def.graph_def_deps) ^ "\ndep_names="
  ^ (match def.graph_def_deps with [] -> "" | deps -> String.concat "," deps)
  ^ "\ncapabilities=" ^ string_of_int (List.length def.graph_def_capability_scope)
  ^ "\ncapability_scope="
  ^ (match def.graph_def_capability_scope with [] -> "" | caps -> String.concat "," caps)
  ^ "\ncapability_scope_refs="
  ^ (match def.graph_def_capability_scope_refs with [] -> "" | refs -> String.concat "," refs)
  ^ "\ntype=" ^ def.graph_def_type_canonical ^ "\nterm="
  ^ def.graph_def_term_canonical ^ "\n"

let describe_graph_definitions defs =
  let line def =
    "name=" ^ def.graph_def_name ^ " def_id=" ^ def.graph_def_id ^ " hash="
    ^ def.graph_def_hash ^ " type_ref=" ^ def.graph_def_type_ref ^ " term_ref="
    ^ def.graph_def_term_ref ^ " deps="
    ^ string_of_int (List.length def.graph_def_deps) ^ " capabilities="
    ^ string_of_int (List.length def.graph_def_capability_scope)
  in
  "Graph roots\ncount=" ^ string_of_int (List.length defs) ^ "\n"
  ^ String.concat "\n" (List.map line defs)
  ^ (if defs = [] then "" else "\n")

type graph_dependency = {
  graph_dep_def_name : string;
  graph_dep_def_id : string;
  graph_dep_def_hash : string;
  graph_dep_name : string;
  graph_dep_id : string;
  graph_dep_hash : string;
}

let graph_definition_by_name defs name =
  match List.find_opt (fun def -> String.equal name def.graph_def_name) defs with
  | Some def -> def
  | None -> fail ("canonical graph dependency target not found: " ^ name)

let graph_dependencies_for_def defs def =
  def.graph_def_deps
  |> List.map (fun dep_name ->
         let dep = graph_definition_by_name defs dep_name in
         {
           graph_dep_def_name = def.graph_def_name;
           graph_dep_def_id = def.graph_def_id;
           graph_dep_def_hash = def.graph_def_hash;
           graph_dep_name = dep.graph_def_name;
           graph_dep_id = dep.graph_def_id;
           graph_dep_hash = dep.graph_def_hash;
         })

let sort_graph_dependencies deps =
  List.sort
    (fun a b ->
      match String.compare a.graph_dep_def_name b.graph_dep_def_name with
      | 0 -> String.compare a.graph_dep_name b.graph_dep_name
      | n -> n)
    deps

let graph_dependencies input =
  let defs = graph_definitions input in
  defs |> List.concat_map (graph_dependencies_for_def defs) |> sort_graph_dependencies

let graph_dependencies_for input id =
  let defs = graph_definitions input in
  graph_definition_in defs id |> graph_dependencies_for_def defs |> sort_graph_dependencies

let describe_graph_dependencies deps =
  let line dep =
    "def=" ^ dep.graph_dep_def_name ^ " def_id=" ^ dep.graph_dep_def_id ^ " def_hash="
    ^ dep.graph_dep_def_hash ^ " depends_on=" ^ dep.graph_dep_name ^ " dep_id="
    ^ dep.graph_dep_id ^ " dep_hash=" ^ dep.graph_dep_hash
  in
  "Graph deps\ncount=" ^ string_of_int (List.length deps) ^ "\n"
  ^ String.concat "\n" (List.map line deps)
  ^ (if deps = [] then "" else "\n")

type graph_capability_request = {
  graph_cap_req_ref : string;
  graph_cap_req_tag : string;
  graph_cap_req_payload_type : Ast.typ;
  graph_cap_req_response_type : Ast.typ;
}

type graph_capability = {
  graph_cap_name : string;
  graph_cap_ref : string;
  graph_cap_requests : graph_capability_request list;
}

let graph_capability_request_of_json req =
  {
    graph_cap_req_ref = json_string_field "ref" req;
    graph_cap_req_tag = json_string_field "tag" req;
    graph_cap_req_payload_type = type_of_graph_json (json_field "payloadType" req);
    graph_cap_req_response_type = type_of_graph_json (json_field "responseType" req);
  }

let graph_capability_of_json desc =
  {
    graph_cap_name = json_string_field "name" desc;
    graph_cap_ref = json_string_field "ref" desc;
    graph_cap_requests =
      json_array_field "requests" desc |> List.map graph_capability_request_of_json;
  }

let graph_capabilities input =
  ignore (checked_of_graph input);
  let graph = Json.parse input in
  json_array_field "capabilityDescriptors" graph |> List.map graph_capability_of_json

let graph_capability input id =
  match
    List.find_opt
      (fun cap -> String.equal id cap.graph_cap_name || String.equal id cap.graph_cap_ref)
      (graph_capabilities input)
  with
  | Some cap -> cap
  | None -> fail ("canonical graph capability not found: " ^ id)

let describe_graph_capabilities caps =
  let request_line cap req =
    "capability=" ^ cap.graph_cap_name ^ " capability_ref=" ^ cap.graph_cap_ref
    ^ " request=" ^ req.graph_cap_req_tag ^ " request_ref=" ^ req.graph_cap_req_ref
    ^ " payload_type=" ^ Kernel.type_to_canonical req.graph_cap_req_payload_type
    ^ " response_type=" ^ Kernel.type_to_canonical req.graph_cap_req_response_type
  in
  let request_count =
    List.fold_left (fun count cap -> count + List.length cap.graph_cap_requests) 0 caps
  in
  let lines =
    caps
    |> List.concat_map (fun cap -> List.map (request_line cap) cap.graph_cap_requests)
  in
  "Graph capabilities\ncount=" ^ string_of_int (List.length caps) ^ "\nrequests="
  ^ string_of_int request_count ^ "\n"
  ^ String.concat "\n" lines
  ^ (if lines = [] then "" else "\n")

type graph_capability_scope = {
  graph_scope_def_name : string;
  graph_scope_def_id : string;
  graph_scope_def_hash : string;
  graph_scope_capability : string;
  graph_scope_capability_ref : string;
}

let graph_capability_scopes_for_def def =
  let rec zip caps refs =
    match (caps, refs) with
    | [], [] -> []
    | cap :: caps, ref :: refs ->
        {
          graph_scope_def_name = def.graph_def_name;
          graph_scope_def_id = def.graph_def_id;
          graph_scope_def_hash = def.graph_def_hash;
          graph_scope_capability = cap;
          graph_scope_capability_ref = ref;
        }
        :: zip caps refs
    | _ -> fail ("canonical graph capability scope/ref length mismatch: " ^ def.graph_def_name)
  in
  zip def.graph_def_capability_scope def.graph_def_capability_scope_refs

let sort_graph_capability_scopes scopes =
  List.sort
    (fun a b ->
      match String.compare a.graph_scope_capability b.graph_scope_capability with
      | 0 -> String.compare a.graph_scope_def_name b.graph_scope_def_name
      | n -> n)
    scopes

let graph_capability_scopes input =
  graph_definitions input
  |> List.concat_map graph_capability_scopes_for_def
  |> sort_graph_capability_scopes

let graph_capability_scopes_for input id =
  graph_capability_scopes input
  |> List.filter (fun scope ->
         String.equal id scope.graph_scope_capability
         || String.equal id scope.graph_scope_capability_ref)

let describe_graph_capability_scopes scopes =
  let line scope =
    "def=" ^ scope.graph_scope_def_name ^ " def_id=" ^ scope.graph_scope_def_id
    ^ " def_hash=" ^ scope.graph_scope_def_hash ^ " capability="
    ^ scope.graph_scope_capability ^ " capability_ref=" ^ scope.graph_scope_capability_ref
  in
  "Graph capability scopes\ncount=" ^ string_of_int (List.length scopes) ^ "\n"
  ^ String.concat "\n" (List.map line scopes)
  ^ (if scopes = [] then "" else "\n")

let parse_def = Kernel.parse_serialized_def

let parse_program = Kernel.parse_serialized_program

let term_to_string = Kernel.cterm_to_string
