(* Frozen inferred interface of kernel.ml.

   This .mli exists for incremental-build speed, not encapsulation: with it,
   body-only edits to kernel.ml (including adding private top-level helpers)
   leave the .cmi unchanged, so dune's early cutoff stops the recompile
   cascade through runtime/workspace/web/the shims/bin/test. Without it, any
   new top-level binding changes the inferred interface and recompiles every
   dependent.

   When you intentionally add or change public kernel API, extend this file
   accordingly (the compiler error tells you the expected signature). To
   regenerate from scratch: take the ocamlc command dune uses for kernel.cmo
   (dune build --verbose), replace `-c -o ...` with `-i`, and tidy the
   Hashtbl functor signatures back to `Hashtbl.S with type key = ...`. *)

exception Error of string
val fail : string -> 'a
val hash_algorithm : string
val hash_prefix : string
val hash_string : string -> string
val builtin_types : (string * Ast.typ) list
val builtin_names : string list
val is_builtin : string -> bool
val assoc_opt : string -> (string * 'a) list -> 'a option
val require : bool -> string -> unit
val option_or_fail : string -> 'a option -> 'a
val type_to_canonical : Ast.typ -> string
val req_capability : Ast.req -> string
val req_tag : Ast.req -> string
val req_payload_type : Ast.req -> Ast.typ
val req_result_type : Ast.req -> Ast.typ
type capability_request_signature = {
  request_tag : string;
  request_payload_type : Ast.typ;
  response_type : Ast.typ;
}
type capability_descriptor = {
  capability_name : string;
  request_signatures : capability_request_signature list;
}
val capability_catalog : capability_descriptor list
val capability_descriptor : string -> capability_descriptor option
val capability_request_signature_canonical :
  string -> capability_request_signature -> string
val capability_request_signature_ref :
  string -> capability_request_signature -> string
val capability_descriptor_canonical : capability_descriptor -> string
val capability_descriptor_ref : capability_descriptor -> string
val capability_ref : string -> string option
val capability_scope_canonical : string list -> string
val capability_scope_ref : string list -> string
val req_signature_ref : Ast.req -> string
val req_capability_ref : Ast.req -> string option
val known_capabilities : unit -> string list
val validate_capabilities : string list -> unit
val req_to_canonical : Ast.req -> string
val builtin_type_names : string list
val check_duplicate_names : Ast.def list -> unit
val check_duplicate_type_aliases : Ast.type_alias list -> unit
val alias_by_name : Ast.type_alias list -> string -> Ast.type_alias option
val bind_type_params : Ast.type_alias -> 'a list -> (string * 'a) list
val direct_self_refs : Ast.type_alias -> bool -> Ast.typ -> bool list
val validate_type_alias_recursion : Ast.type_alias list -> unit
val recursive_alias_names : Ast.type_alias list -> string list
val expand_type :
  string list ->
  Ast.type_alias list ->
  (string * Ast.typ) list -> string list -> Ast.typ -> Ast.typ
val type_var_env : 'a list -> ('a * Ast.typ) list
val expand_expr_types :
  string list ->
  Ast.type_alias list -> (string * Ast.typ) list -> Ast.expr -> Ast.expr
val expand_branch_types :
  string list ->
  Ast.type_alias list -> (string * Ast.typ) list -> Ast.branch -> Ast.branch
val resolve_program_types : Ast.program -> Ast.program
val collect_deps : Ast.def list -> (string * string list) list
val deps_table_memo :
  (Ast.def list * (string, string list) Hashtbl.t) list ref
val deps_table : Ast.def list -> (string, string list) Hashtbl.t
val dependencies_of_defs : Ast.def list -> string -> string list
val reject_cycles : Ast.def list -> unit
type global_type = {
  global_type_params : string list;
  global_typ : Ast.typ;
}
type fold_scope = {
  fold_target : Ast.typ;
  fold_result : Ast.typ;
  fold_allowed : Ast.expr list;
  fold_list_allowed : Ast.expr list;
}
type type_ctx = {
  type_aliases : Ast.type_alias list;
  globals : (string * global_type) list;
  capabilities : string list;
  locals : (string * Ast.typ) list;
  fold_scope : fold_scope option;
}
val recur_root_name : Ast.expr -> string option
val shadow_fold_scope : fold_scope option -> string -> fold_scope option
val bind_local : type_ctx -> string -> Ast.typ -> type_ctx
val bind_lambda : type_ctx -> string -> Ast.typ -> type_ctx
val expr_names : Ast.expr -> string list
val branch_names_in_expr : Ast.branch -> string list
val fresh_record_name :
  type_ctx -> Ast.expr -> ('a * string) list -> Ast.expr -> string
val desugar_let_record :
  type_ctx -> Ast.expr -> (string * string) list -> Ast.expr -> Ast.expr
val subst_type : Ast.typ list -> Ast.typ -> Ast.typ
val instantiate_type :
  string -> 'a list -> Ast.typ -> Ast.typ list -> Ast.typ
val type_body : Ast.typ -> Ast.typ
val lookup_global : type_ctx -> string -> global_type option
val subst_named_type_params : (string * Ast.typ) list -> Ast.typ -> Ast.typ
val unfold_type : type_ctx -> Ast.typ -> Ast.typ
val expr_equal : Ast.expr -> Ast.expr -> bool
val direct_recur_terms_for_value :
  type_ctx -> Ast.typ -> Ast.expr -> Ast.typ -> Ast.expr list
val direct_recur_terms :
  type_ctx -> Ast.typ -> string -> Ast.typ -> Ast.expr list
val has_direct_recur_terms : type_ctx -> Ast.typ -> Ast.typ -> bool
val direct_recur_list_terms :
  type_ctx -> Ast.typ -> string -> Ast.typ -> Ast.expr list
val fresh_wildcard_payload_name : Ast.expr -> string
val expand_variant_case_branches :
  (string * 'a) list -> Ast.branch list -> string -> Ast.branch list
val bool_branch_name : bool -> string
val bool_case_branches : Ast.branch list -> Ast.expr * Ast.expr
val recur_scope : type_ctx -> fold_scope
val require_unit_branch_payload : string -> Ast.typ -> unit
val lookup_type : type_ctx -> string -> Ast.typ option
val edit_distance : string -> string -> int
val suggestion : type_ctx -> string -> string
val require_type : Ast.typ -> Ast.typ -> string -> unit
val require_type_expr : Ast.typ -> Ast.typ -> string -> Ast.expr -> unit
val has_capability : type_ctx -> string -> bool
val contains_process_type : Ast.typ -> bool
val is_process_type : Ast.typ -> bool
val fold_branch_ctx :
  type_ctx -> Ast.typ -> Ast.typ -> string -> Ast.typ -> type_ctx
val structural_list_recur_allowed : type_ctx -> Ast.expr -> Ast.typ -> bool
val bind_recur_item : type_ctx -> string -> Ast.typ -> type_ctx
val infer : type_ctx -> Ast.expr -> Ast.typ
val infer_fold_list_step :
  type_ctx -> Ast.expr -> Ast.typ -> Ast.typ -> Ast.expr -> unit
val infer_bool_case : type_ctx -> Ast.branch list -> Ast.typ
val infer_variant_case :
  type_ctx -> (string * Ast.typ) list -> Ast.branch list -> Ast.typ
val app_spine : Ast.expr -> Ast.expr * Ast.expr list
val infer_elab : type_ctx -> Ast.expr -> Ast.typ * Ast.expr
val check_elab : type_ctx -> Ast.typ -> Ast.expr -> Ast.typ * Ast.expr
val check_fold_list_step_elab :
  type_ctx ->
  Ast.expr -> Ast.typ -> Ast.typ -> Ast.expr -> Ast.typ * Ast.expr
val poly_app_elab :
  type_ctx -> Ast.typ option -> Ast.expr -> (Ast.typ * Ast.expr) option
val elaborate_variant_payload :
  type_ctx -> Ast.typ -> string -> Ast.expr -> Ast.typ * Ast.expr
val infer_bool_case_elab :
  type_ctx -> Ast.branch list -> Ast.typ * Ast.branch list
val check_bool_case_elab :
  type_ctx -> Ast.typ -> Ast.branch list -> Ast.branch list
val infer_variant_case_elab :
  type_ctx ->
  (string * Ast.typ) list -> Ast.branch list -> Ast.typ * Ast.branch list
val check_variant_case_elab :
  type_ctx ->
  (string * Ast.typ) list -> Ast.typ -> Ast.branch list -> Ast.branch list
type cterm =
    CUnit
  | CBool of bool
  | CNat of int
  | CString of string
  | CVar of int
  | CGlobal of string
  | CLambda of Ast.typ * cterm
  | CApp of cterm * cterm
  | CLet of cterm * cterm
  | CRecord of (string * cterm) list
  | CField of cterm * string
  | CVariant of Ast.typ * string * cterm
  | CInst of string * Ast.typ list
  | CCase of cterm * cbranch list
  | CFoldNat of cterm * cterm * cterm
  | CFoldVariant of Ast.typ * Ast.typ * cterm * cbranch list
  | CRecur of cterm
  | CNil of Ast.typ
  | CCons of Ast.typ * cterm * cterm
  | CFoldList of cterm * cterm * cterm
  | CCaseList of cterm * cterm * cterm
  | CText of cterm
  | CImage of cterm * cterm
  | CButton of cterm * cterm
  | CInput of cterm * cterm
  | CColumn of cterm
  | CRow of cterm
  | CListView of cterm * cterm
  | CWhenView of cterm * cterm
  | CNode of cterm * cterm * cterm
  | CAttr of cterm * cterm
  | COn of cterm * cterm
  | CDone of cterm
  | CRequest of Ast.req
  | CBind of cterm * Ast.typ * cterm
and cbranch = CBBool of bool * cterm | CBVariant of string * cterm
val index_of : string -> string list -> int -> int option
val canonical_expr : string list -> Ast.expr -> cterm
val canonical_branches : string list -> Ast.branch list -> cbranch list
val cterm_direct_capabilities : cterm -> string list
val cbranch_direct_capabilities : cbranch -> string list
val cterm_global_refs : cterm -> string list
val cbranch_global_refs : cbranch -> string list
val cterm_to_string : cterm -> string
val cbranch_to_string : cbranch -> string
val canonical_version : string
type canonical_def = {
  cname : string;
  cdef_id : string;
  ctyp : Ast.typ;
  cbody : cterm;
}
val cterm_to_canonical_v2 : (string -> string) -> cterm -> string
val cbranch_to_canonical_v2 : (string -> string) -> cbranch -> string
val serialize_def_payload :
  string -> string -> Ast.typ -> cterm -> (string -> string) -> string
val serialize_def :
  string -> string -> Ast.typ -> cterm -> (string -> string) -> string
val serialize_program_payload : string list -> canonical_def list -> string
val serialize_program : string list -> canonical_def list -> string
val canonical_graph_legacy_v1 : string
val canonical_graph_version : string
val canonical_node_graph_version : string
val json_string : string -> string
val json_field : string -> string -> string
val json_obj : string list -> string
val json_array : ('a -> string) -> 'a list -> string
val json_bool : bool -> string
val canonical_node_id : string -> string -> string
val type_node_id : Ast.typ -> string
val term_node_id : (string -> string) -> cterm -> string
val uniq_strings : string list -> string list
val type_to_graph_json : Ast.typ -> string
val req_to_graph_json : Ast.req -> string
val capability_request_to_graph_json :
  string -> capability_request_signature -> string
val capability_descriptor_to_graph_json : capability_descriptor -> string
val declared_capability_descriptors :
  string list -> capability_descriptor list
val capabilities_to_graph_json : string list -> string
val cterm_to_graph_json_via :
  (cterm -> string) -> (string -> string) -> cterm -> string
val cbranch_to_graph_json_via :
  (cterm -> string) -> (string -> string) -> cbranch -> string
val cterm_to_graph_json : (string -> string) -> cterm -> string
val type_node_tag : Ast.typ -> string
val cterm_node_tag : cterm -> string
val type_node_edges_via : (Ast.typ -> 'a) -> Ast.typ -> 'a list
val type_node_edges : Ast.typ -> string list
val cbranch_body_edges_via : (cterm -> 'a) -> cbranch -> 'a list
val cterm_node_edges_via :
  term_id:(cterm -> 'a) -> type_id:(Ast.typ -> 'a) -> cterm -> 'a list
val cbranch_body_edges : (string -> string) -> cbranch -> string list
val cterm_node_edges : (string -> string) -> cterm -> string list
val canonical_node_json :
  string -> string -> string -> string -> string -> string list -> string
module Phys_cterm_tbl : Hashtbl.S with type key = cterm
module Phys_typ_tbl : Hashtbl.S with type key = Ast.typ
val canonical_node_graph_json :
  string -> (string -> string) -> canonical_def list -> string
val single_sexp : string -> Sexp.t
val atom : Sexp.t -> string
val strip_prefix : string -> string -> string option
val parse_nat_atom : string -> int option
val type_of_canonical_sexp : Sexp.t -> Ast.typ
val req_of_canonical_sexp : Sexp.t -> Ast.req
val cterm_of_canonical_sexp : Sexp.t -> cterm
val cbranch_of_canonical_sexp : Sexp.t -> cbranch
val def_of_payload : Sexp.t -> canonical_def
val parse_serialized_def : string -> canonical_def
val parse_serialized_program : string -> string list * canonical_def list
type checked_def = {
  def : Ast.def;
  def_id : string;
  cterm : cterm;
  canonical : string;
  hash : string;
  capabilities : string list;
}
type checked = { program : Ast.program; defs : checked_def list; }
val secret_leak_risks : checked -> string list
val ensure_unique_canonical_defs : canonical_def list -> unit
val canonical_def_by_ref :
  canonical_def list -> string -> canonical_def option
val canonical_def_id_of : canonical_def list -> string -> string
val canonical_def_name_of : canonical_def list -> string -> string
val canonical_type_params : Ast.typ -> string list
val canonical_surface_expr : canonical_def list -> cterm -> Ast.expr
val validate_canonical_refs : canonical_def list -> unit
val validate_canonical_def_ids : canonical_def list -> unit
val canonical_capabilities_of_defs :
  string list -> canonical_def list -> canonical_def -> string list
val checked_of_canonical : string list -> canonical_def list -> checked
val check_program : Ast.program -> checked
val canonical_defs_of_checked : checked -> canonical_def list
val serialize_checked_memo : (checked * string) list ref
val serialize_checked_program : checked -> string
val hash_program_memo : (checked * string) list ref
val hash_program : checked -> string
val checked_to_graph_json_fields_uncached :
  version:string ->
  include_capability_scope_ref:bool -> checked -> string list
val graph_json_fields_memo : (checked * string * bool * string list) list ref
val checked_to_graph_json_fields :
  ?version:string ->
  ?include_capability_scope_ref:bool -> checked -> string list
val graph_payload_memo : (checked * string * bool * string) list ref
val checked_to_graph_payload_json_for :
  version:string -> include_capability_scope_ref:bool -> checked -> string
val checked_to_graph_payload_json : checked -> string
val graph_content_hash_memo : (checked * string * bool * string) list ref
val checked_to_graph_content_hash_for :
  version:string -> include_capability_scope_ref:bool -> checked -> string
val checked_to_graph_content_hash : checked -> string
val graph_json_memo : (checked * string * bool * string) list ref
val checked_to_graph_json_for :
  version:string -> include_capability_scope_ref:bool -> checked -> string
val checked_to_graph_json : checked -> string
val checked_to_graph_json_legacy_v1 : checked -> string
val checked_to_graph_content_hash_legacy_v1 : checked -> string
val checked_def_by_name : checked -> string -> checked_def option
type termination_counts = {
  fold_nat : int;
  fold_list : int;
  fold_variant : int;
  recur : int;
}
val termination_counts_term : cterm -> termination_counts
val termination_status : termination_counts -> string
val termination_explanation_text : checked -> string -> string
val shift : int -> int -> cterm -> cterm
val shift_branch : int -> int -> cbranch -> cbranch
val subst : int -> cterm -> cterm -> cterm
val subst_branch : int -> cterm -> cbranch -> cbranch
val subst_top : cterm -> cterm -> cterm
val subst_top2 : cterm -> cterm -> cterm -> cterm
val subst_type_in_cterm : Ast.typ list -> cterm -> cterm
val subst_type_in_branch : Ast.typ list -> cbranch -> cbranch
val normalize_cterm : checked -> cterm -> cterm
val normalize_branch : checked -> cbranch -> cbranch
val normalize_checked_def : checked -> string -> cterm
