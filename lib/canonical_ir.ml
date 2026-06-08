type term = Kernel.cterm

type branch = Kernel.cbranch

type def = Kernel.canonical_def

let version = Kernel.canonical_version

let serialize_def = Kernel.serialize_def

let serialize_program = Kernel.serialize_program

let serialize_graph = Kernel.checked_to_graph_json

let parse_def = Kernel.parse_serialized_def

let parse_program = Kernel.parse_serialized_program

let term_to_string = Kernel.cterm_to_string
