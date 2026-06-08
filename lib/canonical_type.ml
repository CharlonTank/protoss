type t = Ast.typ

let serialize = Kernel.type_to_canonical

let parse = Kernel.type_of_canonical_sexp
