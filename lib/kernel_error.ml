exception Error of string

let fail msg = raise (Error msg)
