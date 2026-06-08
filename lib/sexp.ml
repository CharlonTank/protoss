exception Error of string

type t =
  | Atom of string
  | Str of string
  | List of t list

let is_space = function ' ' | '\t' | '\r' | '\n' -> true | _ -> false

let is_delim = function
  | '(' | ')' | '"' | ';' -> true
  | c -> is_space c

let parse input =
  let len = String.length input in
  let rec skip i =
    if i >= len then i
    else
      match input.[i] with
      | c when is_space c -> skip (i + 1)
      | ';' ->
          let rec line j =
            if j >= len || input.[j] = '\n' then skip j else line (j + 1)
          in
          line (i + 1)
      | _ -> i
  in
  let rec parse_string buf i =
    if i >= len then raise (Error "unterminated string")
    else
      match input.[i] with
      | '"' -> (Buffer.contents buf, i + 1)
      | '\\' when i + 1 < len ->
          let c =
            match input.[i + 1] with
            | 'n' -> '\n'
            | 'r' -> '\r'
            | 't' -> '\t'
            | '"' -> '"'
            | '\\' -> '\\'
            | c -> c
          in
          Buffer.add_char buf c;
          parse_string buf (i + 2)
      | c ->
          Buffer.add_char buf c;
          parse_string buf (i + 1)
  in
  let parse_atom i =
    let j = ref i in
    while !j < len && not (is_delim input.[!j]) do
      incr j
    done;
    if !j = i then raise (Error ("unexpected character: " ^ String.make 1 input.[i]));
    (String.sub input i (!j - i), !j)
  in
  let rec expr i =
    let i = skip i in
    if i >= len then raise (Error "unexpected end of input")
    else
      match input.[i] with
      | '(' ->
          let rec items acc j =
            let j = skip j in
            if j >= len then raise (Error "unterminated list")
            else if input.[j] = ')' then (List (List.rev acc), j + 1)
            else
              let item, k = expr j in
              items (item :: acc) k
          in
          items [] (i + 1)
      | ')' -> raise (Error "unexpected )")
      | '"' ->
          let s, j = parse_string (Buffer.create 16) (i + 1) in
          (Str s, j)
      | _ ->
          let a, j = parse_atom i in
          (Atom a, j)
  in
  let rec many acc i =
    let i = skip i in
    if i >= len then List.rev acc
    else
      let item, j = expr i in
      many (item :: acc) j
  in
  many [] 0

let atom = function Atom s -> Some s | _ -> None

let rec to_string = function
  | Atom s -> s
  | Str s ->
      let b = Buffer.create (String.length s + 2) in
      Buffer.add_char b '"';
      String.iter
        (function
          | '"' -> Buffer.add_string b "\\\""
          | '\\' -> Buffer.add_string b "\\\\"
          | '\n' -> Buffer.add_string b "\\n"
          | '\r' -> Buffer.add_string b "\\r"
          | '\t' -> Buffer.add_string b "\\t"
          | c -> Buffer.add_char b c)
        s;
      Buffer.add_char b '"';
      Buffer.contents b
  | List xs -> "(" ^ String.concat " " (List.map to_string xs) ^ ")"
