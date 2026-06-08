exception Error of string

type t =
  | Null
  | Bool of bool
  | Num of int
  | String of string
  | Array of t list
  | Object of (string * t) list

let fail msg = raise (Error msg)

let line_col input offset =
  let limit = min (max 0 offset) (String.length input) in
  let line = ref 1 and col = ref 1 in
  for i = 0 to limit - 1 do
    if input.[i] = '\n' then (
      incr line;
      col := 1)
    else incr col
  done;
  (!line, !col)

let error_at input offset msg =
  let line, col = line_col input offset in
  fail (string_of_int line ^ ":" ^ string_of_int col ^ ": " ^ msg)

let parse input =
  let len = String.length input in
  let rec skip i =
    if i < len then
      match input.[i] with
      | ' ' | '\t' | '\r' | '\n' -> skip (i + 1)
      | _ -> i
    else i
  in
  let expect_word i word value =
    let n = String.length word in
    if i + n <= len && String.sub input i n = word then (value, i + n)
    else error_at input i ("expected " ^ word)
  in
  let rec parse_string start buf i =
    if i >= len then error_at input start "unterminated JSON string"
    else
      match input.[i] with
      | '"' -> (Buffer.contents buf, i + 1)
      | '\\' when i + 1 < len ->
          let c =
            match input.[i + 1] with
            | '"' -> '"'
            | '\\' -> '\\'
            | '/' -> '/'
            | 'b' -> '\b'
            | 'f' -> '\012'
            | 'n' -> '\n'
            | 'r' -> '\r'
            | 't' -> '\t'
            | c -> c
          in
          Buffer.add_char buf c;
          parse_string start buf (i + 2)
      | c ->
          Buffer.add_char buf c;
          parse_string start buf (i + 1)
  in
  let rec value i =
    let i = skip i in
    if i >= len then error_at input i "unexpected end of JSON"
    else
      match input.[i] with
      | 'n' -> expect_word i "null" Null
      | 't' -> expect_word i "true" (Bool true)
      | 'f' -> expect_word i "false" (Bool false)
      | '"' ->
          let s, j = parse_string i (Buffer.create 16) (i + 1) in
          (String s, j)
      | '[' -> array i [] (i + 1)
      | '{' -> object_ i [] (i + 1)
      | '-' | '0' .. '9' -> number i
      | c -> error_at input i ("unexpected JSON character: " ^ String.make 1 c)
  and number i =
    let j = ref i in
    if !j < len && input.[!j] = '-' then incr j;
    let digit_start = !j in
    while !j < len
          &&
          match input.[!j] with
          | '0' .. '9' -> true
          | _ -> false
    do
      incr j
    done;
    if !j = digit_start then error_at input i "invalid JSON number";
    let j = !j in
    try (Num (int_of_string (String.sub input i (j - i))), j)
    with Failure _ -> error_at input i "invalid JSON number"
  and array start acc i =
    let i = skip i in
    if i >= len then error_at input start "unterminated JSON array"
    else if input.[i] = ']' then (Array (List.rev acc), i + 1)
    else
      let v, j = value i in
      let j = skip j in
      if j < len && input.[j] = ',' then array start (v :: acc) (j + 1)
      else if j < len && input.[j] = ']' then (Array (List.rev (v :: acc)), j + 1)
      else error_at input j "expected , or ] in JSON array"
  and object_ start acc i =
    let i = skip i in
    if i >= len then error_at input start "unterminated JSON object"
    else if input.[i] = '}' then (Object (List.rev acc), i + 1)
    else
      let key, j =
        match value i with
        | String s, j -> (s, j)
        | _ -> error_at input i "expected JSON object key"
      in
      let j = skip j in
      if j >= len || input.[j] <> ':' then error_at input j "expected : in JSON object";
      let v, k = value (j + 1) in
      let k = skip k in
      if k < len && input.[k] = ',' then object_ start ((key, v) :: acc) (k + 1)
      else if k < len && input.[k] = '}' then (Object (List.rev ((key, v) :: acc)), k + 1)
      else error_at input k "expected , or } in JSON object"
  in
  let v, i = value 0 in
  let i = skip i in
  if i <> len then error_at input i "trailing JSON input";
  v

let field name = function
  | Object fields -> List.find_opt (fun (n, _) -> String.equal n name) fields |> Option.map snd
  | _ -> None

let string = function String s -> Some s | _ -> None

let array = function Array xs -> Some xs | _ -> None
