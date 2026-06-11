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

(* Encode a Unicode code point as UTF-8. For cp < 0x80 this is a single byte,
   the exact inverse of to_string's "\u%04x" control-byte escape. *)
let add_utf8 buf cp =
  if cp < 0x80 then Buffer.add_char buf (Char.chr cp)
  else if cp < 0x800 then (
    Buffer.add_char buf (Char.chr (0xc0 lor (cp lsr 6)));
    Buffer.add_char buf (Char.chr (0x80 lor (cp land 0x3f))))
  else (
    Buffer.add_char buf (Char.chr (0xe0 lor (cp lsr 12)));
    Buffer.add_char buf (Char.chr (0x80 lor ((cp lsr 6) land 0x3f)));
    Buffer.add_char buf (Char.chr (0x80 lor (cp land 0x3f))))

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
      | '\\' when i + 1 < len && input.[i + 1] = 'u' ->
          (* \uXXXX: the inverse of to_string's "\u%04x" for control bytes;
             decode the 4 hex digits to a code point and emit it as UTF-8
             (a single byte for cp < 0x80, the exact inverse of the emitter). *)
          if i + 5 >= len then error_at input i "truncated JSON \\u escape"
          else (
            match int_of_string_opt ("0x" ^ String.sub input (i + 2) 4) with
            | None -> error_at input i "invalid JSON \\u escape"
            | Some cp ->
                add_utf8 buf cp;
                parse_string start buf (i + 6))
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

(* Most strings (hashes, def names, kinds) need no escaping, so scan first and
   memcpy the whole string when clean instead of feeding the buffer char by
   char — escaping showed up in profiles of store-heavy flows. *)
let escape_into buf s =
  let len = String.length s in
  let clean = ref true in
  (try
     for i = 0 to len - 1 do
       let c = String.unsafe_get s i in
       if c = '"' || c = '\\' || Char.code c < 0x20 then (
         clean := false;
         raise Exit)
     done
   with Exit -> ());
  if !clean then Buffer.add_string buf s
  else
    String.iter
      (fun c ->
        match c with
        | '"' -> Buffer.add_string buf "\\\""
        | '\\' -> Buffer.add_string buf "\\\\"
        | '\n' -> Buffer.add_string buf "\\n"
        | '\r' -> Buffer.add_string buf "\\r"
        | '\t' -> Buffer.add_string buf "\\t"
        | '\b' -> Buffer.add_string buf "\\b"
        | '\012' -> Buffer.add_string buf "\\f"
        | c when Char.code c < 0x20 ->
            Buffer.add_string buf (Printf.sprintf "\\u%04x" (Char.code c))
        | c -> Buffer.add_char buf c)
      s

let escape s =
  let buf = Buffer.create (String.length s + 2) in
  escape_into buf s;
  Buffer.contents buf

(* Canonical, deterministic encoder: object keys are emitted in sorted order so
   the same logical value always serializes to identical bytes regardless of how
   it was constructed. Used for content-addressed runtime objects. Writes into a
   single buffer end to end — building nested values as intermediate strings
   re-copied every enclosing level and dominated allocation on large graphs. *)
let rec write_value buf = function
  | Null -> Buffer.add_string buf "null"
  | Bool b -> Buffer.add_string buf (if b then "true" else "false")
  | Num n -> Buffer.add_string buf (string_of_int n)
  | String s ->
      Buffer.add_char buf '"';
      escape_into buf s;
      Buffer.add_char buf '"'
  | Array xs ->
      Buffer.add_char buf '[';
      List.iteri
        (fun i x ->
          if i > 0 then Buffer.add_char buf ',';
          write_value buf x)
        xs;
      Buffer.add_char buf ']'
  | Object fields ->
      let sorted = List.sort (fun (a, _) (b, _) -> String.compare a b) fields in
      Buffer.add_char buf '{';
      List.iteri
        (fun i (k, v) ->
          if i > 0 then Buffer.add_char buf ',';
          Buffer.add_char buf '"';
          escape_into buf k;
          Buffer.add_char buf '"';
          Buffer.add_char buf ':';
          write_value buf v)
        sorted;
      Buffer.add_char buf '}'

let to_string v =
  let buf = Buffer.create 256 in
  write_value buf v;
  Buffer.contents buf
