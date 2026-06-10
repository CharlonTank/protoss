exception Error of string

let fail msg = raise (Error msg)

let trim = String.trim

let starts_with s prefix =
  let n = String.length prefix in
  String.length s >= n && String.sub s 0 n = prefix

let is_space = function ' ' | '\t' | '\r' | '\n' -> true | _ -> false

let is_digit = function '0' .. '9' -> true | _ -> false

let is_ident_start = function
  | 'A' .. 'Z' | 'a' .. 'z' | '_' -> true
  | _ -> false

let is_ident_char = function
  | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' | '.' -> true
  | _ -> false

let is_name s =
  let len = String.length s in
  len > 0 && is_ident_start s.[0]
  &&
  let rec loop i =
    i >= len || (is_ident_char s.[i] && loop (i + 1))
  in
  loop 1

let indentation line =
  let rec loop i =
    if i < String.length line && (line.[i] = ' ' || line.[i] = '\t') then loop (i + 1)
    else i
  in
  loop 0

let strip_line_comment line =
  let len = String.length line in
  let rec loop in_string escaped i =
    if i >= len then line
    else
      match line.[i] with
      | '-' when (not in_string) && i + 1 < len && line.[i + 1] = '-' ->
          String.sub line 0 i
      | '"' when not escaped -> loop (not in_string) false (i + 1)
      | '\\' when in_string && not escaped -> loop in_string true (i + 1)
      | _ -> loop in_string false (i + 1)
  in
  loop false false 0

let split_words s =
  s |> String.split_on_char ' ' |> List.map trim |> List.filter (( <> ) "")

let ensure_unique_names what names =
  let seen = Hashtbl.create 16 in
  List.iter
    (fun name ->
      if Hashtbl.mem seen name then fail ("duplicate " ^ what ^ ": " ^ name);
      Hashtbl.add seen name ())
    names

type layout_line = {
  indent : int;
  text : string;
}

let layout_lines text =
  text |> String.split_on_char '\n'
  |> List.filter_map (fun raw ->
         let source = strip_line_comment raw in
         let text = trim source in
         if text = "" then None else Some { indent = indentation source; text })

let layout_line_source line = String.make line.indent ' ' ^ line.text

let find_sub s needle =
  let n = String.length needle in
  let rec loop i =
    if i + n > String.length s then None
    else if String.sub s i n = needle then Some i
    else loop (i + 1)
  in
  loop 0

let value_separator line =
  match String.index_opt line '=' with
  | None -> None
  | Some i ->
      let lhs = trim (String.sub line 0 i) in
      let rhs = trim (String.sub line (i + 1) (String.length line - i - 1)) in
      (match split_words lhs with
      | name :: params when is_name name && List.for_all is_name params -> Some (name, params, rhs)
      | _ -> None)

let nat_type = Sexp.Atom "Nat"

let fun_type args result =
  List.fold_right
    (fun arg acc -> Sexp.List [ Sexp.Atom "->"; arg; acc ])
    args result

let nat_fun_type arity =
  fun_type (List.init arity (fun _ -> nat_type)) nat_type

type token =
  | Ident of string
  | Str of string
  | LParen
  | RParen
  | LBrace
  | RBrace
  | LBracket
  | RBracket
  | Colon
  | Equals
  | Comma
  | Pipe
  | PipeGt
  | Arrow
  | Backslash

let token_name = function
  | Ident s -> s
  | Str _ -> "string"
  | LParen -> "("
  | RParen -> ")"
  | LBrace -> "{"
  | RBrace -> "}"
  | LBracket -> "["
  | RBracket -> "]"
  | Colon -> ":"
  | Equals -> "="
  | Comma -> ","
  | Pipe -> "|"
  | PipeGt -> "|>"
  | Arrow -> "->"
  | Backslash -> "\\"

let tokenize input =
  let len = String.length input in
  let rec string start buf i =
    if i >= len then fail "unterminated string"
    else
      match input.[i] with
      | '"' -> (Str (Buffer.contents buf), i + 1)
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
          string start buf (i + 2)
      | c ->
          Buffer.add_char buf c;
          string start buf (i + 1)
  in
  let ident_token i =
    let j = ref i in
    while
      !j < len
      && (not (is_space input.[!j]))
      &&
      match input.[!j] with
      | '(' | ')' | '{' | '}' | '[' | ']' | ':' | '=' | ',' | '|' | '+' | '<' | '>' | '&'
      | '/' | '\\' ->
          false
      | '-' when !j + 1 < len && input.[!j + 1] = '>' -> false
      | _ -> true
    do
      incr j
    done;
    if !j = i then fail ("unexpected character: " ^ String.make 1 input.[i]);
    (Ident (String.sub input i (!j - i)), !j)
  in
  let rec loop acc i =
    if i >= len then List.rev acc
    else if is_space input.[i] then loop acc (i + 1)
    else
      match input.[i] with
      | '(' -> loop (LParen :: acc) (i + 1)
      | ')' -> loop (RParen :: acc) (i + 1)
      | '{' -> loop (LBrace :: acc) (i + 1)
      | '}' -> loop (RBrace :: acc) (i + 1)
      | '[' -> loop (LBracket :: acc) (i + 1)
      | ']' -> loop (RBracket :: acc) (i + 1)
      | ':' -> loop (Colon :: acc) (i + 1)
      | '=' when i + 1 < len && input.[i + 1] = '=' -> loop (Ident "==" :: acc) (i + 2)
      | '=' -> loop (Equals :: acc) (i + 1)
      | ',' -> loop (Comma :: acc) (i + 1)
      | '+' -> loop (Ident "+" :: acc) (i + 1)
      | '/' when i + 1 < len && input.[i + 1] = '=' -> loop (Ident "/=" :: acc) (i + 2)
      | '<' when i + 1 < len && input.[i + 1] = '=' -> loop (Ident "<=" :: acc) (i + 2)
      | '>' when i + 1 < len && input.[i + 1] = '=' -> loop (Ident ">=" :: acc) (i + 2)
      | '<' -> loop (Ident "<" :: acc) (i + 1)
      | '>' -> loop (Ident ">" :: acc) (i + 1)
      | '&' when i + 1 < len && input.[i + 1] = '&' -> loop (Ident "&&" :: acc) (i + 2)
      | '&' -> fail "unexpected character: &"
      | '\\' -> loop (Backslash :: acc) (i + 1)
      | '|' when i + 1 < len && input.[i + 1] = '|' -> loop (Ident "||" :: acc) (i + 2)
      | '|' when i + 1 < len && input.[i + 1] = '>' -> loop (PipeGt :: acc) (i + 2)
      | '|' -> loop (Pipe :: acc) (i + 1)
      | '-' when i + 1 < len && input.[i + 1] = '>' -> loop (Arrow :: acc) (i + 2)
      | '"' ->
          let tok, j = string i (Buffer.create 16) (i + 1) in
          loop (tok :: acc) j
      | _ ->
          let tok, j = ident_token i in
          loop (tok :: acc) j
  in
  loop [] 0

type stream = { tokens : token array; mutable pos : int }

let stream tokens = { tokens = Array.of_list tokens; pos = 0 }

let peek st =
  if st.pos >= Array.length st.tokens then None else Some st.tokens.(st.pos)

let take st =
  match peek st with
  | None -> None
  | Some tok ->
      st.pos <- st.pos + 1;
      Some tok

let expect st wanted =
  match take st with
  | Some tok when tok = wanted -> ()
  | Some tok -> fail ("expected " ^ token_name wanted ^ ", got " ^ token_name tok)
  | None -> fail ("expected " ^ token_name wanted)

let at_end st = st.pos >= Array.length st.tokens

let rec parse_type tokens =
  let st = stream tokens in
  let ty = parse_fun_type st in
  if not (at_end st) then fail ("unexpected type token: " ^ token_name st.tokens.(st.pos));
  ty

and parse_fun_type st =
  let left = parse_app_type st in
  match peek st with
  | Some Arrow ->
      ignore (take st);
      Sexp.List [ Sexp.Atom "->"; left; parse_fun_type st ]
  | _ -> left

and parse_app_type st =
  let rec collect acc =
    match peek st with
    | Some (Ident _ | LParen | LBrace) -> collect (parse_type_atom st :: acc)
    | _ -> List.rev acc
  in
  match collect [] with
  | [] -> fail "expected type"
  | [ one ] -> one
  | Sexp.Atom name :: args -> Sexp.List (Sexp.Atom name :: args)
  | head :: _ -> fail ("invalid type application head: " ^ Sexp.to_string head)

and parse_type_atom st =
  match take st with
  | Some (Ident name) -> Sexp.Atom name
  | Some LParen ->
      let ty = parse_fun_type st in
      expect st RParen;
      ty
  | Some LBrace -> parse_record_type st
  | Some tok -> fail ("expected type atom, got " ^ token_name tok)
  | None -> fail "expected type atom"

and parse_record_type st =
  let rec fields acc =
    match peek st with
    | Some RBrace ->
        ignore (take st);
        Sexp.List (Sexp.Atom "Record" :: List.rev acc)
    | Some Comma ->
        ignore (take st);
        fields acc
    | Some (Ident name) ->
        ignore (take st);
        expect st Colon;
        let ty = parse_fun_type st in
        (match peek st with Some Comma -> ignore (take st) | _ -> ());
        fields (Sexp.List [ Sexp.Atom name; ty ] :: acc)
    | Some tok -> fail ("invalid record type field: " ^ token_name tok)
    | None -> fail "unterminated record type"
  in
  fields []

let parse_type_text text = parse_type (tokenize text)

type signature_type = {
  typ : Sexp.t;
  capabilities : string list option;
}

let plain_signature typ = { typ; capabilities = None }

let parse_signature_type_text text =
  let tokens = tokenize text in
  match tokens with
  | Ident "Process" :: LBrace :: rest ->
      let rec caps acc = function
        | RBrace :: value_tokens ->
            ensure_unique_names "process capability" acc;
            let capabilities = List.sort String.compare acc in
            let value_type = parse_type value_tokens in
            {
              typ =
                Sexp.List
                  [
                    Sexp.Atom "Process";
                    Sexp.List (Sexp.Atom "capabilities" :: List.map (fun c -> Sexp.Atom c) capabilities);
                    value_type;
                  ];
              capabilities = Some capabilities;
            }
        | Comma :: rest -> caps acc rest
        | Ident name :: rest ->
            if not (is_name name) then fail ("invalid process capability: " ^ name);
            caps (name :: acc) rest
        | tok :: _ -> fail ("invalid process capability token: " ^ token_name tok)
        | [] -> fail "unterminated Process capability set"
      in
      caps [] rest
  | _ -> plain_signature (parse_type tokens)

let definition_form name typ capabilities expr =
  match capabilities with
  | None -> Sexp.List [ Sexp.Atom "def"; Sexp.Atom name; typ; expr ]
  | Some caps ->
      Sexp.List
        [
          Sexp.Atom "defcap";
          Sexp.Atom name;
          Sexp.List (Sexp.Atom "capabilities" :: List.map (fun c -> Sexp.Atom c) caps);
          typ;
          expr;
        ]

let split_variant_cases tokens =
  let rec loop depth current acc = function
    | [] -> List.rev (List.rev current :: acc)
    | Pipe :: rest when depth = 0 -> loop depth [] (List.rev current :: acc) rest
    | LParen :: rest -> loop (depth + 1) (LParen :: current) acc rest
    | LBrace :: rest -> loop (depth + 1) (LBrace :: current) acc rest
    | LBracket :: rest -> loop (depth + 1) (LBracket :: current) acc rest
    | RParen :: rest -> loop (max 0 (depth - 1)) (RParen :: current) acc rest
    | RBrace :: rest -> loop (max 0 (depth - 1)) (RBrace :: current) acc rest
    | RBracket :: rest -> loop (max 0 (depth - 1)) (RBracket :: current) acc rest
    | tok :: rest -> loop depth (tok :: current) acc rest
  in
  loop 0 [] [] tokens

let rec parse_payload_atoms acc st =
  if at_end st then List.rev acc else parse_payload_atoms (parse_type_atom st :: acc) st

let tuple_payload = function
  | [] -> Sexp.Atom "Unit"
  | [ one ] -> one
  | payloads ->
      Sexp.List
        (Sexp.Atom "Record"
        :: List.mapi
             (fun i ty -> Sexp.List [ Sexp.Atom ("_" ^ string_of_int (i + 1)); ty ])
             payloads)

let list_literal elems =
  List.fold_right
    (fun elem acc -> Sexp.List [ Sexp.Atom "Cons"; elem; acc ])
    elems (Sexp.Atom "Nil")

let nat_add_expr left right =
  Sexp.List
    [
      Sexp.Atom "foldNat";
      left;
      right;
      Sexp.List [ Sexp.Atom "lambda"; Sexp.Atom "__infix_add_acc"; Sexp.List [ Sexp.Atom "succ"; Sexp.Atom "__infix_add_acc" ] ];
    ]

let call2 name left right = Sexp.List [ Sexp.List [ Sexp.Atom name; left ]; right ]

let infix_expr name left right =
  match name with
  | "+" -> nat_add_expr left right
  | "==" -> call2 "Nat.eqNat" left right
  | "/=" -> Sexp.List [ Sexp.Atom "Bool.not"; call2 "Nat.eqNat" left right ]
  | "<" -> call2 "Nat.lt" left right
  | "<=" -> call2 "Nat.lte" left right
  | ">" -> call2 "Nat.gt" left right
  | ">=" -> call2 "Nat.gte" left right
  | "&&" -> call2 "Bool.and" left right
  | "||" -> call2 "Bool.or" left right
  | _ -> fail ("unsupported infix operator: " ^ name)

let is_infix_operator = function
  | "+" | "==" | "/=" | "<" | "<=" | ">" | ">=" | "&&" | "||" -> true
  | _ -> false

let wrap_lambdas params body =
  let rec ensure_unique_params seen = function
    | [] -> ()
    | name :: rest ->
        if List.exists (String.equal name) seen then fail ("duplicate parameter: " ^ name);
        ensure_unique_params (name :: seen) rest
  in
  ensure_unique_params [] params;
  List.fold_right
    (fun name acc -> Sexp.List [ Sexp.Atom "lambda"; Sexp.Atom name; acc ])
    params body

let field_access_expr name =
  match String.split_on_char '.' name with
  | base :: field :: rest
    when base <> ""
         &&
         (match base.[0] with 'a' .. 'z' | '_' -> true | _ -> false)
         && is_name base && List.for_all is_name (field :: rest) ->
      Some
        (List.fold_left
           (fun acc field -> Sexp.List [ Sexp.Atom "get"; acc; Sexp.Atom field ])
           (Sexp.Atom base) (field :: rest))
  | _ -> None

let has_plus_operator text =
  tokenize text |> List.exists (function Ident "+" -> true | _ -> false)

let infer_missing_value_type name params body =
  if has_plus_operator body then nat_fun_type (List.length params)
  else fail ("missing type signature for " ^ name)

let infer_missing_let_type name params body =
  if has_plus_operator body then nat_fun_type (List.length params)
  else fail ("let function binding requires a type signature: " ^ name)

let parse_variant_case tokens =
  match tokens with
  | Ident name :: rest ->
      let payloads = parse_payload_atoms [] (stream rest) in
      Sexp.List [ Sexp.Atom name; tuple_payload payloads ]
  | [] -> fail "empty variant constructor"
  | tok :: _ -> fail ("expected variant constructor, got " ^ token_name tok)

let signature_separator line =
  match String.index_opt line ':' with
  | None -> None
  | Some i ->
      let name = trim (String.sub line 0 i) in
      if is_name name then Some (name, trim (String.sub line (i + 1) (String.length line - i - 1)))
      else None

let rec parse_expr_text text =
  let text = trim text in
  if text = "let" || starts_with text "let\n" then parse_let_expr text
  else if starts_with text "case " then parse_case_expr text
  else
    let st = stream (tokenize text) in
    let expr = parse_expr st in
    if not (at_end st) then fail ("unexpected expression token: " ^ token_name st.tokens.(st.pos));
    expr

and parse_let_expr text =
  let lines = layout_lines text in
  match lines with
  | { text = "let"; _ } :: rest ->
      let signatures = Hashtbl.create 8 in
      let finish_binding acc = function
        | None -> acc
        | Some (name, params, source_lines) ->
            let source = String.concat "\n" (List.rev source_lines) |> trim in
            if source = "" then fail ("let binding missing body: " ^ name);
            (name, params, source) :: acc
      in
      let rec split_bindings binding_indent acc current = function
        | [] -> fail "let block missing in"
        | line :: rest
          when line.text = "in"
               &&
               (match binding_indent with
               | None -> true
               | Some indent -> line.indent <= indent) ->
            let acc = finish_binding acc current in
            if acc = [] then fail "let block requires at least one binding";
            (List.rev acc, rest)
        | line :: rest -> (
            let binding_indent =
              match binding_indent with None -> line.indent | Some indent -> indent
            in
            if line.indent < binding_indent then
              fail ("unexpected let dedent: " ^ line.text)
            else if line.indent > binding_indent then
              match current with
              | None -> fail ("unexpected indented let line: " ^ line.text)
              | Some (name, params, source_lines) ->
                  split_bindings (Some binding_indent) acc
                    (Some (name, params, layout_line_source line :: source_lines))
                    rest
            else
              let acc = finish_binding acc current in
              match signature_separator line.text with
              | Some (name, ty) ->
                  Hashtbl.replace signatures name (parse_type_text ty);
                  split_bindings (Some binding_indent) acc None rest
              | None -> (
                  match value_separator line.text with
                  | Some (name, params, expr) ->
                      let source_lines = if expr = "" then [] else [ expr ] in
                      split_bindings (Some binding_indent) acc
                        (Some (name, params, source_lines))
                        rest
                  | None -> fail ("invalid let binding: " ^ line.text)))
      in
      let bindings, body = split_bindings None [] None rest in
      let body =
        match body with
        | [] -> fail "let block missing body"
        | lines -> parse_expr_text (String.concat "\n" (List.map layout_line_source lines))
      in
      List.fold_right
        (fun (name, params, source) acc ->
          let expr = parse_expr_text source |> wrap_lambdas params in
          match Hashtbl.find_opt signatures name with
          | Some ty -> Sexp.List [ Sexp.Atom "let"; Sexp.List [ Sexp.Atom name; ty; expr ]; acc ]
          | None ->
              if params = [] then Sexp.List [ Sexp.Atom "let"; Sexp.List [ Sexp.Atom name; expr ]; acc ]
              else
                let ty = infer_missing_let_type name params source in
                Sexp.List [ Sexp.Atom "let"; Sexp.List [ Sexp.Atom name; ty; expr ]; acc ])
        bindings body
  | _ -> fail "let syntax is: let ... in ..."

and parse_case_expr text =
  let lines = layout_lines text in
  match lines with
  | header :: branch_lines when starts_with header.text "case " && String.length header.text >= 8 ->
      let scrutinee =
        match find_sub header.text " of" with
        | Some i -> trim (String.sub header.text 5 (i - 5))
        | None -> fail "case syntax is: case expr of"
      in
      if branch_lines = [] then fail "case expression requires branches";
      let branches = parse_case_branches branch_lines in
      Sexp.List (Sexp.Atom "match" :: parse_expr_text scrutinee :: branches)
  | _ -> fail "case syntax is: case expr of"

and parse_case_branches lines =
  let finish acc = function
    | None -> List.rev acc
    | Some (pattern, body_lines) ->
        let body = String.concat "\n" (List.rev body_lines) in
        if trim body = "" then fail ("case branch missing body: " ^ pattern);
        parse_case_branch pattern body :: acc |> List.rev
  in
  let branch_indent =
    match lines with [] -> fail "case expression requires branches" | line :: _ -> line.indent
  in
  let rec loop acc current = function
    | [] -> finish acc current
    | line :: rest -> (
        if line.indent < branch_indent then fail ("unexpected case dedent: " ^ line.text)
        else if line.indent > branch_indent then
          match current with
          | None -> fail ("unexpected indented case line: " ^ line.text)
          | Some (pattern, body) ->
              loop acc (Some (pattern, layout_line_source line :: body)) rest
        else
          match find_sub line.text "->" with
        | Some i ->
            let pattern = trim (String.sub line.text 0 i) in
            let body =
              trim (String.sub line.text (i + 2) (String.length line.text - i - 2))
            in
            let acc =
              match current with
              | None -> acc
              | Some (old_pattern, old_body) ->
                  parse_case_branch old_pattern (String.concat "\n" (List.rev old_body)) :: acc
            in
            let body = if body = "" then [] else [ body ] in
            loop acc (Some (pattern, body)) rest
        | None -> fail ("case branch missing ->: " ^ line.text))
  in
  loop [] None lines

and parse_case_branch pattern body =
  match split_words pattern with
  | [] -> fail "empty case branch pattern"
  | [ "_" ] -> Sexp.List [ Sexp.Atom "_"; parse_expr_text body ]
  | [ "true" ] -> Sexp.List [ Sexp.Atom "true"; parse_expr_text body ]
  | [ "false" ] -> Sexp.List [ Sexp.Atom "false"; parse_expr_text body ]
  | [ con ] -> Sexp.List [ Sexp.Atom con; parse_expr_text body ]
  | [ con; binder ] -> Sexp.List [ Sexp.Atom con; Sexp.Atom binder; parse_expr_text body ]
  | [ "Cons"; head; tail ] -> Sexp.List [ Sexp.Atom "Cons"; Sexp.Atom head; Sexp.Atom tail; parse_expr_text body ]
  | _ -> fail ("unsupported case branch pattern: " ^ pattern)

and parse_expr st =
  match peek st with
  | Some Backslash ->
      ignore (take st);
      let rec params acc =
        match take st with
        | Some (Ident name) -> params (name :: acc)
        | Some Arrow ->
            if acc = [] then fail "lambda requires at least one parameter";
            List.rev acc
        | Some tok -> fail ("expected lambda parameter, got " ^ token_name tok)
        | None -> fail "expected lambda parameter"
      in
      let params = params [] in
      let body = parse_expr st in
      wrap_lambdas params body
  | _ -> parse_pipeline st

and parse_pipeline st =
  let rec loop acc =
    match peek st with
    | Some PipeGt ->
        ignore (take st);
        loop (append_pipeline_arg (parse_app st) acc)
    | _ -> acc
  in
  loop (parse_bool_or st)

and parse_bool_or st =
  let rec loop acc =
    match peek st with
    | Some (Ident "||") ->
        ignore (take st);
        loop (infix_expr "||" acc (parse_bool_and st))
    | _ -> acc
  in
  loop (parse_bool_and st)

and parse_bool_and st =
  let rec loop acc =
    match peek st with
    | Some (Ident "&&") ->
        ignore (take st);
        loop (infix_expr "&&" acc (parse_compare st))
    | _ -> acc
  in
  loop (parse_compare st)

and parse_compare st =
  let left = parse_sum st in
  match peek st with
  | Some (Ident (("==" | "/=" | "<" | "<=" | ">" | ">=") as op)) ->
      ignore (take st);
      infix_expr op left (parse_sum st)
  | _ -> left

and parse_sum st =
  let rec loop acc =
    match peek st with
    | Some (Ident "+") ->
        ignore (take st);
        loop (infix_expr "+" acc (parse_app st))
    | _ -> acc
  in
  loop (parse_app st)

and parse_app st =
  let rec collect acc =
    match peek st with
    | Some (Ident op) when is_infix_operator op -> List.rev acc
    | Some (Ident _ | Str _ | LParen | LBrace | LBracket | Backslash) ->
        collect (parse_atom_expr st :: acc)
    | _ -> List.rev acc
  in
  match collect [] with
  | [] -> fail "expected expression"
  | [ one ] -> one
  | f :: args -> Sexp.List (f :: args)

and parse_atom_expr st =
  match take st with
  | Some (Ident "if") -> parse_if_expr st
  | Some (Ident "not") -> Sexp.List [ Sexp.Atom "Bool.not"; parse_atom_expr st ]
  | Some (Ident op) when is_infix_operator op -> fail (op ^ " is an infix operator")
  | Some (Ident name) -> (
      match field_access_expr name with Some expr -> expr | None -> Sexp.Atom name)
  | Some (Str value) -> Sexp.Str value
  | Some LParen ->
      let expr = parse_expr st in
      expect st RParen;
      expr
  | Some LBrace -> parse_record_expr st
  | Some LBracket -> parse_list_expr st
  | Some Backslash ->
      st.pos <- st.pos - 1;
      parse_expr st
  | Some tok -> fail ("expected expression atom, got " ^ token_name tok)
  | None -> fail "expected expression atom"

and parse_record_expr st =
  let rec literal_fields acc =
    match peek st with
    | Some RBrace ->
        ignore (take st);
        Sexp.List (Sexp.Atom "record" :: List.rev acc)
    | Some Comma ->
        ignore (take st);
        literal_fields acc
    | Some (Ident name) ->
        ignore (take st);
        expect st Equals;
        let expr = parse_expr st in
        (match peek st with Some Comma -> ignore (take st) | _ -> ());
        literal_fields (Sexp.List [ Sexp.Atom name; expr ] :: acc)
    | Some tok -> fail ("invalid record field: " ^ token_name tok)
    | None -> fail "unterminated record expression"
  in
  let rec update_fields acc =
    match peek st with
    | Some RBrace ->
        if acc = [] then fail "record update requires at least one field";
        ignore (take st);
        List.rev acc
    | Some Comma ->
        ignore (take st);
        update_fields acc
    | Some (Ident name) ->
        ignore (take st);
        expect st Equals;
        let expr = parse_expr st in
        (match peek st with Some Comma -> ignore (take st) | _ -> ());
        update_fields (Sexp.List [ Sexp.Atom name; expr ] :: acc)
    | Some tok -> fail ("invalid record update field: " ^ token_name tok)
    | None -> fail "unterminated record update"
  in
  match peek st with
  | Some RBrace -> literal_fields []
  | _ ->
      let start = st.pos in
      let base = parse_expr st in
      (match peek st with
      | Some Pipe ->
          ignore (take st);
          Sexp.List (Sexp.Atom "recordUpdate" :: base :: update_fields [])
      | _ ->
          st.pos <- start;
          literal_fields [])

and parse_list_expr st =
  let rec elems acc =
    match peek st with
    | Some RBracket ->
        ignore (take st);
        list_literal (List.rev acc)
    | Some Comma ->
        ignore (take st);
        elems acc
    | Some _ ->
        let expr = parse_expr st in
        (match peek st with
        | Some Comma ->
            ignore (take st);
            elems (expr :: acc)
        | Some RBracket -> elems (expr :: acc)
        | Some tok -> fail ("expected , or ], got " ^ token_name tok)
        | None -> fail "unterminated list expression")
    | None -> fail "unterminated list expression"
  in
  elems []

and parse_if_expr st =
  let then_index = find_if_then st.pos st in
  let else_index = find_if_else (then_index + 1) st in
  let end_index = find_expr_boundary (else_index + 1) st in
  let cond = parse_expr_slice st.pos then_index st in
  let if_true = parse_expr_slice (then_index + 1) else_index st in
  let if_false = parse_expr_slice (else_index + 1) end_index st in
  st.pos <- end_index;
  Sexp.List
    [
      Sexp.Atom "match";
      cond;
      Sexp.List [ Sexp.Atom "true"; if_true ];
      Sexp.List [ Sexp.Atom "false"; if_false ];
    ]

and parse_expr_slice start stop st =
  if stop <= start then fail "expected expression";
  let slice = Array.sub st.tokens start (stop - start) |> Array.to_list in
  let nested = stream slice in
  let expr = parse_expr nested in
  if not (at_end nested) then
    fail ("unexpected expression token: " ^ token_name nested.tokens.(nested.pos));
  expr

and find_if_then start st =
  let rec loop paren_depth if_depth i =
    if i >= Array.length st.tokens then fail "if expression missing then"
    else
      match st.tokens.(i) with
      | Ident "if" when paren_depth = 0 -> loop paren_depth (if_depth + 1) (i + 1)
      | Ident "then" when paren_depth = 0 && if_depth = 0 -> i
      | Ident "else" when paren_depth = 0 && if_depth > 0 ->
          loop paren_depth (if_depth - 1) (i + 1)
      | LParen | LBrace | LBracket -> loop (paren_depth + 1) if_depth (i + 1)
      | RParen | RBrace | RBracket -> loop (max 0 (paren_depth - 1)) if_depth (i + 1)
      | _ -> loop paren_depth if_depth (i + 1)
  in
  loop 0 0 start

and find_if_else start st =
  let rec loop paren_depth if_depth i =
    if i >= Array.length st.tokens then fail "if expression missing else"
    else
      match st.tokens.(i) with
      | Ident "if" when paren_depth = 0 -> loop paren_depth (if_depth + 1) (i + 1)
      | Ident "else" when paren_depth = 0 && if_depth = 0 -> i
      | Ident "else" when paren_depth = 0 && if_depth > 0 ->
          loop paren_depth (if_depth - 1) (i + 1)
      | LParen | LBrace | LBracket -> loop (paren_depth + 1) if_depth (i + 1)
      | RParen | RBrace | RBracket -> loop (max 0 (paren_depth - 1)) if_depth (i + 1)
      | _ -> loop paren_depth if_depth (i + 1)
  in
  loop 0 0 start

and find_expr_boundary start st =
  let rec loop depth i =
    if i >= Array.length st.tokens then i
    else
      match st.tokens.(i) with
      | RParen | RBrace | RBracket | Comma when depth = 0 -> i
      | LParen | LBrace | LBracket -> loop (depth + 1) (i + 1)
      | RParen | RBrace | RBracket -> loop (max 0 (depth - 1)) (i + 1)
      | _ -> loop depth (i + 1)
  in
  loop 0 start

and append_pipeline_arg stage arg =
  match stage with
  | Sexp.List (f :: args) -> Sexp.List (f :: args @ [ arg ])
  | f -> Sexp.List [ f; arg ]

let sexp text = Sexp.to_string text

let collect_block lines start first =
  let len = Array.length lines in
  let rec loop i acc =
    if i >= len then (String.concat "\n" (List.rev acc), i)
    else
      let raw = lines.(i) |> strip_line_comment in
      let trimmed = trim raw in
      if trimmed = "" then loop (i + 1) acc
      else if indentation raw > 0 || starts_with trimmed "|" || starts_with trimmed "," then
        loop (i + 1) (raw :: acc)
      else (String.concat "\n" (List.rev acc), i)
  in
  let acc = if trim first = "" then [] else [ trim first ] in
  loop start acc

let parse_type_decl text =
  let tokens = tokenize text in
  match tokens with
  | Ident "type" :: Ident "alias" :: Ident name :: rest ->
      let params, rhs =
        let rec split acc = function
          | Equals :: rhs -> (List.rev acc, rhs)
          | Ident p :: rest -> split (p :: acc) rest
          | tok :: _ -> fail ("invalid type alias header: " ^ token_name tok)
          | [] -> fail "type alias missing ="
        in
        split [] rest
      in
      Sexp.List
        (Sexp.Atom "type" :: Sexp.Atom name
        :: (if params = [] then [] else [ Sexp.List (List.map (fun p -> Sexp.Atom p) params) ])
        @ [ parse_type rhs ])
  | Ident "type" :: Ident name :: rest ->
      let params, rhs =
        let rec split acc = function
          | Equals :: rhs -> (List.rev acc, rhs)
          | Ident p :: rest -> split (p :: acc) rest
          | tok :: _ -> fail ("invalid type header: " ^ token_name tok)
          | [] -> fail "type declaration missing ="
        in
        split [] rest
      in
      let cases = split_variant_cases rhs |> List.map parse_variant_case in
      Sexp.List
        (Sexp.Atom "variant" :: Sexp.Atom name
        :: (if params = [] then [] else [ Sexp.List (Sexp.Atom "params" :: List.map (fun p -> Sexp.Atom p) params) ])
        @ cases)
  | _ -> fail "invalid type declaration"

let parse_exposing_clause text =
  let text = trim text in
  let len = String.length text in
  if len < 2 || text.[0] <> '(' || text.[len - 1] <> ')' then
    fail "exposing syntax is: exposing (name, ...)";
  let inner = trim (String.sub text 1 (len - 2)) in
  if String.equal inner ".." then None
  else
    let names =
      inner |> String.split_on_char ',' |> List.map trim |> List.filter (( <> ) "")
    in
    if names = [] then fail "exposing requires at least one name";
    List.iter
      (fun name ->
        if not (is_name name) then fail ("invalid exposing name: " ^ name))
      names;
    ensure_unique_names "exposing name" names;
    Some names

let parse_module_line line =
  let rest = trim (String.sub line 7 (String.length line - 7)) in
  match find_sub rest " exposing " with
  | None ->
      if not (is_name rest) then fail "module syntax is: module Name";
      [ Sexp.List [ Sexp.Atom "module"; Sexp.Atom rest ] ]
  | Some i ->
      let name = trim (String.sub rest 0 i) in
      if not (is_name name) then fail "module syntax is: module Name exposing (...)";
      let exposing =
        trim (String.sub rest (i + 10) (String.length rest - i - 10))
        |> parse_exposing_clause
      in
      Sexp.List [ Sexp.Atom "module"; Sexp.Atom name ]
      ::
      (match exposing with
      | None -> []
      | Some names -> [ Sexp.List (Sexp.Atom "export" :: List.map (fun n -> Sexp.Atom n) names) ])

let parse_import_line line =
  let rest = trim (String.sub line 7 (String.length line - 7)) in
  let path =
    match find_sub rest " exposing " with
    | None -> rest
    | Some i ->
        let path = trim (String.sub rest 0 i) in
        let exposing = trim (String.sub rest (i + 10) (String.length rest - i - 10)) in
        ignore (parse_exposing_clause exposing);
        path
  in
  if String.length path >= 2 && path.[0] = '"' && path.[String.length path - 1] = '"'
  then String.sub path 1 (String.length path - 2)
  else path

let looks_like input =
  input |> String.split_on_char '\n'
  |> List.exists (fun raw ->
         let line = trim (strip_line_comment raw) in
         line <> "" && not (starts_with line "(")
         &&
         (starts_with line "type " || starts_with line "module " || starts_with line "export "
        || starts_with line "import " || starts_with line "capabilities "
        || Option.is_some (signature_separator line) || Option.is_some (value_separator line)))

let to_sexp_source input =
  let lines = input |> String.split_on_char '\n' |> Array.of_list in
  let signatures = Hashtbl.create 16 in
  let forms = ref [] in
  let rec loop i =
    if i >= Array.length lines then ()
    else
      let raw = strip_line_comment lines.(i) in
      let line = trim raw in
      if line = "" then loop (i + 1)
      else if indentation raw > 0 then fail ("unexpected indented top-level line: " ^ line)
      else if starts_with line "module " then (
        forms := List.rev_append (parse_module_line line) !forms;
        loop (i + 1))
      else if starts_with line "export " then (
        match split_words line with
        | "export" :: names ->
            forms := Sexp.List (Sexp.Atom "export" :: List.map (fun n -> Sexp.Atom n) names) :: !forms;
            loop (i + 1)
        | _ -> fail "export syntax is: export name ...")
      else if starts_with line "import " then (
        let path = parse_import_line line in
        forms := Sexp.List [ Sexp.Atom "import"; Sexp.Str path ] :: !forms;
        loop (i + 1))
      else if starts_with line "capabilities " then (
        let caps =
          match split_words line with "capabilities" :: caps -> caps | _ -> []
        in
        forms := Sexp.List (Sexp.Atom "capabilities" :: List.map (fun c -> Sexp.Atom c) caps) :: !forms;
        loop (i + 1))
      else if starts_with line "type " then
        let block, next = collect_block lines (i + 1) line in
        forms := parse_type_decl block :: !forms;
        loop next
      else
        match signature_separator line with
        | Some (name, ty) ->
            Hashtbl.replace signatures name (parse_signature_type_text ty);
            loop (i + 1)
        | None -> (
            match value_separator line with
            | Some (name, params, first_body) ->
                let body, next = collect_block lines (i + 1) first_body in
                let signature =
                  match Hashtbl.find_opt signatures name with
                  | Some signature -> signature
                  | None -> plain_signature (infer_missing_value_type name params body)
                in
                let expr = parse_expr_text body |> wrap_lambdas params in
                forms :=
                  definition_form name signature.typ signature.capabilities expr :: !forms;
                loop next
            | None -> fail ("invalid Elm-like top-level line: " ^ line))
  in
  loop 0;
  !forms |> List.rev |> List.map Sexp.to_string |> String.concat "\n"
