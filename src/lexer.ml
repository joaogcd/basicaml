open Token

let is_digit c = c >= '0' && c <= '9'

let is_letter c =
  (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')

let explode s = List.init (String.length s) (String.get s)

let rec read_digits acc = function
  | c :: rest when is_digit c ->
      read_digits (acc ^ String.make 1 c) rest
  | rest -> (acc, rest)

let read_number acc chars =
  let int_part, rest = read_digits acc chars in
  match rest with
  | '.' :: rest' ->
      let frac_part, rest'' = read_digits "" rest' in
      if frac_part = "" then (int_part, rest)
      else (int_part ^ "." ^ frac_part, rest'')
  | _ -> (int_part, rest)

let rec read_ident acc = function
  | c :: rest when is_letter c ->
      read_ident (acc ^ String.make 1 c) rest
  | rest -> (acc, rest)

let rec read_string acc = function
  | '"' :: rest -> Some (acc, rest)
  | c :: rest -> read_string (c :: acc) rest
  | [] -> None

let keyword_or_ident = function
  | "LET" -> LET
  | "PRINT" -> PRINT
  | "IF" -> IF
  | "THEN" -> THEN
  | "ELSE" -> ELSE
  | "GOTO" -> GOTO
  | "END" -> END
  | "INPUT" -> INPUT
  | "FOR" -> FOR
  | "TO" -> TO
  | "STEP" -> STEP
  | "NEXT" -> NEXT
  | "GOSUB" -> GOSUB
  | "RETURN" -> RETURN
  | "DIM" -> DIM
  | "AND" -> AND
  | "OR" -> OR
  | "NOT" -> NOT
  | s -> IDENT s

(* main lexer *)
let rec tokenize acc = function
  | [] -> Ok (List.rev acc)
  | ' ' :: rest -> tokenize acc rest
  | ',' :: rest -> tokenize (COMMA :: acc) rest
  | '+' :: rest -> tokenize (PLUS :: acc) rest
  | '-' :: rest -> tokenize (MINUS :: acc) rest
  | '*' :: rest -> tokenize (STAR :: acc) rest
  | '/' :: rest -> tokenize (SLASH :: acc) rest
  | '(' :: rest -> tokenize (LPAREN :: acc) rest
  | ')' :: rest -> tokenize (RPAREN :: acc) rest
  | '"' :: rest ->
      begin match read_string [] rest with
      | Some (chars, rest') ->
          tokenize (STRING (String.of_seq (List.to_seq (List.rev chars))) :: acc) rest'
      | None ->
          Error (Error.make ~kind:Error.Lex_error "unterminated string literal")
      end
  | '=' :: rest -> tokenize (EQUAL :: acc) rest
  | '<' :: '=' :: rest -> tokenize (LE :: acc) rest
  | '<' :: '>' :: rest -> tokenize (NE :: acc) rest
  | '<' :: rest -> tokenize (LT :: acc) rest
  | '>' :: '=' :: rest -> tokenize (GE :: acc) rest
  | '>' :: rest -> tokenize (GT :: acc) rest
  | c :: rest when is_digit c ->
      let (n, rest') = read_number (String.make 1 c) rest in
      if String.contains n '.' then
        tokenize (FLOAT (float_of_string n) :: acc) rest'
      else
        tokenize (INT (int_of_string n) :: acc) rest'
  | c :: rest when is_letter c ->
      let (s, rest') = read_ident (String.make 1 c) rest in
      tokenize (keyword_or_ident s :: acc) rest'
  | c :: _ ->
      Error (Error.make ~kind:Error.Lex_error
        (Printf.sprintf "invalid character '%c'" c))


let lex s = tokenize [] (explode s)
