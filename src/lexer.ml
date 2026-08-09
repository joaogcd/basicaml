open Token

let is_digit c = c >= '0' && c <= '9'
let is_letter c =
  (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')

let explode s = List.init (String.length s) (String.get s)

let rec read_number acc = function
  | c :: rest when is_digit c ->
      read_number (acc ^ String.make 1 c) rest
  | rest -> (int_of_string acc, rest)

let rec read_ident acc = function
  | c :: rest when is_letter c ->
      read_ident (acc ^ String.make 1 c) rest
  | rest -> (acc, rest)

let keyword_or_ident = function
  | "LET" -> LET
  | "PRINT" -> PRINT
  | "IF" -> IF
  | "THEN" -> THEN
  | "GOTO" -> GOTO
  | "END" -> END
  | s -> IDENT s


(* main lexer *)
let rec tokenize = function
  | [] -> []
  | ' ' :: rest -> tokenize rest
  | '+' :: rest -> PLUS :: tokenize rest
  | '-' :: rest -> MINUS :: tokenize rest
  | '=' :: rest -> EQUAL :: tokenize rest
  | c :: rest when is_digit c ->
      let (n, rest') = read_number (String.make 1 c) rest in
      INT n :: tokenize rest'
  | c :: rest when is_letter c ->
      let (s, rest') = read_ident (String.make 1 c) rest in
      keyword_or_ident s :: tokenize rest'
  | _ -> failwith "Invalid character"


let lex s = tokenize (explode s)