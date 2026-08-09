

type token =
  | LET
  | PRINT
  | IF
  | THEN
  | GOTO
  | END
  | IDENT of string
  | INT of int
  | PLUS
  | MINUS
  | EQUAL
