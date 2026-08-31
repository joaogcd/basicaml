type token =
  | LET
  | PRINT
  | IF
  | THEN
  | ELSE
  | GOTO
  | END
  | INPUT
  | FOR
  | TO
  | STEP
  | NEXT
  | GOSUB
  | RETURN
  | DIM
  | AND
  | OR
  | NOT
  | IDENT of string
  | STRING of string
  | INT of int
  | FLOAT of float
  | COMMA
  | PLUS
  | MINUS
  | STAR
  | SLASH
  | EQUAL
  | LT
  | GT
  | LE
  | GE
  | NE
  | LPAREN
  | RPAREN