open Token
open Ast

let parse_term = function
  | INT n :: rest -> (Const n, rest)
  | IDENT s :: rest -> (Var s, rest)
  | _ -> failwith "Expected term"

let rec parse_expr tokens =
  let (left, rest) = parse_term tokens in
  parse_expr_tail left rest

and parse_expr_tail left = function
  | PLUS :: rest ->
      let (right, rest') = parse_term rest in
      parse_expr_tail (Add (left, right)) rest'
  | MINUS :: rest ->
      let (right, rest') = parse_term rest in
      parse_expr_tail (Sub (left, right)) rest'
  | rest -> (left, rest)



let parse_command = function
  | LET :: IDENT v :: EQUAL :: rest ->
      let (e, _) = parse_expr rest in
      Let (v, e)
  | PRINT :: rest ->
      let (e, _) = parse_expr rest in
      Print e
  | GOTO :: INT n :: _ -> Goto n
  | IF :: rest ->
      let (e, rest') = parse_expr rest in
      begin match rest' with
      | THEN :: GOTO :: INT n :: _ -> If (e, n)
      | _ -> failwith "IF syntax error"
      end
  | END :: _ -> End
  | _ -> failwith "Invalid command"

let parse_line line =
  match Lexer.lex line with
  | INT n :: rest -> (n, parse_command rest)
  | _ -> failwith "Line must start with line number"

let parse_program lines =
  List.map parse_line lines
