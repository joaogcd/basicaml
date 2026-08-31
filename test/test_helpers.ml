open Basicaml

let string_of_token = function
  | Token.LET -> "LET"
  | Token.PRINT -> "PRINT"
  | Token.IF -> "IF"
  | Token.THEN -> "THEN"
  | Token.ELSE -> "ELSE"
  | Token.GOTO -> "GOTO"
  | Token.END -> "END"
  | Token.INPUT -> "INPUT"
  | Token.FOR -> "FOR"
  | Token.TO -> "TO"
  | Token.STEP -> "STEP"
  | Token.NEXT -> "NEXT"
  | Token.GOSUB -> "GOSUB"
  | Token.RETURN -> "RETURN"
  | Token.DIM -> "DIM"
  | Token.AND -> "AND"
  | Token.OR -> "OR"
  | Token.NOT -> "NOT"
  | Token.IDENT s -> "IDENT(" ^ s ^ ")"
  | Token.STRING s -> "STRING(" ^ s ^ ")"
  | Token.INT n -> "INT(" ^ string_of_int n ^ ")"
  | Token.FLOAT f -> "FLOAT(" ^ string_of_float f ^ ")"
  | Token.PLUS -> "PLUS"
  | Token.MINUS -> "MINUS"
  | Token.STAR -> "STAR"
  | Token.SLASH -> "SLASH"
  | Token.EQUAL -> "EQUAL"
  | Token.LT -> "LT"
  | Token.GT -> "GT"
  | Token.LE -> "LE"
  | Token.GE -> "GE"
  | Token.NE -> "NE"
  | Token.LPAREN -> "LPAREN"
  | Token.RPAREN -> "RPAREN"
  | Token.COMMA -> "COMMA"

let string_of_tokens tokens = List.map string_of_token tokens

let string_of_value = function
  | Ast.Number n -> Machine.string_of_number n
  | Ast.String s -> s
  | Ast.Array a ->
      "[" ^ String.concat ";" (List.map Machine.string_of_number (Array.to_list a)) ^ "]"

let parse_lines lines =
  match Parser.parse_program (List.map String.trim lines) with
  | Ok p -> p
  | Error err -> Alcotest.fail ("parse failed: " ^ Error.to_string err)

let run_program ?input ?output lines =
  let prog = parse_lines lines in
  Machine.run ?input ?output prog Env.empty (fst (List.hd prog))
  |> Result.map Machine.env

let eval_in env expr =
  match Eval.eval_expr env expr with
  | Ok value -> value
  | Error err -> Alcotest.fail ("unexpected eval error: " ^ Error.to_string err)

let eval_expr_str env s =
  match Parser.parse_program [ "10 LET X = " ^ s ] with
  | Error err -> Alcotest.fail ("parse failed: " ^ Error.to_string err)
  | Ok ((_, Ast.Let (Ast.Var _, e)) :: _) -> eval_in env e
  | Ok _ -> Alcotest.fail "expected LET"
