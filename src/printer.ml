open Ast

let string_of_binop = function
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Eq -> "="
  | Ne -> "<>"
  | Lt -> "<"
  | Le -> "<="
  | Gt -> ">"
  | Ge -> ">="
  | And -> "AND"
  | Or -> "OR"

let string_of_unop = function
  | Neg -> "-"
  | Not -> "NOT "

let rec string_of_expr = function
  | Const f -> Eval.string_of_number f
  | Str s -> "\"" ^ s ^ "\""
  | Var s -> s
  | ArrayAccess (name, idx) -> name ^ "(" ^ string_of_expr idx ^ ")"
  | Call (name, args) ->
      name ^ "(" ^ String.concat ", " (List.map string_of_expr args) ^ ")"
  | Binop (op, a, b) ->
      "(" ^ string_of_expr a ^ " " ^ string_of_binop op ^ " " ^ string_of_expr b ^ ")"
  | Unop (op, a) -> "(" ^ string_of_unop op ^ string_of_expr a ^ ")"

let string_of_target = function
  | Var s -> s
  | ArrayElement (name, idx) -> name ^ "(" ^ string_of_expr idx ^ ")"

let rec string_of_command = function
  | Let (target, e) -> "LET " ^ string_of_target target ^ " = " ^ string_of_expr e
  | Print e -> "PRINT " ^ string_of_expr e
  | Goto n -> "GOTO " ^ string_of_int n
  | If (e, then_cmd, else_cmd) ->
      "IF " ^ string_of_expr e ^ " THEN " ^ string_of_command then_cmd
      ^ (match else_cmd with
        | Some c -> " ELSE " ^ string_of_command c
        | None -> "")
  | Input v -> "INPUT " ^ v
  | For (v, start, limit, step) ->
      "FOR " ^ v ^ " = " ^ string_of_expr start ^ " TO " ^ string_of_expr limit
      ^ (match step with
        | Some s -> " STEP " ^ string_of_expr s
        | None -> "")
  | Next None -> "NEXT"
  | Next (Some v) -> "NEXT " ^ v
  | Gosub n -> "GOSUB " ^ string_of_int n
  | Return -> "RETURN"
  | Dim (v, size) -> "DIM " ^ v ^ "(" ^ string_of_expr size ^ ")"
  | Rem -> "REM"
  | End -> "END"

let string_of_line (n, c) = string_of_int n ^ ": " ^ string_of_command c

let string_of_program p = String.concat "\n" (List.map string_of_line p)