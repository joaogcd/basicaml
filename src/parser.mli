val parse_program : string list -> (Ast.program, Error.t) result

val parse_line : string -> (Ast.line, Error.t) result

val parse_command : Token.token list -> (Ast.command, Error.t) result

val line_number_of_string : string -> int option