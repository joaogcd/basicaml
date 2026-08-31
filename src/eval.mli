val string_of_number : float -> string

val string_of_value : Ast.value -> string

val eval_expr : Env.t -> Ast.expr -> (Ast.value, Error.t) result

val number_of : string -> Ast.value -> (float, Error.t) result

val element_index : string -> float -> int -> (int, Error.t) result

val type_error : string -> Error.t