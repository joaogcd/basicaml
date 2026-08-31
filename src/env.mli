type t

val empty : t

val lookup : string -> t -> Ast.value option

val update : string -> Ast.value -> t -> t