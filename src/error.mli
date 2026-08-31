type kind =
  | Lex_error
  | Parse_error
  | Runtime_error

type t =
  { kind : kind
  ; message : string
  ; line : int option
  }

val make : kind:kind -> string -> t

val with_line : int -> t -> t

val kind_to_string : kind -> string

val to_string : t -> string