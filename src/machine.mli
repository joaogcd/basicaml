type state

val run
  :  ?input:(unit -> string option)
  -> ?output:(string -> unit)
  -> Ast.program
  -> Env.t
  -> int
  -> (state, Error.t) result

val env : state -> Env.t

val string_of_number : float -> string

val string_of_value : Ast.value -> string