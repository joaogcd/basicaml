
type kind =
  | Lex_error
  | Parse_error
  | Runtime_error

type t =
  { kind : kind
  ; message : string
  ; line : int option
  }

let make ~kind message = { kind; message; line = None }

let with_line line err = { err with line = Some line }

let kind_to_string = function
  | Lex_error -> "lexical error"
  | Parse_error -> "syntax error"
  | Runtime_error -> "runtime error"

let to_string { kind; message; line } =
  match line with
  | Some n -> Printf.sprintf "Line %d: %s: %s" n (kind_to_string kind) message
  | None -> Printf.sprintf "%s: %s" (kind_to_string kind) message
