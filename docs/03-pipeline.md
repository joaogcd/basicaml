# 3. The pipeline

A BASIC program goes through these stages:

1. **Lexer** (`Lexer.lex : string -> (token list, Error.t) result`) turns raw
   text into tokens. Keywords are uppercase; extra whitespace is dropped.
2. **Parser** (`Parser.parse_program : string list -> (program, Error.t)
   result`) turns one snippet of text into a `line` (`line number * command`).
   The final `program` is a `line list` **sorted by line number**, and
   duplicate line numbers are rejected.
3. **Execution** (`Machine.run`) walks the sorted program with a program
   counter, evaluating expressions via `Eval.eval_expr` and updating the
   environment.

Errors are always returned as `(..., Error.t) result`; the code never uses
`failwith` for user-facing failures. See [6. Error model](06-error-model.md).
