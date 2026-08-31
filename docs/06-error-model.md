# 6. Error model

- Every fallible function returns a `result`; there is no `failwith` in the
  user-facing path. Parse and lexical errors originate in `parser.ml` /
  `lexer.ml`; runtime errors originate in `eval.ml` / `machine.ml`.
- The line number is attached as early as possible and `Error.to_string`
  formats it. `machine.ml` wraps runtime errors with
  `Error.with_line line err` where a line is in scope. `parse_line` attaches
  the source line to parse/lex errors (added in Phase 6); `parse_program`
  re-attaches (idempotent, same number).
- Presentation: runtime errors triggered by a command that flows through an
  `IF ... THEN` body keep the *enclosing* line because `eval_command`
  recurses with the same `line`.
- When you add a failure path, prefer `type_error op` / `error msg` in
  `eval.ml` or the local `error` in `machine.ml` — never raise.
