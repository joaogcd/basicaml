# 8. The REPL

Design: "accumulate a program and `RUN` it", plus immediate execution of
unnumbered commands. All state lives in two refs created by `Repl.run`:

- `program : program ref` — always kept **sorted**, one `(number, command)`
  per line number (re-typing a line replaces it).
- `env : Env.t ref` — variables shared between immediate commands and stored
  programs.

Loop behavior per line (after trimming):

| Input | Action |
| --- | --- |
| (blank) | ignored |
| `QUIT` / `BYE` / `EXIT` | stop the loop |
| `RUN` | `Machine.run ~input ~output prog env first`; on success persist `Machine.env`; errors printed via `Error.to_string` |
| `LIST` | print `Printer.string_of_program !program` |
| `CLEAR` / `NEW` | reset program and env |
| `HELP` | print built-in help text |
| starts with a digit | program line: `Parser.parse_line`, insert/replace sorted |
| lone line number (empty rest) | **delete** that line |
| anything else | immediate command: lex + `Parser.parse_command`, run as one-line program `[(0, cmd)]` |

- `RUN` on an empty program prints "No program loaded." (not an error).
- Immediate execution cannot reference stored program line numbers (a
  one-line synthetic program is loaded; `GOTO 99` fails with
  "line 99 does not exist").
- When a stored program executes `INPUT`, it reads from the **same**
  `~input` hook as the REPL loop, which is why `Repl.run` forwards its
  `~input`/`~output` to `Machine.run`. In a terminal this behaves like classic
  BASIC (the next typed line goes to `INPUT`).
- The prompt is printed with `print_string` + `flush stdout` and only when
  `~prompt:(Some p)` is given (tests pass `None`/inject hooks to stay
  deterministic).
- Special commands are matched case-insensitively on the *first word* of the
  line (`String.uppercase_ascii`).
