# 9. Testing

Alcotest suite distributed across multiple modules, configured in `test/dune`
as an executable. Each module exports a `tests` list that is aggregated by
`test/main.ml`.

Test structure:

```
test/main.ml               Aggregates all suites and runs Alcotest.run
test/test_helpers.ml       Shared helper functions
test/test_lexer.ml         Tokenization tests (success + errors)
test/test_parser.ml        Parse tests (success + errors)
test/test_eval.ml          Expression evaluation tests (success + errors)
test/test_machine.ml       Program execution tests (success + errors)
test/test_repl.ml          REPL behavior tests
```

### Helpers (`test_helpers.ml`)

- `string_of_token` / `string_of_tokens` — to assert lexer output;
- `string_of_value` — like `Eval.string_of_value` but renders arrays as
  `[0;5;0]`;
- `parse_lines` — parses a `string list` and `Alcotest.fail`s on error;
- `run_program ~input ~output lines` — parses + runs, returns
  `(Env.t, Error.t) result`;
- `eval_in` / `eval_expr_str` — evaluate an expression string against an env
  (parses `10 LET X = <s>` and extracts the expression).

To run only one suite:

```sh
dune exec ./test/main.exe -- test repl
```

(`dune test` passes extra args after `--` to the test binaries; Alcotest
accepts section filters.)

Use `Printer.string_of_program` (from the library) for asserted program
texts, not a local re-implementation.
