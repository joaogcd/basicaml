# Basicaml

A minimalist BASIC interpreter implemented in OCaml for educational purposes.

Basicaml demonstrates a layered interpreter pipeline — tokenization, parsing, a
typed AST, expression evaluation and a stack-based execution machine — with
line-aware error reporting across all stages.

## Features

- **Statements:** `LET`, `PRINT`, `GOTO`, `IF ... THEN ... ELSE`,
  `FOR ... TO ... [STEP] ... NEXT`, `GOSUB`/`RETURN`, `INPUT`, `DIM`, `REM`,
  `END`.
- **Expressions:** arithmetic (`+ - * /`), comparisons (`= <> < <= > >=`),
  boolean logic (`AND OR NOT`), unary negation and `NOT`, parentheses.
- **Strings:** string literals, string/number concatenation with `+`.
- **Built-in functions:** `ABS(x)`, `INT(x)`, `RND(x)`, `LEN(s)`,
  `MOD(a, b)`.
- **Arrays:** one-dimensional numeric arrays via `DIM A(n)` (0-based).
- **Error reporting:** typed (`Lex_error`, `Parse_error`, `Runtime_error`),
  line-aware error messages at every stage.
- **REPL:** interactive loop with immediate execution, program storage, and
  `LIST`/`RUN`/`CLEAR`/`NEW`/`QUIT`/`HELP` commands.
- **File execution:** run `.bas` files directly from the command line.

## Requirements

- OCaml >= 4.14
- Dune >= 3.18

## Building and testing

```sh
dune build              # compile library + executable
dune test               # run the Alcotest test suite
dune test --force       # ignore cached results
```

## Usage

Run a `.bas` file:

```sh
dune exec basicaml -- program.bas
```

Start the interactive REPL:

```sh
dune exec basicaml
```

Other options:

```sh
dune exec basicaml -- --help
dune exec basicaml -- --version
```

### REPL commands

| Command | Description |
| --- | --- |
| `<number> <statement>` | Store a line in the program |
| `<number>` | Delete a line |
| `<statement>` | Run immediately (shares variables with stored program) |
| `RUN` | Execute the stored program |
| `LIST` | Print the stored program |
| `CLEAR` / `NEW` | Reset program and variables |
| `QUIT` / `BYE` / `EXIT` | Leave the REPL |
| `HELP` | Show help text |

Example session:

```
basicaml> 10 FOR I = 1 TO 3
basicaml> 20 PRINT I
basicaml> 30 NEXT I
basicaml> RUN
1
2
3
```

## Examples

The `examples/` directory contains sample programs:

| File | Description |
| --- | --- |
| `arithmetic.bas` | Basic arithmetic operations |
| `arrays.bas` | Array declaration and access |
| `countdown.bas` | Countdown loop with GOTO |
| `factorial.bas` | Recursive factorial with GOSUB/RETURN |
| `greet.bas` | User input and string concatenation |
| `ifelse.bas` | Conditional branching |
| `loops.bas` | FOR/NEXT loop patterns |

Run any example with:

```sh
dune exec basicaml -- examples/factorial.bas
```

## Project layout

```
src/
  token.ml        Token definitions
  lexer.ml        Text to tokens
  ast.ml          Abstract syntax tree
  parser.ml       Tokens to AST
  env.ml          Immutable variable environment
  eval.ml         Expression evaluation
  machine.ml      Program execution (the VM)
  printer.ml      AST pretty-printing
  repl.ml         Interactive loop
  error.ml        Typed, line-aware errors
  main.ml         CLI entry point

test/
  main.ml         Test runner (aggregates all suites)
  test_helpers.ml Shared test utilities
  test_lexer.ml   Lexer tests
  test_parser.ml  Parser tests
  test_eval.ml    Eval tests
  test_machine.ml Machine tests
  test_repl.ml    REPL tests

docs/             Documentation
examples/         Sample BASIC programs
```

See [docs/](docs/) for the full developer guide covering architecture, module details, conventions, and known limitations.

## License

This project is licensed under the MIT License. See [LICENSE](LICENSE) for details.
