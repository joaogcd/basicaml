# 2. Project layout

```
dune-project                 Dune project config (lang 3.18, name basicaml)
basicaml.opam                Package metadata and dependencies
src/dune                     Library `basicaml` + executable `main`
src/error.ml   (+ .mli)     Typed, line-aware error type
src/token.ml   (+ .mli)     Token definitions
src/lexer.ml   (+ .mli)     Text  -> tokens
src/ast.ml     (+ .mli)     Abstract syntax tree
src/parser.ml  (+ .mli)     Tokens -> AST, program assembly
src/env.ml     (+ .mli)     Immutable variable environment
src/eval.ml    (+ .mli)     Expression evaluation
src/machine.ml (+ .mli)     Program execution (the VM)
src/printer.ml (+ .mli)     AST pretty-printing
src/repl.ml    (+ .mli)     Interactive loop
src/main.ml                CLI entry point
test/dune                   Test build configuration
test/main.ml               Aggregates and runs all test suites
test/test_helpers.ml       Shared helper functions for tests
test/test_lexer.ml         Lexer tests
test/test_parser.ml        Parser tests
test/test_eval.ml          Eval tests
test/test_machine.ml       Machine tests
test/test_repl.ml          REPL tests
examples/*.bas              Sample BASIC programs
```

Dependency direction (bottom layers know nothing about top layers):

```
main
  -> repl
     -> printer, machine, parser, lexer, env, error
machine -> eval -> env, ast
parser  -> lexer -> token, error
printer -> eval, ast
```

There are no dependency cycles; if you add a module, keep it acyclic.
