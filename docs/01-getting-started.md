# 1. Getting started

Requirements: OCaml >= 4.14, Dune >= 3.18.

```sh
dune build                          # compile library + executable
dune test                           # run the Alcotest suite
dune test --force                   # run the suite, ignoring cached results
dune exec basicaml -- program.bas   # run a program file
dune exec basicaml                  # start the interactive REPL
```

The build profile treats warnings as errors (the default `dev` profile), and
the code relies on this: a non-exhaustive `match` that would compile with a
warning **breaks the build**. Whenever you extend a sum type (for example
`value`, `expr`, `command`), update every `match` that consumes it.
