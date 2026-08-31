let () =
  Alcotest.run "basicaml"
    ( Test_lexer.tests
    @ Test_parser.tests
    @ Test_eval.tests
    @ Test_machine.tests
    @ Test_repl.tests )
