open Alcotest
open Basicaml
open Test_helpers

let lex_tokens =
  let check_lex input expected =
    test_case ("tokens: " ^ input) `Quick (fun () ->
      match Lexer.lex input with
      | Ok tokens ->
          check (list string) "tokens" expected (string_of_tokens tokens)
      | Error err ->
          Alcotest.fail ("unexpected lex error: " ^ Error.to_string err))
  in
  [ check_lex "10 LET X = 10"
      [ "INT(10)"; "LET"; "IDENT(X)"; "EQUAL"; "INT(10)" ]
  ; check_lex "PRINT X + 1"
      [ "PRINT"; "IDENT(X)"; "PLUS"; "INT(1)" ]
  ; check_lex "5 - 3" [ "INT(5)"; "MINUS"; "INT(3)" ]
  ; check_lex "IF X THEN GOTO 20"
      [ "IF"; "IDENT(X)"; "THEN"; "GOTO"; "INT(20)" ]
  ; check_lex "END" [ "END" ]
  ; check_lex "" []
  ; check_lex "3.14" [ "FLOAT(3.14)" ]
  ; check_lex "10 <= 20" [ "INT(10)"; "LE"; "INT(20)" ]
  ; check_lex "10 >= 20" [ "INT(10)"; "GE"; "INT(20)" ]
  ; check_lex "10 <> 20" [ "INT(10)"; "NE"; "INT(20)" ]
  ; check_lex "5 < 3" [ "INT(5)"; "LT"; "INT(3)" ]
  ; check_lex "2 AND NOT 3" [ "INT(2)"; "AND"; "NOT"; "INT(3)" ]
  ; check_lex "X * (Y / 2)"
      [ "IDENT(X)"; "STAR"; "LPAREN"; "IDENT(Y)"; "SLASH"; "INT(2)"; "RPAREN" ]
  ; check_lex "PRINT \"hello\"" [ "PRINT"; "STRING(hello)" ]
  ; check_lex "PRINT \"\"" [ "PRINT"; "STRING()" ]
  ; check_lex "PRINT \"a b c\"" [ "PRINT"; "STRING(a b c)" ]
  ; check_lex "PRINT \"2 + 2 = 4\"" [ "PRINT"; "STRING(2 + 2 = 4)" ]
  ; check_lex "INPUT X" [ "INPUT"; "IDENT(X)" ]
  ; check_lex "FOR X = 1 TO 5"
      [ "FOR"; "IDENT(X)"; "EQUAL"; "INT(1)"; "TO"; "INT(5)" ]
  ; check_lex "10 FOR X = 1 TO 5 STEP 2"
      [ "INT(10)"; "FOR"; "IDENT(X)"; "EQUAL"; "INT(1)"; "TO"; "INT(5)"; "STEP"; "INT(2)" ]
  ; check_lex "NEXT X" [ "NEXT"; "IDENT(X)" ]
  ; check_lex "NEXT" [ "NEXT" ]
  ; check_lex "GOSUB 100" [ "GOSUB"; "INT(100)" ]
  ; check_lex "RETURN" [ "RETURN" ]
  ; check_lex "DIM A(10)"
      [ "DIM"; "IDENT(A)"; "LPAREN"; "INT(10)"; "RPAREN" ]
  ; check_lex "MOD(A, 2)"
      [ "IDENT(MOD)"; "LPAREN"; "IDENT(A)"; "COMMA"; "INT(2)"; "RPAREN" ]
  ; check_lex "LEN(\"abc\")"
      [ "IDENT(LEN)"; "LPAREN"; "STRING(abc)"; "RPAREN" ]
  ; check_lex "IF X THEN PRINT 1 ELSE PRINT 2"
      [ "IF"; "IDENT(X)"; "THEN"; "PRINT"; "INT(1)"; "ELSE"; "PRINT"; "INT(2)" ]
  ]

let lex_errors =
  [ test_case "invalid character" `Quick (fun () ->
        match Lexer.lex "10 LET @ = 1" with
        | Error err ->
            check (option int) "line is unknown at lex time" None err.Error.line;
            (match err.Error.kind with
             | Error.Lex_error -> ()
             | _ -> Alcotest.fail "expected a lexical error")
        | Ok _ -> Alcotest.fail "expected lex to fail")
  ; test_case "unterminated string literal" `Quick (fun () ->
        match Lexer.lex "PRINT \"hello" with
        | Error err ->
            (match err.Error.kind with
             | Error.Lex_error -> ()
             | _ -> Alcotest.fail "expected a lexical error")
        | Ok _ -> Alcotest.fail "expected lex to fail")
  ]

let tests = [ "lexer", lex_tokens @ lex_errors ]
