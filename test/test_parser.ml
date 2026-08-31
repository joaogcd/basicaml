open Alcotest
open Basicaml
open Test_helpers

let parser_valid =
  let check_program input expected =
    test_case input `Quick (fun () ->
      let prog = parse_lines [ input ] in
      check string "program" expected (Printer.string_of_program prog))
  in
  [ check_program "10 LET X = 10" "10: LET X = 10"
  ; check_program "20 PRINT X + 1" "20: PRINT (X + 1)"
  ; check_program "30 GOTO 10" "30: GOTO 10"
  ; check_program "40 IF X THEN GOTO 20" "40: IF X THEN GOTO 20"
  ; check_program "40 IF X = 5 THEN GOTO 20" "40: IF (X = 5) THEN GOTO 20"
  ; check_program "50 END" "50: END"
  ; check_program "10 LET X = 2 + 3 * 4" "10: LET X = (2 + (3 * 4))"
  ; check_program "10 LET X = (2 + 3) * 4" "10: LET X = ((2 + 3) * 4)"
  ; check_program "10 LET X = 2 * 3 / 4" "10: LET X = ((2 * 3) / 4)"
  ; check_program "10 LET X = 5 > 3" "10: LET X = (5 > 3)"
  ; check_program "10 LET X = NOT X = Y" "10: LET X = (NOT (X = Y))"
  ; check_program "10 LET X = -X * 2" "10: LET X = ((-X) * 2)"
  ; check_program "10 LET X = 1.5 + 2.5" "10: LET X = (1.5 + 2.5)"
  ; test_case "unsorted lines are sorted" `Quick (fun () ->
        let prog = parse_lines [ "30 END"; "10 LET X = 1"; "20 PRINT X" ] in
        check string "program" "10: LET X = 1\n20: PRINT X\n30: END"
          (Printer.string_of_program prog))
  ; test_case "blank lines are skipped" `Quick (fun () ->
        let prog = parse_lines [ "10 END"; ""; "20 END" ] in
        check string "program" "10: END\n20: END" (Printer.string_of_program prog))
  ; check_program "10 PRINT \"hello\"" "10: PRINT \"hello\""
  ; check_program "10 LET X = \"abc\"" "10: LET X = \"abc\""
  ; check_program "10 INPUT A" "10: INPUT A"
  ; check_program "10 PRINT \"X=\" + X" "10: PRINT (\"X=\" + X)"
  ; check_program "10 LET X = \"a\" + \"b\"" "10: LET X = (\"a\" + \"b\")"
  ; check_program "10 FOR X = 1 TO 5" "10: FOR X = 1 TO 5"
  ; check_program "10 FOR X = 1 TO 5 STEP 2" "10: FOR X = 1 TO 5 STEP 2"
  ; check_program "10 FOR X = 1 TO 5 STEP -1" "10: FOR X = 1 TO 5 STEP (-1)"
  ; check_program "20 NEXT" "20: NEXT"
  ; check_program "20 NEXT X" "20: NEXT X"
  ; check_program "30 GOSUB 100" "30: GOSUB 100"
  ; check_program "40 RETURN" "40: RETURN"
  ; check_program "40 IF X THEN GOTO 20" "40: IF X THEN GOTO 20"
  ; check_program "40 IF X > 5 THEN PRINT \"hi\" ELSE PRINT \"lo\""
      "40: IF (X > 5) THEN PRINT \"hi\" ELSE PRINT \"lo\""
  ; check_program "40 IF X THEN LET Y = 1 ELSE END" "40: IF X THEN LET Y = 1 ELSE END"
  ; check_program "10 REM anything #@! 1+2" "10: REM"
  ; check_program "10 REM" "10: REM"
  ; test_case "REM line keeps its number and sorts" `Quick (fun () ->
        let prog = parse_lines [ "10 REM first"; "20 END"; "15 REM middle" ] in
        check string "program" "10: REM\n15: REM\n20: END" (Printer.string_of_program prog))
  ; check_program "10 DIM A(10)" "10: DIM A(10)"
  ; check_program "10 DIM A(2 + 3)" "10: DIM A((2 + 3))"
  ; check_program "10 LET A(0) = 5" "10: LET A(0) = 5"
  ; check_program "10 LET A(I) = X + 1" "10: LET A(I) = (X + 1)"
  ; check_program "10 LET X = ABS(-5)" "10: LET X = ABS((-5))"
  ; check_program "10 LET X = INT(-2.7)" "10: LET X = INT((-2.7))"
  ; check_program "10 LET X = MOD(7, 3)" "10: LET X = MOD(7, 3)"
  ; check_program "10 LET X = LEN(\"abc\")" "10: LET X = LEN(\"abc\")"
  ; check_program "10 LET X = A(0) + B(1)" "10: LET X = (A(0) + B(1))"
  ; check_program "10 LET X = ABS(A(1))" "10: LET X = ABS(A(1))"
  ]

let parser_errors =
  [ test_case "line without line number" `Quick (fun () ->
        match Parser.parse_program [ "LET X = 1" ] with
        | Error err ->
            check (option int) "no line known" None err.Error.line
        | Ok _ -> Alcotest.fail "expected parse to fail")
  ; test_case "trailing tokens rejected" `Quick (fun () ->
        match Parser.parse_program [ "10 LET X = 1 2" ] with
        | Error err ->
            check (option int) "line attached" (Some 10) err.Error.line;
            (match err.Error.kind with
             | Error.Parse_error -> ()
             | _ -> Alcotest.fail "expected a syntax error")
        | Ok _ -> Alcotest.fail "expected parse to fail")
  ; test_case "invalid character carries line number" `Quick (fun () ->
        match Parser.parse_program [ "10 LET X = $5" ] with
        | Error err ->
            check (option int) "line attached" (Some 10) err.Error.line;
            (match err.Error.kind with
             | Error.Lex_error -> ()
             | _ -> Alcotest.fail "expected a lexical error")
        | Ok _ -> Alcotest.fail "expected parse to fail")
  ; test_case "duplicate line numbers rejected" `Quick (fun () ->
        match Parser.parse_program [ "10 END"; "10 END" ] with
        | Error err ->
            check (option int) "line attached" (Some 10) err.Error.line
        | Ok _ -> Alcotest.fail "expected parse to fail")
  ; test_case "IF without THEN GOTO" `Quick (fun () ->
        match Parser.parse_program [ "10 IF X PRINT" ] with
        | Error err -> check (option int) "line attached" (Some 10) err.Error.line
        | Ok _ -> Alcotest.fail "expected parse to fail")
  ; test_case "unclosed parenthesis" `Quick (fun () ->
        match Parser.parse_program [ "10 LET X = (2 + 3" ] with
        | Error err ->
            check (option int) "line attached" (Some 10) err.Error.line;
            (match err.Error.kind with
             | Error.Parse_error -> ()
             | _ -> Alcotest.fail "expected a syntax error")
        | Ok _ -> Alcotest.fail "expected parse to fail")
  ; test_case "INPUT with trailing tokens" `Quick (fun () ->
        match Parser.parse_program [ "10 INPUT A B" ] with
        | Error err ->
            check (option int) "line attached" (Some 10) err.Error.line;
            (match err.Error.kind with
             | Error.Parse_error -> ()
             | _ -> Alcotest.fail "expected a syntax error")
        | Ok _ -> Alcotest.fail "expected parse to fail")
  ; test_case "FOR without TO" `Quick (fun () ->
        match Parser.parse_program [ "10 FOR X = 1 5" ] with
        | Error err ->
            check (option int) "line attached" (Some 10) err.Error.line;
            (match err.Error.kind with
             | Error.Parse_error -> ()
             | _ -> Alcotest.fail "expected a syntax error")
        | Ok _ -> Alcotest.fail "expected parse to fail")
  ; test_case "FOR without variable" `Quick (fun () ->
        match Parser.parse_program [ "10 FOR 1 TO 5" ] with
        | Error err ->
            check (option int) "line attached" (Some 10) err.Error.line;
            (match err.Error.kind with
             | Error.Parse_error -> ()
             | _ -> Alcotest.fail "expected a syntax error")
        | Ok _ -> Alcotest.fail "expected parse to fail")
  ; test_case "GOSUB without line number" `Quick (fun () ->
        match Parser.parse_program [ "10 GOSUB ABC" ] with
        | Error err ->
            check (option int) "line attached" (Some 10) err.Error.line;
            (match err.Error.kind with
             | Error.Parse_error -> ()
             | _ -> Alcotest.fail "expected a syntax error")
        | Ok _ -> Alcotest.fail "expected parse to fail")
  ; test_case "NEXT with trailing tokens" `Quick (fun () ->
        match Parser.parse_program [ "10 NEXT X Y" ] with
        | Error err ->
            check (option int) "line attached" (Some 10) err.Error.line;
            (match err.Error.kind with
             | Error.Parse_error -> ()
             | _ -> Alcotest.fail "expected a syntax error")
        | Ok _ -> Alcotest.fail "expected parse to fail")
  ; test_case "REMX is not a comment" `Quick (fun () ->
        match Parser.parse_program [ "10 REMX = 1" ] with
        | Error err -> check (option int) "line attached" (Some 10) err.Error.line
        | Ok _ -> Alcotest.fail "expected parse to fail")
  ; test_case "DIM without size" `Quick (fun () ->
        match Parser.parse_program [ "10 DIM A" ] with
        | Error err ->
            check (option int) "line attached" (Some 10) err.Error.line;
            (match err.Error.kind with
             | Error.Parse_error -> ()
             | _ -> Alcotest.fail "expected a syntax error")
        | Ok _ -> Alcotest.fail "expected parse to fail")
  ; test_case "LET array element missing =" `Quick (fun () ->
        match Parser.parse_program [ "10 LET A(0) 5" ] with
        | Error err ->
            check (option int) "line attached" (Some 10) err.Error.line;
            (match err.Error.kind with
             | Error.Parse_error -> ()
             | _ -> Alcotest.fail "expected a syntax error")
        | Ok _ -> Alcotest.fail "expected parse to fail")
  ]

let tests = [ "parser", parser_valid @ parser_errors ]
