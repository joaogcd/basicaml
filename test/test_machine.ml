open Alcotest
open Basicaml
open Test_helpers

let machine_ok =
  [ test_case "countdown ends with X = 0" `Quick (fun () ->
        let result = run_program
            [ "10 LET X = 3"
            ; "20 LET X = X - 1"
            ; "30 IF X THEN GOTO 20"
            ; "40 END"
            ] in
        match result with
        | Ok env ->
            check (option string) "X" (Some "0")
              (Option.map string_of_value (Env.lookup "X" env))
        | Error err -> Alcotest.fail ("unexpected runtime error: " ^ Error.to_string err))
  ; test_case "program with no END terminates" `Quick (fun () ->
        match run_program [ "10 LET X = 7" ] with
        | Ok env ->
            check (option string) "X" (Some "7")
              (Option.map string_of_value (Env.lookup "X" env))
        | Error err -> Alcotest.fail ("unexpected runtime error: " ^ Error.to_string err))
  ; test_case "loop until X reaches 10" `Quick (fun () ->
        let result = run_program
            [ "10 LET X = 0"
            ; "20 LET X = X + 2"
            ; "30 IF X < 10 THEN GOTO 20"
            ; "40 PRINT X"
            ; "50 END"
            ] in
        match result with
        | Ok env ->
            check (option string) "X" (Some "10")
              (Option.map string_of_value (Env.lookup "X" env))
        | Error err -> Alcotest.fail ("unexpected runtime error: " ^ Error.to_string err))
  ; test_case "IF with equality jumps" `Quick (fun () ->
        let result = run_program
            [ "10 LET X = 5"
            ; "20 IF X = 5 THEN GOTO 50"
            ; "30 LET X = 99"
            ; "50 END"
            ] in
        match result with
        | Ok env ->
            check (option string) "X" (Some "5")
              (Option.map string_of_value (Env.lookup "X" env))
        | Error err -> Alcotest.fail ("unexpected runtime error: " ^ Error.to_string err))
  ; test_case "float division in program" `Quick (fun () ->
        match run_program [ "10 LET X = 7 / 2" ] with
        | Ok env ->
            check (option string) "X" (Some "3.5")
              (Option.map string_of_value (Env.lookup "X" env))
        | Error err -> Alcotest.fail ("unexpected runtime error: " ^ Error.to_string err))
  ; test_case "PRINT mixed string and number" `Quick (fun () ->
        let output = ref [] in
        match run_program
            ~output:(fun s -> output := s :: !output)
            [ "10 LET X = 5"
            ; "20 PRINT \"X=\" + X"
            ; "30 END"
            ] with
        | Ok _ ->
            check (list string) "output" [ "X=5" ] (List.rev !output)
        | Error err -> Alcotest.fail ("unexpected runtime error: " ^ Error.to_string err))
  ; test_case "INPUT numeric" `Quick (fun () ->
        let result = run_program
            ~input:(fun () -> Some "42")
            [ "10 INPUT X"
            ; "20 END"
            ] in
        match result with
        | Ok env ->
            check (option string) "X" (Some "42")
              (Option.map string_of_value (Env.lookup "X" env))
        | Error err -> Alcotest.fail ("unexpected runtime error: " ^ Error.to_string err))
  ; test_case "INPUT string" `Quick (fun () ->
        let result = run_program
            ~input:(fun () -> Some "hello")
            [ "10 INPUT X"
            ; "20 END"
            ] in
        match result with
        | Ok env ->
            check (option string) "X" (Some "hello")
              (Option.map string_of_value (Env.lookup "X" env))
        | Error err -> Alcotest.fail ("unexpected runtime error: " ^ Error.to_string err))
  ; test_case "INPUT negative number" `Quick (fun () ->
        let result = run_program
            ~input:(fun () -> Some "-3.5")
            [ "10 INPUT X"
            ; "20 END"
            ] in
        match result with
        | Ok env ->
            check (option string) "X" (Some "-3.5")
              (Option.map string_of_value (Env.lookup "X" env))
        | Error err -> Alcotest.fail ("unexpected runtime error: " ^ Error.to_string err))
  ; test_case "INPUT quoted string strips quotes" `Quick (fun () ->
        let result = run_program
            ~input:(fun () -> Some "\"João\"")
            [ "10 INPUT X"
            ; "20 END"
            ] in
        match result with
        | Ok env ->
            check (option string) "X" (Some "João")
              (Option.map string_of_value (Env.lookup "X" env))
        | Error err -> Alcotest.fail ("unexpected runtime error: " ^ Error.to_string err))
  ; test_case "INPUT then PRINT" `Quick (fun () ->
        let output = ref [] in
        match run_program
            ~input:(fun () -> Some "World")
            ~output:(fun s -> output := s :: !output)
            [ "10 INPUT N"
            ; "20 PRINT \"Hello, \" + N + \"!\""
            ; "30 END"
            ] with
        | Ok _ ->
            check (list string) "output" [ "Hello, World!" ] (List.rev !output)
        | Error err -> Alcotest.fail ("unexpected runtime error: " ^ Error.to_string err))
  ; test_case "FOR loop prints 1 2 3" `Quick (fun () ->
        let output = ref [] in
        match run_program
            ~output:(fun s -> output := s :: !output)
            [ "10 FOR X = 1 TO 3"
            ; "20 PRINT X"
            ; "30 NEXT X"
            ; "40 END"
            ] with
        | Ok _ ->
            check (list string) "output" [ "1"; "2"; "3" ] (List.rev !output)
        | Error err -> Alcotest.fail ("unexpected runtime error: " ^ Error.to_string err))
  ; test_case "FOR with STEP 2" `Quick (fun () ->
        let output = ref [] in
        match run_program
            ~output:(fun s -> output := s :: !output)
            [ "10 FOR X = 1 TO 6 STEP 2"
            ; "20 PRINT X"
            ; "30 NEXT X"
            ; "40 END"
            ] with
        | Ok _ ->
            check (list string) "output" [ "1"; "3"; "5" ] (List.rev !output)
        | Error err -> Alcotest.fail ("unexpected runtime error: " ^ Error.to_string err))
  ; test_case "FOR with negative STEP" `Quick (fun () ->
        let output = ref [] in
        match run_program
            ~output:(fun s -> output := s :: !output)
            [ "10 FOR X = 5 TO 1 STEP -2"
            ; "20 PRINT X"
            ; "30 NEXT X"
            ; "40 END"
            ] with
        | Ok _ ->
            check (list string) "output" [ "5"; "3"; "1" ] (List.rev !output)
        | Error err -> Alcotest.fail ("unexpected runtime error: " ^ Error.to_string err))
  ; test_case "FOR body runs at least once" `Quick (fun () ->
        let output = ref [] in
        match run_program
            ~output:(fun s -> output := s :: !output)
            [ "10 FOR X = 1 TO 0"
            ; "20 PRINT X"
            ; "30 NEXT X"
            ; "40 END"
            ] with
        | Ok _ ->
            check (list string) "output" [ "1" ] (List.rev !output)
        | Error err -> Alcotest.fail ("unexpected runtime error: " ^ Error.to_string err))
  ; test_case "nested FOR loops" `Quick (fun () ->
        let output = ref [] in
        match run_program
            ~output:(fun s -> output := s :: !output)
            [ "10 FOR X = 1 TO 2"
            ; "20 FOR Y = 1 TO 2"
            ; "30 PRINT X + Y"
            ; "40 NEXT Y"
            ; "50 NEXT X"
            ; "60 END"
            ] with
        | Ok _ ->
            check (list string) "output" [ "2"; "3"; "3"; "4" ] (List.rev !output)
        | Error err -> Alcotest.fail ("unexpected runtime error: " ^ Error.to_string err))
  ; test_case "bare NEXT works" `Quick (fun () ->
        let output = ref [] in
        match run_program
            ~output:(fun s -> output := s :: !output)
            [ "10 FOR X = 1 TO 2"
            ; "20 PRINT X"
            ; "30 NEXT"
            ; "40 END"
            ] with
        | Ok _ ->
            check (list string) "output" [ "1"; "2" ] (List.rev !output)
        | Error err -> Alcotest.fail ("unexpected runtime error: " ^ Error.to_string err))
  ; test_case "GOSUB subroutine and RETURN" `Quick (fun () ->
        let output = ref [] in
        match run_program
            ~output:(fun s -> output := s :: !output)
            [ "10 GOSUB 100"
            ; "20 GOSUB 100"
            ; "30 END"
            ; "100 PRINT \"sub\""
            ; "110 RETURN"
            ] with
        | Ok _ ->
            check (list string) "output" [ "sub"; "sub" ] (List.rev !output)
        | Error err -> Alcotest.fail ("unexpected runtime error: " ^ Error.to_string err))
  ; test_case "recursive GOSUB computes factorial" `Quick (fun () ->
        let result = run_program
            [ "10 LET N = 5"
            ; "20 LET R = 1"
            ; "30 GOSUB 100"
            ; "40 END"
            ; "100 IF N <= 1 THEN RETURN"
            ; "110 LET R = R * N"
            ; "120 LET N = N - 1"
            ; "130 GOSUB 100"
            ; "140 RETURN"
            ] in
        match result with
        | Ok env ->
            check (option string) "R" (Some "120")
              (Option.map string_of_value (Env.lookup "R" env))
        | Error err -> Alcotest.fail ("unexpected runtime error: " ^ Error.to_string err))
  ; test_case "IF THEN command executes" `Quick (fun () ->
        let output = ref [] in
        match run_program
            ~output:(fun s -> output := s :: !output)
            [ "10 LET X = 1"
            ; "20 IF X THEN PRINT \"yes\""
            ; "30 END"
            ] with
        | Ok _ ->
            check (list string) "output" [ "yes" ] (List.rev !output)
        | Error err -> Alcotest.fail ("unexpected runtime error: " ^ Error.to_string err))
  ; test_case "IF THEN ELSE both directions" `Quick (fun () ->
        let output = ref [] in
        match run_program
            ~output:(fun s -> output := s :: !output)
            [ "10 LET X = 1"
            ; "20 IF X > 5 THEN PRINT \"big\" ELSE PRINT \"small\""
            ; "30 LET X = 9"
            ; "40 IF X > 5 THEN PRINT \"big\" ELSE PRINT \"small\""
            ; "50 END"
            ] with
        | Ok _ ->
            check (list string) "output" [ "small"; "big" ] (List.rev !output)
        | Error err -> Alcotest.fail ("unexpected runtime error: " ^ Error.to_string err))
  ; test_case "IF THEN GOTO preserved" `Quick (fun () ->
        let result = run_program
            [ "10 LET X = 5"
            ; "20 IF X = 5 THEN GOTO 50"
            ; "30 LET X = 99"
            ; "50 END"
            ] in
        match result with
        | Ok env ->
            check (option string) "X" (Some "5")
              (Option.map string_of_value (Env.lookup "X" env))
        | Error err -> Alcotest.fail ("unexpected runtime error: " ^ Error.to_string err))
  ; test_case "IF THEN with FOR and NEXT" `Quick (fun () ->
        let output = ref [] in
        match run_program
            ~output:(fun s -> output := s :: !output)
            [ "10 LET X = 1"
            ; "20 IF X THEN FOR Y = 1 TO 2"
            ; "30 PRINT Y"
            ; "40 NEXT Y"
            ; "50 END"
            ] with
        | Ok _ ->
            check (list string) "output" [ "1"; "2" ] (List.rev !output)
        | Error err -> Alcotest.fail ("unexpected runtime error: " ^ Error.to_string err))
  ; test_case "ELSE binds to innermost IF" `Quick (fun () ->
        let output = ref [] in
        match run_program
            ~output:(fun s -> output := s :: !output)
            [ "10 IF 1 THEN IF 0 THEN PRINT \"a\" ELSE PRINT \"b\""
            ; "20 END"
            ] with
        | Ok _ ->
            check (list string) "output" [ "b" ] (List.rev !output)
        | Error err -> Alcotest.fail ("unexpected runtime error: " ^ Error.to_string err))
  ; test_case "GOTO to a REM line works" `Quick (fun () ->
        let result = run_program
            [ "10 GOTO 30"
            ; "20 PRINT 99"
            ; "30 REM skipped comment"
            ; "40 LET X = 7"
            ; "50 END"
            ] in
        match result with
        | Ok env ->
            check (option string) "X" (Some "7")
              (Option.map string_of_value (Env.lookup "X" env))
        | Error err -> Alcotest.fail ("unexpected runtime error: " ^ Error.to_string err))
  ; test_case "REM lines are no-ops" `Quick (fun () ->
        let result = run_program
            [ "10 REM a comment"
            ; "20 LET X = 1"
            ; "30 END"
            ] in
        match result with
        | Ok env ->
            check (option string) "X" (Some "1")
              (Option.map string_of_value (Env.lookup "X" env))
        | Error err -> Alcotest.fail ("unexpected runtime error: " ^ Error.to_string err))
  ; test_case "DIM and array element access" `Quick (fun () ->
        let result = run_program
            [ "10 DIM A(5)"
            ; "20 LET A(5) = 30"
            ; "30 LET X = A(5)"
            ; "40 END"
            ] in
        match result with
        | Ok env ->
            check (option string) "X" (Some "30")
              (Option.map string_of_value (Env.lookup "X" env))
        | Error err -> Alcotest.fail ("unexpected runtime error: " ^ Error.to_string err))
  ; test_case "DIM array filled by FOR loop" `Quick (fun () ->
        let output = ref [] in
        match run_program
            ~output:(fun s -> output := s :: !output)
            [ "10 DIM A(3)"
            ; "20 FOR I = 0 TO 3"
            ; "30 LET A(I) = I * 10"
            ; "40 NEXT I"
            ; "50 PRINT A(2)"
            ; "60 END"
            ] with
        | Ok _ ->
            check (list string) "output" [ "20" ] (List.rev !output)
        | Error err -> Alcotest.fail ("unexpected runtime error: " ^ Error.to_string err))
  ; test_case "array sum with MOD" `Quick (fun () ->
        let result = run_program
            [ "10 DIM A(2)"
            ; "20 LET A(0) = 7"
            ; "30 LET A(1) = 2"
            ; "40 LET A(2) = 8"
            ; "50 LET X = A(0) + A(1) + A(2)"
            ; "60 LET Y = MOD(X, 8)"
            ; "70 END"
            ] in
        match result with
        | Ok env ->
            check (option string) "X" (Some "17")
              (Option.map string_of_value (Env.lookup "X" env));
            check (option string) "Y" (Some "1")
              (Option.map string_of_value (Env.lookup "Y" env))
        | Error err -> Alcotest.fail ("unexpected runtime error: " ^ Error.to_string err))
  ; test_case "functions in program" `Quick (fun () ->
        let output = ref [] in
        match run_program
            ~output:(fun s -> output := s :: !output)
            [ "10 PRINT ABS(-5)"
            ; "20 PRINT INT(-2.7)"
            ; "30 PRINT LEN(\"abc\")"
            ; "40 PRINT MOD(17, 5)"
            ; "50 END"
            ] with
        | Ok _ ->
            check (list string) "output" [ "5"; "-3"; "3"; "2" ] (List.rev !output)
        | Error err -> Alcotest.fail ("unexpected runtime error: " ^ Error.to_string err))
  ; test_case "array variable is stored in env" `Quick (fun () ->
        let result = run_program
            [ "10 DIM A(2)"
            ; "20 LET A(1) = 5"
            ; "30 END"
            ] in
        match result with
        | Ok env ->
            (match Env.lookup "A" env with
             | Some (Ast.Array a) ->
                 check (list string) "elements"
                   [ "0"; "5"; "0" ]
                   (List.map Machine.string_of_number (Array.to_list a))
             | _ -> Alcotest.fail "expected 'A' to be an array")
        | Error err -> Alcotest.fail ("unexpected runtime error: " ^ Error.to_string err))
  ]

let machine_errors =
  [ test_case "GOTO to missing line" `Quick (fun () ->
        match run_program [ "10 GOTO 99" ] with
        | Error err ->
            (match err.Error.kind with
             | Error.Runtime_error -> ()
             | _ -> Alcotest.fail "expected a runtime error")
        | Ok _ -> Alcotest.fail "expected run to fail")
  ; test_case "undefined variable at runtime" `Quick (fun () ->
        match run_program [ "10 PRINT Y" ] with
        | Error err ->
            (match err.Error.kind with
             | Error.Runtime_error -> ()
             | _ -> Alcotest.fail "expected a runtime error")
        | Ok _ -> Alcotest.fail "expected run to fail")
  ; test_case "division by zero at runtime" `Quick (fun () ->
        match run_program [ "10 LET X = 5 / 0" ] with
        | Error err ->
            (match err.Error.kind with
             | Error.Runtime_error -> ()
             | _ -> Alcotest.fail "expected a runtime error")
        | Ok _ -> Alcotest.fail "expected run to fail")
  ; test_case "INPUT at end of input" `Quick (fun () ->
        match run_program ~input:(fun () -> None) [ "10 INPUT X" ] with
        | Error err ->
            (match err.Error.kind with
             | Error.Runtime_error -> ()
             | _ -> Alcotest.fail "expected a runtime error")
        | Ok _ -> Alcotest.fail "expected run to fail")
  ; test_case "IF with string condition rejected" `Quick (fun () ->
        match run_program [ "10 IF \"a\" THEN GOTO 30" ] with
        | Error err ->
            (match err.Error.kind with
             | Error.Runtime_error -> ()
             | _ -> Alcotest.fail "expected a runtime error")
        | Ok _ -> Alcotest.fail "expected run to fail")
  ; test_case "STEP zero is rejected" `Quick (fun () ->
        match run_program [ "10 FOR X = 1 TO 5 STEP 0" ] with
        | Error err ->
            (match err.Error.kind with
             | Error.Runtime_error -> ()
             | _ -> Alcotest.fail "expected a runtime error")
        | Ok _ -> Alcotest.fail "expected run to fail")
  ; test_case "NEXT without FOR" `Quick (fun () ->
        match run_program [ "10 NEXT X" ] with
        | Error err ->
            (match err.Error.kind with
             | Error.Runtime_error -> ()
             | _ -> Alcotest.fail "expected a runtime error")
        | Ok _ -> Alcotest.fail "expected run to fail")
  ; test_case "NEXT variable mismatch" `Quick (fun () ->
        match run_program
            [ "10 FOR X = 1 TO 3"
            ; "20 NEXT Y"
            ] with
        | Error err ->
            (match err.Error.kind with
             | Error.Runtime_error -> ()
             | _ -> Alcotest.fail "expected a runtime error")
        | Ok _ -> Alcotest.fail "expected run to fail")
  ; test_case "FOR bound type mismatch" `Quick (fun () ->
        match run_program [ "10 FOR X = \"a\" TO 5" ] with
        | Error err ->
            (match err.Error.kind with
             | Error.Runtime_error -> ()
             | _ -> Alcotest.fail "expected a runtime error")
        | Ok _ -> Alcotest.fail "expected run to fail")
  ; test_case "RETURN without GOSUB" `Quick (fun () ->
        match run_program [ "10 RETURN" ] with
        | Error err ->
            (match err.Error.kind with
             | Error.Runtime_error -> ()
             | _ -> Alcotest.fail "expected a runtime error")
        | Ok _ -> Alcotest.fail "expected run to fail")
  ; test_case "GOSUB to missing line" `Quick (fun () ->
        match run_program [ "10 GOSUB 99" ] with
        | Error err ->
            (match err.Error.kind with
             | Error.Runtime_error -> ()
             | _ -> Alcotest.fail "expected a runtime error")
        | Ok _ -> Alcotest.fail "expected run to fail")
  ; test_case "array element without DIM" `Quick (fun () ->
        match run_program [ "10 LET A(0) = 5" ] with
        | Error err ->
            (match err.Error.kind with
             | Error.Runtime_error -> ()
             | _ -> Alcotest.fail "expected a runtime error")
        | Ok _ -> Alcotest.fail "expected run to fail")
  ; test_case "array index out of bounds on assignment" `Quick (fun () ->
        match run_program [ "10 DIM A(2)"; "20 LET A(3) = 5" ] with
        | Error err ->
            (match err.Error.kind with
             | Error.Runtime_error -> ()
             | _ -> Alcotest.fail "expected a runtime error")
        | Ok _ -> Alcotest.fail "expected run to fail")
  ; test_case "negative array index" `Quick (fun () ->
        match run_program [ "10 DIM A(2)"; "20 LET A(-1) = 5" ] with
        | Error err ->
            (match err.Error.kind with
             | Error.Runtime_error -> ()
             | _ -> Alcotest.fail "expected a runtime error")
        | Ok _ -> Alcotest.fail "expected run to fail")
  ; test_case "reading out of bounds array element" `Quick (fun () ->
        match run_program [ "10 DIM A(2)"; "20 LET X = A(5)" ] with
        | Error err ->
            (match err.Error.kind with
             | Error.Runtime_error -> ()
             | _ -> Alcotest.fail "expected a runtime error")
        | Ok _ -> Alcotest.fail "expected run to fail")
  ; test_case "duplicate DIM rejected" `Quick (fun () ->
        match run_program [ "10 DIM A(2)"; "20 DIM A(2)" ] with
        | Error err ->
            (match err.Error.kind with
             | Error.Runtime_error -> ()
             | _ -> Alcotest.fail "expected a runtime error")
        | Ok _ -> Alcotest.fail "expected run to fail")
  ; test_case "negative DIM size rejected" `Quick (fun () ->
        match run_program [ "10 DIM A(-1)" ] with
        | Error err ->
            (match err.Error.kind with
             | Error.Runtime_error -> ()
             | _ -> Alcotest.fail "expected a runtime error")
        | Ok _ -> Alcotest.fail "expected run to fail")
  ; test_case "assigning string to array element rejected" `Quick (fun () ->
        match run_program [ "10 DIM A(2)"; "20 LET A(0) = \"x\"" ] with
        | Error err ->
            (match err.Error.kind with
             | Error.Runtime_error -> ()
             | _ -> Alcotest.fail "expected a runtime error")
        | Ok _ -> Alcotest.fail "expected run to fail")
  ; test_case "printing an array rejected" `Quick (fun () ->
        match run_program [ "10 DIM A(2)"; "20 PRINT A" ] with
        | Error err ->
            (match err.Error.kind with
             | Error.Runtime_error -> ()
             | _ -> Alcotest.fail "expected a runtime error")
        | Ok _ -> Alcotest.fail "expected run to fail")
  ; test_case "assigning a whole array rejected" `Quick (fun () ->
        match run_program [ "10 DIM A(2)"; "20 LET B = A" ] with
        | Error err ->
            (match err.Error.kind with
             | Error.Runtime_error -> ()
             | _ -> Alcotest.fail "expected a runtime error")
        | Ok _ -> Alcotest.fail "expected run to fail")
  ]

let tests = [ "machine", machine_ok @ machine_errors ]
