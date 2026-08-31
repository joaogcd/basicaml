open Alcotest
open Basicaml

let repl_output =
  let check name lines expected =
    test_case name `Quick (fun () ->
      let remaining = ref lines in
      let output = ref [] in
      Repl.run
        ~input:(fun () ->
          match !remaining with
          | [] -> None
          | line :: rest ->
              remaining := rest;
              Some line)
        ~output:(fun s -> output := s :: !output)
        ();
      check (list string) "output" expected (List.rev !output))
  in
  [ check "store and RUN a program"
      [ "10 PRINT 42"; "RUN" ]
      [ "42" ]
  ; check "run a two-line program with a variable"
      [ "10 LET X = 5"
      ; "20 PRINT X * 2"
      ; "RUN"
      ]
      [ "10" ]
  ; check "lone line number deletes the line"
      [ "10 PRINT 1"; "20 PRINT 2"; "10"; "RUN" ]
      [ "2" ]
  ; check "LIST prints the stored program"
      [ "10 PRINT 1"; "20 PRINT 2"; "LIST" ]
      [ "10: PRINT 1\n20: PRINT 2" ]
  ; check "LIST replaces a line with the same number"
      [ "10 PRINT 1"; "10 PRINT 3"; "LIST" ]
      [ "10: PRINT 3" ]
  ; check "immediate commands share variables"
      [ "LET X = 2"; "PRINT X * 3" ]
      [ "6" ]
  ; check "immediate and stored variables persist after RUN"
      [ "LET X = 2"; "10 PRINT X"; "RUN" ]
      [ "2" ]
  ; check "syntax error reported and program intact"
      [ "10 PRINT 1"; "20 PRINT ("; "RUN" ]
      [ "Line 20: syntax error: expected a term"; "1" ]
  ; check "CLEAR empties the program"
      [ "10 PRINT 1"; "CLEAR"; "RUN" ]
      [ "No program loaded." ]
  ; check "NEW empties the program"
      [ "10 PRINT 1"; "NEW"; "LIST" ]
      [ "No program loaded." ]
  ; check "RUN with no program"
      [ "RUN" ]
      [ "No program loaded." ]
  ; check "QUIT stops reading"
      [ "10 PRINT 1"; "QUIT" ]
      [ ]
  ; test_case "help is shown" `Quick (fun () ->
        let remaining = ref [ "HELP" ] in
        let output = ref [] in
        Repl.run
          ~input:(fun () ->
            match !remaining with
            | [] -> None
            | line :: rest ->
                remaining := rest;
                Some line)
          ~output:(fun s -> output := s :: !output)
          ();
        match List.rev !output with
        | [ text ] ->
            if String.starts_with ~prefix:"BasicAML REPL" text then ()
            else Alcotest.fail ("unexpected help text: " ^ text)
        | _ -> Alcotest.fail "expected exactly one output line")
  ; test_case "immediate error is reported" `Quick (fun () ->
        let remaining = ref [ "GOTO 99" ] in
        let output = ref [] in
        Repl.run
          ~input:(fun () ->
            match !remaining with
            | [] -> None
            | line :: rest ->
                remaining := rest;
                Some line)
          ~output:(fun s -> output := s :: !output)
          ();
        match List.rev !output with
        | [ text ] ->
            if text = "runtime error: line 99 does not exist" then ()
            else Alcotest.fail ("unexpected immediate error: " ^ text)
        | _ -> Alcotest.fail "expected exactly one output line")
  ; check "INPUT reads from the same stream at runtime"
      [ "10 INPUT X"
      ; "20 PRINT X + 1"
      ; "RUN"
      ; "42"
      ]
      [ "43" ]
  ]

let tests = [ "repl", repl_output ]
