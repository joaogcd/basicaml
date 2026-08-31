open Alcotest
open Basicaml
open Test_helpers

let eval_exprs =
  let check_eval s expected =
    test_case ("eval: " ^ s) `Quick (fun () ->
      let value = eval_expr_str Env.empty s in
      check string "value" expected (string_of_value value))
  in
  [ check_eval "1.5 + 2.5" "4"
  ; check_eval "10 / 4" "2.5"
  ; check_eval "2 + 3 * 4" "14"
  ; check_eval "(2 + 3) * 4" "20"
  ; check_eval "10 - 3 - 2" "5"
  ; check_eval "2 * 3 / 4" "1.5"
  ; check_eval "-5" "-5"
  ; check_eval "- (2 + 3)" "-5"
  ; check_eval "5 > 3" "1"
  ; check_eval "5 < 3" "0"
  ; check_eval "5 <= 5" "1"
  ; check_eval "5 >= 6" "0"
  ; check_eval "5 <> 5" "0"
  ; check_eval "2 = 2" "1"
  ; check_eval "5 > 3 AND 2 < 4" "1"
  ; check_eval "1 AND 0" "0"
  ; check_eval "1 OR 0" "1"
  ; check_eval "NOT 0" "1"
  ; check_eval "NOT 5" "0"
  ; check_eval "NOT 1 = 0" "1"
  ; check_eval "\"a\" + \"b\"" "ab"
  ; check_eval "\"X=\" + 2" "X=2"
  ; check_eval "2 + \" items\"" "2 items"
  ; check_eval "\"pi = \" + 3.5" "pi = 3.5"
  ; check_eval "\"a\" = \"a\"" "1"
  ; check_eval "\"a\" <> \"b\"" "1"
  ; check_eval "\"b\" < \"a\"" "0"
  ; check_eval "\"a\" + \"b\" = \"ab\"" "1"
  ; check_eval "ABS(-5)" "5"
  ; check_eval "ABS(3.5)" "3.5"
  ; check_eval "INT(2.7)" "2"
  ; check_eval "INT(-2.7)" "-3"
  ; check_eval "LEN(\"hello\")" "5"
  ; check_eval "MOD(7, 3)" "1"
  ; check_eval "MOD(-21, 4)" "-1"
  ; check_eval "MOD(10, 5)" "0"
  ; check_eval "ABS(-2) + INT(1.9)" "3"
  ]

let eval_errors =
  [ test_case "undefined variable" `Quick (fun () ->
        match Eval.eval_expr Env.empty (Ast.Var "Y") with
        | Error err ->
            (match err.Error.kind with
             | Error.Runtime_error -> ()
             | _ -> Alcotest.fail "expected a runtime error")
        | Ok _ -> Alcotest.fail "expected eval to fail")
  ; test_case "division by zero" `Quick (fun () ->
        match Eval.eval_expr Env.empty (Ast.Binop (Ast.Div, Ast.Const 5., Ast.Const 0.)) with
        | Error err ->
            (match err.Error.kind with
             | Error.Runtime_error -> ()
             | _ -> Alcotest.fail "expected a runtime error")
        | Ok _ -> Alcotest.fail "expected eval to fail")
  ; test_case "type mismatch: string minus number" `Quick (fun () ->
        match Eval.eval_expr Env.empty (Ast.Binop (Ast.Sub, Ast.Str "a", Ast.Const 1.)) with
        | Error err ->
            (match err.Error.kind with
             | Error.Runtime_error -> ()
             | _ -> Alcotest.fail "expected a runtime error")
        | Ok _ -> Alcotest.fail "expected eval to fail")
  ; test_case "type mismatch: NOT string" `Quick (fun () ->
        match Eval.eval_expr Env.empty (Ast.Unop (Ast.Not, Ast.Str "a")) with
        | Error err ->
            (match err.Error.kind with
             | Error.Runtime_error -> ()
             | _ -> Alcotest.fail "expected a runtime error")
        | Ok _ -> Alcotest.fail "expected eval to fail")
  ; test_case "type mismatch: compare string with number" `Quick (fun () ->
        match Eval.eval_expr Env.empty (Ast.Binop (Ast.Lt, Ast.Str "a", Ast.Const 1.)) with
        | Error err ->
            (match err.Error.kind with
             | Error.Runtime_error -> ()
             | _ -> Alcotest.fail "expected a runtime error")
        | Ok _ -> Alcotest.fail "expected eval to fail")
  ; test_case "type mismatch: ABS on string" `Quick (fun () ->
        match Eval.eval_expr Env.empty (Ast.Call ("ABS", [ Ast.Str "a" ])) with
        | Error err ->
            (match err.Error.kind with
             | Error.Runtime_error -> ()
             | _ -> Alcotest.fail "expected a runtime error")
        | Ok _ -> Alcotest.fail "expected eval to fail")
  ; test_case "type mismatch: LEN on number" `Quick (fun () ->
        match Eval.eval_expr Env.empty (Ast.Call ("LEN", [ Ast.Const 5. ])) with
        | Error err ->
            (match err.Error.kind with
             | Error.Runtime_error -> ()
             | _ -> Alcotest.fail "expected a runtime error")
        | Ok _ -> Alcotest.fail "expected eval to fail")
  ; test_case "MOD division by zero" `Quick (fun () ->
        match Eval.eval_expr Env.empty (Ast.Call ("MOD", [ Ast.Const 5.; Ast.Const 0. ])) with
        | Error err ->
            (match err.Error.kind with
             | Error.Runtime_error -> ()
             | _ -> Alcotest.fail "expected a runtime error")
        | Ok _ -> Alcotest.fail "expected eval to fail")
  ; test_case "wrong number of arguments" `Quick (fun () ->
        match Eval.eval_expr Env.empty (Ast.Call ("ABS", [])) with
        | Error err ->
            (match err.Error.kind with
             | Error.Runtime_error -> ()
             | _ -> Alcotest.fail "expected a runtime error")
        | Ok _ -> Alcotest.fail "expected eval to fail")
  ; test_case "array index out of bounds" `Quick (fun () ->
        let env = Env.update "A" (Ast.Array (Array.make 4 0.0)) Env.empty in
        match Eval.eval_expr env (Ast.ArrayAccess ("A", Ast.Const 5.)) with
        | Error err ->
            (match err.Error.kind with
             | Error.Runtime_error -> ()
             | _ -> Alcotest.fail "expected a runtime error")
        | Ok _ -> Alcotest.fail "expected eval to fail")
  ; test_case "array index must be an integer" `Quick (fun () ->
        let env = Env.update "A" (Ast.Array (Array.make 4 0.0)) Env.empty in
        match Eval.eval_expr env (Ast.ArrayAccess ("A", Ast.Const 1.5)) with
        | Error err ->
            (match err.Error.kind with
             | Error.Runtime_error -> ()
             | _ -> Alcotest.fail "expected a runtime error")
        | Ok _ -> Alcotest.fail "expected eval to fail")
  ; test_case "undefined array" `Quick (fun () ->
        match Eval.eval_expr Env.empty (Ast.ArrayAccess ("A", Ast.Const 1.)) with
        | Error err ->
            (match err.Error.kind with
             | Error.Runtime_error -> ()
             | _ -> Alcotest.fail "expected a runtime error")
        | Ok _ -> Alcotest.fail "expected eval to fail")
  ; test_case "adding an array is a type mismatch" `Quick (fun () ->
        let env = Env.update "A" (Ast.Array (Array.make 4 0.0)) Env.empty in
        match Eval.eval_expr env (Ast.Binop (Ast.Add, Ast.Var "A", Ast.Const 1.)) with
        | Error err ->
            (match err.Error.kind with
             | Error.Runtime_error -> ()
             | _ -> Alcotest.fail "expected a runtime error")
        | Ok _ -> Alcotest.fail "expected eval to fail")
  ]

let tests = [ "eval", eval_exprs @ eval_errors ]
