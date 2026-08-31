open Token
open Ast

let error msg = Error.make ~kind:Error.Parse_error msg

let rec parse_binop parse_operand match_op left tokens =
  match tokens with
  | tok :: rest ->
      begin match match_op tok with
      | Some op ->
          begin match parse_operand rest with
          | Error e -> Error e
          | Ok (right, rest') ->
              parse_binop parse_operand match_op (Binop (op, left, right)) rest'
          end
      | None -> Ok (left, tokens)
      end
  | [] -> Ok (left, tokens)

let match_add = function
  | PLUS -> Some Add
  | MINUS -> Some Sub
  | _ -> None

let match_mul = function
  | STAR -> Some Mul
  | SLASH -> Some Div
  | _ -> None

let match_cmp = function
  | EQUAL -> Some Eq
  | LT -> Some Lt
  | GT -> Some Gt
  | LE -> Some Le
  | GE -> Some Ge
  | NE -> Some Ne
  | _ -> None

let match_and = function
  | AND -> Some And
  | _ -> None

let match_or = function
  | OR -> Some Or
  | _ -> None

let builtins = [ "ABS"; "INT"; "RND"; "LEN"; "MOD" ]

let rec parse_primary = function
  | INT n :: rest -> Ok (Const (float_of_int n), rest)
  | FLOAT f :: rest -> Ok (Const f, rest)
  | STRING s :: rest -> Ok (Str s, rest)
  | IDENT s :: LPAREN :: rest when List.mem s builtins ->
      begin match parse_expr rest with
      | Error e -> Error e
      | Ok (arg, rest') -> parse_call_args [ arg ] rest' |> Result.map (fun (args, rest'') -> (Call (s, args), rest''))
      end
  | IDENT s :: LPAREN :: rest ->
      begin match parse_expr rest with
      | Error e -> Error e
      | Ok (idx, RPAREN :: rest') -> Ok (ArrayAccess (s, idx), rest')
      | Ok _ -> Error (error "expected ')'")
      end
  | IDENT s :: rest -> Ok (Var s, rest)
  | LPAREN :: rest ->
      begin match parse_expr rest with
      | Error e -> Error e
      | Ok (e, RPAREN :: rest') -> Ok (e, rest')
      | Ok _ -> Error (error "expected ')'")
      end
  | _ -> Error (error "expected a term")

and parse_unary = function
  | MINUS :: rest ->
      begin match parse_unary rest with
      | Error e -> Error e
      | Ok (e, rest') -> Ok (Unop (Neg, e), rest')
      end
  | rest -> parse_primary rest

and parse_mul tokens =
  match parse_unary tokens with
  | Error e -> Error e
  | Ok (left, rest) -> parse_binop parse_unary match_mul left rest

and parse_add tokens =
  match parse_mul tokens with
  | Error e -> Error e
  | Ok (left, rest) -> parse_binop parse_mul match_add left rest

and parse_cmp tokens =
  match parse_add tokens with
  | Error e -> Error e
  | Ok (left, rest) -> parse_binop parse_add match_cmp left rest

and parse_not = function
  | NOT :: rest ->
      begin match parse_not rest with
      | Error e -> Error e
      | Ok (e, rest') -> Ok (Unop (Not, e), rest')
      end
  | rest -> parse_cmp rest

and parse_and tokens =
  match parse_not tokens with
  | Error e -> Error e
  | Ok (left, rest) -> parse_binop parse_not match_and left rest

and parse_or tokens =
  match parse_and tokens with
  | Error e -> Error e
  | Ok (left, rest) -> parse_binop parse_and match_or left rest

and parse_expr tokens = parse_or tokens

and parse_call_args acc = function
  | RPAREN :: rest -> Ok (List.rev acc, rest)
  | COMMA :: rest ->
      begin match parse_expr rest with
      | Error e -> Error e
      | Ok (e, rest') -> parse_call_args (e :: acc) rest'
      end
  | _ -> Error (error "expected ',' or ')'")

let parse_target = function
  | IDENT s :: LPAREN :: rest ->
      begin match parse_expr rest with
      | Error e -> Error e
      | Ok (idx, RPAREN :: rest') -> Ok (ArrayElement (s, idx), rest')
      | Ok _ -> Error (error "expected ')'")
      end
  | IDENT s :: rest -> Ok (Var s, rest)
  | _ -> Error (error "expected a variable")

let expect_end = function
  | [] -> Ok ()
  | _ -> Error (error "unexpected tokens after command")

let rec parse_command_raw = function
  | LET :: rest ->
      begin match parse_target rest with
      | Error e -> Error e
      | Ok (t, EQUAL :: rest') ->
          parse_expr rest' |> Result.map (fun (e, rest'') -> (Let (t, e), rest''))
      | Ok _ -> Error (error "LET syntax error: expected 'LET variable = expression'")
      end
  | PRINT :: rest ->
      parse_expr rest |> Result.map (fun (e, rest') -> (Print e, rest'))
  | GOTO :: INT n :: rest -> Ok (Goto n, rest)
  | GOTO :: _ -> Error (error "GOTO syntax error: expected 'GOTO line-number'")
  | INPUT :: IDENT v :: rest -> Ok (Input v, rest)
  | INPUT :: _ -> Error (error "INPUT syntax error: expected 'INPUT variable'")
  | FOR :: IDENT v :: EQUAL :: rest ->
      begin match parse_expr rest with
      | Error e -> Error e
      | Ok (start, rest') ->
          begin match rest' with
          | TO :: rest'' ->
              begin match parse_expr rest'' with
              | Error e -> Error e
              | Ok (limit, rest''') ->
                  begin match rest''' with
                  | STEP :: rest'''' ->
                      parse_expr rest'''' |> Result.map
                        (fun (step, rest''''') -> (For (v, start, limit, Some step), rest'''''))
                  | _ -> Ok (For (v, start, limit, None), rest''')
                  end
              end
          | _ -> Error (error "FOR syntax error: expected 'FOR var = start TO limit [STEP step]'")
          end
      end
  | FOR :: _ -> Error (error "FOR syntax error: expected 'FOR var = start TO limit [STEP step]'")
  | NEXT :: IDENT v :: rest -> Ok (Next (Some v), rest)
  | NEXT :: rest -> Ok (Next None, rest)
  | GOSUB :: INT n :: rest -> Ok (Gosub n, rest)
  | GOSUB :: _ -> Error (error "GOSUB syntax error: expected 'GOSUB line-number'")
  | RETURN :: rest -> Ok (Return, rest)
  | IF :: rest ->
      begin match parse_expr rest with
      | Error e -> Error e
      | Ok (e, rest') ->
          begin match rest' with
          | THEN :: rest'' ->
              begin match parse_command_raw rest'' with
              | Error e -> Error e
              | Ok (then_cmd, rest''') ->
                  begin match rest''' with
                  | ELSE :: rest'''' ->
                      parse_command_raw rest'''' |> Result.map
                        (fun (else_cmd, rest''''') -> (If (e, then_cmd, Some else_cmd), rest'''''))
                  | _ -> Ok (If (e, then_cmd, None), rest''')
                  end
              end
          | _ -> Error (error "IF syntax error: expected 'IF expr THEN command [ELSE command]'")
          end
      end
  | DIM :: IDENT v :: LPAREN :: rest ->
      begin match parse_expr rest with
      | Error e -> Error e
      | Ok (e, RPAREN :: rest') -> Ok (Dim (v, e), rest')
      | Ok _ -> Error (error "expected ')'")
      end
  | DIM :: _ -> Error (error "DIM syntax error: expected 'DIM variable(size)'")
  | END :: rest -> Ok (End, rest)
  | _ -> Error (error "invalid command")

let parse_command tokens =
  match parse_command_raw tokens with
  | Error e -> Error e
  | Ok (cmd, rest) ->
      expect_end rest |> Result.map (fun () -> cmd)

let line_number_of_string s =
  let n = String.length s in
  let rec go i =
    if i < n && Lexer.is_digit s.[i] then go (i + 1)
    else i
  in
  let i = go 0 in
  if i = 0 then None
  else Some (int_of_string (String.sub s 0 i))

let is_rem_command rest =
  let len = String.length rest in
  len >= 3
  && String.sub rest 0 3 = "REM"
  && (len = 3 || not (Lexer.is_letter rest.[3]))

let rem_of_line line =
  let n = String.length line in
  let rec go i =
    if i < n && Lexer.is_digit line.[i] then go (i + 1)
    else i
  in
  let i = go 0 in
  if i = 0 then None
  else
    let rest = String.trim (String.sub line i (n - i)) in
    if is_rem_command rest then Some (int_of_string (String.sub line 0 i))
    else None

let attach_line err line =
  match line_number_of_string line with
  | Some n -> Error.with_line n err
  | None -> err

let parse_line raw =
  let line = String.trim raw in
  match rem_of_line line with
  | Some n -> Ok (n, Rem)
  | None ->
      begin match Lexer.lex line with
      | Error e -> Error (attach_line e line)
      | Ok (INT n :: rest) ->
          begin match parse_command rest with
          | Error e -> Error (attach_line e line)
          | Ok cmd -> Ok (n, cmd)
          end
      | Ok _ -> Error (attach_line (error "line must start with a line number") line)
      end

let parse_program lines =
  let rec go acc = function
    | [] -> Ok (List.rev acc)
    | line :: rest ->
        if String.trim line = "" then go acc rest
        else
          match parse_line line with
          | Error err -> Error (attach_line err line)
          | Ok l -> go (l :: acc) rest
  in
  match go [] lines with
  | Error e -> Error e
  | Ok prog ->
      let sorted = List.sort (fun (a, _) (b, _) -> compare a b) prog in
      let rec check_dups prev = function
        | [] -> Ok ()
        | (n, _) :: _ when Some n = prev ->
            Error (Error.with_line n (error (Printf.sprintf "duplicate line number %d" n)))
        | (n, _) :: rest -> check_dups (Some n) rest
      in
      check_dups None sorted |> Result.map (fun () -> sorted)
