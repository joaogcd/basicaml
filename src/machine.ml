open Ast
open Env
open Eval

let ( let* ) = Result.bind

type loop =
  { loop_var : string
  ; loop_limit : float
  ; loop_step : float
  ; loop_body : int
  }

type state =
  { env : Env.t
  ; loops : loop list
  ; rets : int list
  }

let env st = st.env

type step =
  | Next of state
  | Jump of state * int
  | Halt
  | Fail of Error.t

let error msg = Error.make ~kind:Error.Runtime_error msg

let string_of_number = Eval.string_of_number
let string_of_value = Eval.string_of_value

let default_input () = In_channel.input_line stdin

let parse_input text =
  let t = String.trim text in
  let len = String.length t in
  let is_quoted = len >= 2 && t.[0] = '"' && t.[len - 1] = '"' in
  if is_quoted then String (String.sub t 1 (len - 2))
  else
    match float_of_string_opt t with
    | Some f -> Number f
    | None -> String t

let eval_number env what e =
  match eval_expr env e with
  | Error err -> Error err
  | Ok (Number n) -> Ok n
  | Ok (String _) -> Error (error (Printf.sprintf "type mismatch in '%s'" what))
  | Ok (Array _) -> Error (error (Printf.sprintf "type mismatch in '%s'" what))

let find_line pc prog =
  List.find_opt (fun (n, _) -> n = pc) prog

let next_line pc prog =
  prog
  |> List.map fst
  |> List.filter (fun n -> n > pc)
  |> List.sort compare
  |> (function
    | [] -> None
    | x :: _ -> Some x)

let rec eval_command ~input ~output prog line st = function
| Let (target, e) ->
      let err_or_step =
        let* value = eval_expr st.env e in
        begin match value with
        | Array _ -> Error (error "cannot assign an array")
        | _ ->
            begin match target with
            | Var v -> Ok (Next { st with env = update v value st.env })
            | ArrayElement (name, idx) ->
                let* idx_val = eval_expr st.env idx in
                let* n = number_of "array index" idx_val in
                let* arr =
                  Option.to_result
                    ~none:(error (Printf.sprintf "array '%s' is undefined" name))
                    (lookup name st.env)
                in
                begin match arr with
                | Array a ->
                    let* k = element_index name n (Array.length a) in
                    begin match value with
                    | Number x ->
                        a.(k) <- x;
                        Ok (Next st)
                    | String _ -> Error (type_error "array element")
                    | Array _ -> assert false
                    end
                | _ -> Error (error (Printf.sprintf "'%s' is not an array" name))
                end
            end
        end
      in
      begin match err_or_step with
      | Ok step -> step
      | Error err -> Fail (Error.with_line line err)
      end
  | Print e ->
      begin match eval_expr st.env e with
      | Error err -> Fail (Error.with_line line err)
      | Ok (Array _) -> Fail (Error.with_line line (error "cannot print an array"))
      | Ok value ->
          output (string_of_value value);
          Next st
      end
  | Goto n -> Jump (st, n)
  | If (e, then_cmd, else_cmd) ->
      begin match eval_expr st.env e with
      | Error err -> Fail (Error.with_line line err)
      | Ok (String _) -> Fail (Error.with_line line (error "IF condition must be a number"))
      | Ok (Array _) -> Fail (Error.with_line line (error "IF condition must be a number"))
      | Ok (Number v) ->
          if v <> 0.0 then eval_command ~input ~output prog line st then_cmd
          else
            match else_cmd with
            | Some c -> eval_command ~input ~output prog line st c
            | None -> Next st
      end
  | Input v ->
      begin match input () with
      | None -> Fail (Error.with_line line (error "INPUT: unexpected end of input"))
      | Some text -> Next { st with env = update v (parse_input text) st.env }
      end
  | For (v, start_e, limit_e, step_e) ->
      let step =
        match step_e with
        | None -> Ok 1.0
        | Some e -> eval_number st.env "FOR bound" e
      in
      begin match
        let* start = eval_number st.env "FOR bound" start_e in
        let* limit = eval_number st.env "FOR bound" limit_e in
        let* step = step in
        Ok (start, limit, step)
      with
      | Error err -> Fail (Error.with_line line err)
      | Ok (start, limit, step) ->
          if step = 0.0 then Fail (Error.with_line line (error "FOR STEP cannot be zero"))
          else
            match next_line line prog with
            | None -> Next st
            | Some body ->
                let frame = { loop_var = v; loop_limit = limit; loop_step = step; loop_body = body } in
                Next { st with env = update v (Number start) st.env; loops = frame :: st.loops }
      end
  | Ast.Next var_opt ->
      begin match st.loops with
      | [] -> Fail (Error.with_line line (error "NEXT without FOR"))
      | frame :: rest_loops ->
          begin match var_opt with
          | Some v when v <> frame.loop_var ->
              Fail (Error.with_line line
                (error (Printf.sprintf "NEXT variable '%s' does not match FOR variable '%s'" v frame.loop_var)))
          | _ ->
              begin match lookup frame.loop_var st.env with
              | Some (Number cur) ->
                  let next = cur +. frame.loop_step in
                  let env' = update frame.loop_var (Number next) st.env in
                  let continuing =
                    if frame.loop_step > 0.0 then next <= frame.loop_limit
                    else next >= frame.loop_limit
                  in
                  if continuing then Jump ({ st with env = env' }, frame.loop_body)
                  else Next { st with env = env'; loops = rest_loops }
              | Some (String _) ->
                  Fail (Error.with_line line
                    (error (Printf.sprintf "type mismatch: '%s' is not a number" frame.loop_var)))
              | Some (Array _) ->
                  Fail (Error.with_line line
                    (error (Printf.sprintf "type mismatch: '%s' is not a number" frame.loop_var)))
              | None ->
                  Fail (Error.with_line line
                    (error (Printf.sprintf "FOR variable '%s' is undefined" frame.loop_var)))
              end
          end
      end
  | Gosub n ->
      begin match next_line line prog with
      | None -> Fail (Error.with_line line (error "GOSUB has no return line"))
      | Some ret -> Jump ({ st with rets = ret :: st.rets }, n)
      end
  | Return ->
      begin match st.rets with
      | [] -> Fail (Error.with_line line (error "RETURN without GOSUB"))
      | r :: rest -> Jump ({ st with rets = rest }, r)
      end
  | Dim (name, size_e) ->
      let err_or_step =
        let* size = eval_number st.env "DIM size" size_e in
        if not (Float.is_integer size) then
          Error (error (Printf.sprintf "array size in 'DIM %s' must be an integer" name))
        else
          let n = int_of_float size in
          if n < 0 then
            Error (error (Printf.sprintf "array size %d in 'DIM %s' cannot be negative" n name))
          else
            match lookup name st.env with
            | Some _ -> Error (error (Printf.sprintf "'%s' is already defined" name))
            | None ->
                Ok (Next { st with env = update name (Array (Array.make (n + 1) 0.0)) st.env })
      in
      begin match err_or_step with
      | Ok step -> step
      | Error err -> Fail (Error.with_line line err)
      end
  | Rem -> Next st
  | End -> Halt

let rec run_impl input output prog st pc =
  match find_line pc prog with
  | None -> Error (error (Printf.sprintf "line %d does not exist" pc))
  | Some (line, cmd) ->
      match eval_command ~input ~output prog line st cmd with
      | Next st' ->
          begin match next_line pc prog with
          | Some n -> run_impl input output prog st' n
          | None -> Ok st'
          end
      | Jump (st', n) -> run_impl input output prog st' n
      | Halt -> Ok st
      | Fail err -> Error err

let run ?(input = default_input) ?(output = print_endline) prog env pc =
  run_impl input output prog { env; loops = []; rets = [] } pc