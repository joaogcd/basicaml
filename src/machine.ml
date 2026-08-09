open Ast
open Env
open Eval

type step =
  | Next of t
  | Jump of t * int
  | Halt
  | Error of string

let eval_command env = function
  | Let (v, e) ->
      begin match eval_expr env e with
      | Some value -> Next (update v value env)
      | None -> Error "LET Error"
      end
  | Print e ->
      begin match eval_expr env e with
      | Some (Number n) ->
          print_endline (string_of_int n);
          Next env
      | _ -> Error "PRINT Error"
      end
  | Goto n -> Jump (env, n)
  | If (e, n) ->
      begin match eval_expr env e with
      | Some (Number v) when v <> 0 -> Jump (env, n)
      | Some (Number _) -> Next env
      | _ -> Error " IF Error"
      end
  | End -> Halt

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

let rec run prog env pc =
  match find_line pc prog with
  | None -> ()
  | Some (_, cmd) ->
      match eval_command env cmd with
      | Next env' ->
          begin match next_line pc prog with
          | Some n -> run prog env' n
          | None -> ()
          end
      | Jump (env', n) -> run prog env' n
      | Halt -> ()
      | Error msg ->
          print_endline msg
