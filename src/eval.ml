open Ast
open Env

let rec eval_expr env = function
  | Const n -> Some (Number n)
  | Var x -> lookup x env
  | Add (a, b) ->
      begin match eval_expr env a, eval_expr env b with
      | Some (Number x), Some (Number y) -> Some (Number (x + y))
      | _ -> None
      end
  | Sub (a, b) ->
      begin match eval_expr env a, eval_expr env b with
      | Some (Number x), Some (Number y) -> Some (Number (x - y))
      | _ -> None
      end