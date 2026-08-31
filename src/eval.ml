open Ast
open Env

let ( let* ) = Result.bind

let error msg = Error.make ~kind:Error.Runtime_error msg

let type_error op =
  error (Printf.sprintf "type mismatch in '%s'" op)

let bool_of = function
  | true -> Number 1.0
  | false -> Number 0.0

let string_of_number f =
  if Float.is_integer f then Printf.sprintf "%.0f" f
  else Printf.sprintf "%g" f

let string_of_value = function
  | Number n -> string_of_number n
  | String s -> s
  | Array _ -> assert false

let number_of op = function
  | Number n -> Ok n
  | String _ -> Error (type_error op)
  | Array _ -> Error (type_error op)

let string_of = function
  | Number n -> string_of_number n
  | String s -> s
  | Array _ -> assert false

let eval_binop op a b =
  match op with
  | Add ->
      begin match a, b with
      | Array _, _ | _, Array _ -> Error (type_error "+")
      | Number x, Number y -> Ok (Number (x +. y))
      | String _, _ | _, String _ -> Ok (String (string_of a ^ string_of b))
      end
  | Sub | Mul | Div ->
      let* x = number_of "operand" a in
      let* y = number_of "operand" b in
      begin match op with
      | Sub -> Ok (Number (x -. y))
      | Mul -> Ok (Number (x *. y))
      | Div ->
          if y = 0.0 then Error (error "division by zero")
          else Ok (Number (x /. y))
      | _ -> assert false
      end
  | Eq | Ne | Lt | Le | Gt | Ge ->
      begin match a, b with
      | Number x, Number y ->
          Ok (bool_of (match op with
            | Eq -> x = y | Ne -> x <> y
            | Lt -> x < y | Le -> x <= y
            | Gt -> x > y | Ge -> x >= y
            | _ -> assert false))
      | String x, String y ->
          let c = String.compare x y in
          Ok (bool_of (match op with
            | Eq -> c = 0 | Ne -> c <> 0
            | Lt -> c < 0 | Le -> c <= 0
            | Gt -> c > 0 | Ge -> c >= 0
            | _ -> assert false))
      | _ -> Error (type_error "comparison")
      end
  | And | Or ->
      let* x = number_of "operand" a in
      let* y = number_of "operand" b in
      Ok (bool_of (match op with
        | And -> x <> 0.0 && y <> 0.0
        | Or -> x <> 0.0 || y <> 0.0
        | _ -> assert false))

let eval_unop op a =
  match op with
  | Neg ->
      let* x = number_of "operand" a in
      Ok (Number (-.x))
  | Not ->
      let* x = number_of "operand" a in
      Ok (bool_of (x = 0.0))

let element_index name n len =
  if Float.is_integer n then
    let k = int_of_float n in
    if k < 0 || k >= len then
      Error (error (Printf.sprintf "array index %d out of bounds in '%s'" k name))
    else Ok k
  else Error (error (Printf.sprintf "array index in '%s' must be an integer" name))

let rec eval_call env f args =
  let rec eval_args acc = function
    | [] -> Ok (List.rev acc)
    | a :: rest ->
        let* v = eval_expr env a in
        eval_args (v :: acc) rest
  in
  let* vals = eval_args [] args in
  match f, vals with
  | "ABS", [ v ] ->
      let* x = number_of "ABS" v in
      Ok (Number (Float.abs x))
  | "INT", [ v ] ->
      let* x = number_of "INT" v in
      Ok (Number (Float.floor x))
  | "RND", [ _ ] -> Ok (Number (Random.float 1.0))
  | "LEN", [ v ] ->
      begin match v with
      | String s -> Ok (Number (float_of_int (String.length s)))
      | _ -> Error (type_error "LEN")
      end
  | "MOD", [ a; b ] ->
      let* x = number_of "MOD" a in
      let* y = number_of "MOD" b in
      let m = int_of_float x in
      let n = int_of_float y in
      if n = 0 then Error (error "division by zero")
      else Ok (Number (float_of_int (m mod n)))
  | name, _ ->
      Error (error (Printf.sprintf "wrong number of arguments for '%s'" name))

and eval_expr env = function
  | Const f -> Ok (Number f)
  | Str s -> Ok (String s)
  | Var x ->
      Option.to_result
        ~none:(error (Printf.sprintf "variable '%s' is undefined" x))
        (lookup x env)
  | ArrayAccess (name, idx) ->
      let* i = eval_expr env idx in
      let* n = number_of "array index" i in
      let* arr =
        Option.to_result
          ~none:(error (Printf.sprintf "array '%s' is undefined" name))
          (lookup name env)
      in
      begin match arr with
      | Array a ->
          let* k = element_index name n (Array.length a) in
          Ok (Number a.(k))
      | _ -> Error (error (Printf.sprintf "'%s' is not an array" name))
      end
  | Call (name, args) -> eval_call env name args
  | Binop (op, a, b) ->
      let* x = eval_expr env a in
      let* y = eval_expr env b in
      eval_binop op x y
  | Unop (op, a) ->
      let* x = eval_expr env a in
      eval_unop op x