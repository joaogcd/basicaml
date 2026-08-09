
type expr =
  | Const of int
  | Var of string
  | Add of expr * expr
  | Sub of expr * expr

type command =
  | Let of string * expr
  | Print of expr
  | Goto of int
  | If of expr * int
  | End

type line = int * command
type program = line list

type value =
  | Number of int
