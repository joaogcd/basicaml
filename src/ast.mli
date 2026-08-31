type binop =
  | Add
  | Sub
  | Mul
  | Div
  | Eq
  | Ne
  | Lt
  | Le
  | Gt
  | Ge
  | And
  | Or

type unop =
  | Neg
  | Not

type expr =
  | Const of float
  | Str of string
  | Var of string
  | ArrayAccess of string * expr
  | Call of string * expr list
  | Binop of binop * expr * expr
  | Unop of unop * expr

type target =
  | Var of string
  | ArrayElement of string * expr

type command =
  | Let of target * expr
  | Print of expr
  | Goto of int
  | If of expr * command * command option
  | Input of string
  | For of string * expr * expr * expr option
  | Next of string option
  | Gosub of int
  | Return
  | Dim of string * expr
  | Rem
  | End

type line = int * command

type program = line list

type value =
  | Number of float
  | String of string
  | Array of float array