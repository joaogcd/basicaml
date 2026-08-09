open Ast


type t = (string * value) list

let empty : t = []

let rec lookup name = function
  | [] -> None
  | (n, v) :: rest ->
      if n = name then Some v else lookup name rest

let rec update name value = function
  | [] -> [ (name, value) ]
  | (n, v) :: rest ->
      if n = name then (name, value) :: rest
      else (n, v) :: update name value rest
