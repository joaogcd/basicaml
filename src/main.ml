open Parser
open Machine
open Env


let program = [
  "10 LET X = 10";
  "20 PRINT X";
  "30 LET X = X - 1";
  "40 IF X THEN GOTO 20";
  "50 END";
]

let () =
  let prog = parse_program program in
  run prog empty 10


(* implement the file handling... *)