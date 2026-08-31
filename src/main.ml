open Basicaml

let usage = "Usage: basicaml [OPTIONS] [FILE]"

let version = "0.1.0"

let help =
  String.concat "\n"
    [ "BasicAML - a simple BASIC interpreter in OCaml"
    ; ""
    ; usage
    ; ""
    ; "With no FILE, starts the interactive REPL."
    ; ""
    ; "Options:"
    ; "  -h, --help  show this help and exit"
    ; "  --version   show version and exit"
    ]

let read_lines path =
  let channel = open_in path in
  Fun.protect
    ~finally:(fun () -> close_in channel)
    (fun () ->
      channel
      |> In_channel.input_all
      |> String.split_on_char '\n'
      |> List.map String.trim)

let print_error err =
  Printf.eprintf "basicaml: %s\n" (Error.to_string err)

let () =
  Random.self_init ();
  match Sys.argv with
  | [| _ |] -> Repl.run ~prompt:(Some "basicaml> ") ()
  | [| _; arg |] when arg = "-h" || arg = "--help" ->
      print_endline help
  | [| _; "--version" |] ->
      Printf.printf "basicaml %s\n" version
  | [| _; path |] ->
      begin try
        let lines = read_lines path in
        begin match Parser.parse_program lines with
        | Error err ->
            print_error err;
            exit 1
        | Ok [] -> ()
        | Ok ((first, _) :: _ as prog) ->
            begin match Machine.run prog Env.empty first with
            | Ok _ -> ()
            | Error err ->
                print_error err;
                exit 1
            end
        end
      with Sys_error msg ->
        Printf.eprintf "basicaml: cannot read '%s': %s\n" path msg;
        exit 1
      end
  | _ ->
      print_endline usage;
      exit 1