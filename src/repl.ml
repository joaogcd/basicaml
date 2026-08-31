let help_text =
  String.concat "\n"
    [ "BasicAML REPL"
    ; "Program lines: type a line number followed by a command, e.g. '10 PRINT 1'."
    ; "A lone line number deletes that line, e.g. '10'."
    ; "Commands without a line number run immediately and share variables."
    ; "Commands:"
    ; "  RUN                 run the stored program"
    ; "  LIST                list stored program lines"
    ; "  CLEAR (or NEW)      clear the program and all variables"
    ; "  QUIT (or BYE/EXIT)  leave the REPL"
    ; "  HELP                show this help"
    ]

let run
    ?(input = fun () -> In_channel.input_line stdin)
    ?(output = print_endline) ?(prompt = None) () =
  let program = ref [] in
  let env = ref Env.empty in
  let print_prompt () =
    match prompt with
    | None -> ()
    | Some p ->
        print_string p;
        flush stdout
  in
  let show_error err = output (Error.to_string err) in
  let run_program () =
    match !program with
    | [] -> output "No program loaded."
    | (first, _) :: _ ->
        begin match Machine.run ~input ~output !program !env first with
        | Ok st -> env := Machine.env st
        | Error err -> show_error err
        end
  in
  let list_program () =
    match !program with
    | [] -> output "No program loaded."
    | _ -> output (Printer.string_of_program !program)
  in
  let clear () =
    program := [];
    env := Env.empty
  in
  let insert (ln, cmd) =
    let rec go acc = function
      | [] -> List.rev_append acc [ (ln, cmd) ]
      | (m, _) :: _ as rest when ln < m ->
          List.rev_append acc ((ln, cmd) :: rest)
      | (m, _) :: rest when ln = m ->
          List.rev_append acc ((ln, cmd) :: rest)
      | item :: rest -> go (item :: acc) rest
    in
    program := go [] !program
  in
  let remove ln =
    program := List.filter (fun (m, _) -> m <> ln) !program
  in
  let store_line raw =
    match Parser.parse_line raw with
    | Error err -> show_error err
    | Ok line -> insert line
  in
  let immediate line =
    match Lexer.lex line with
    | Error err -> show_error err
    | Ok tokens ->
        begin match Parser.parse_command tokens with
        | Error err -> show_error err
        | Ok cmd ->
            begin match Machine.run ~input ~output [ (0, cmd) ] !env 0 with
            | Ok st -> env := Machine.env st
            | Error err -> show_error err
            end
        end
  in
  let split_number line =
    let n = String.length line in
    let rec go i =
      if i < n && Lexer.is_digit line.[i] then go (i + 1) else i
    in
    let i = go 0 in
    if i = 0 then None
    else
      Some (int_of_string (String.sub line 0 i), String.trim (String.sub line i (n - i)))
  in
  let first_word line =
    match String.index_opt line ' ' with
    | None -> line
    | Some i -> String.sub line 0 i
  in
  let rec loop () =
    print_prompt ();
    match input () with
    | None -> ()
    | Some raw ->
        let line = String.trim raw in
        if line = "" then loop ()
        else
          match String.uppercase_ascii (first_word line) with
          | "QUIT" | "BYE" | "EXIT" -> ()
          | "RUN" ->
              run_program ();
              loop ()
          | "LIST" ->
              list_program ();
              loop ()
          | "CLEAR" | "NEW" ->
              clear ();
              loop ()
          | "HELP" ->
              output help_text;
              loop ()
          | _ ->
              begin match split_number line with
              | Some (n, "") -> remove n
              | Some _ -> store_line line
              | None -> immediate line
              end;
              loop ()
  in
  loop ()