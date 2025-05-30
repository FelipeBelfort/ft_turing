open Yojson.Basic.Util
open Turing_types
open Turing_errors
open Turing_display
open Turing_exec


let rec make_step machine state tape =

  if List.mem state machine.finals then
    print_endline "END OF THE PROGRAM!"
  else
    let trans = find_transition machine.transitions state tape.curr in
    display_step state trans tape;
    let tape' =
    match trans.action with
    | "LEFT" -> move_left tape trans machine.blank
    | "RIGHT" -> move_right tape trans machine.blank
    | _ -> raise (ExecError "Invalid action in the transition");
    in
    make_step machine trans.to_state tape'


let () =

  try
    let machine = validate_params Sys.argv in
    display_machine machine Sys.argv.(2);
    let tape = create_input_tape Sys.argv.(2) in
    make_step machine machine.initial tape;

  with
  | InvalidParams msg ->
    Printf.printf "Error: %s: Expected: ft_turing [-h] jsonfile input\n" msg
  | InvalidArgs msg ->
    Printf.printf "Parser Error: %s\n" msg
  | ExecError msg ->
    Printf.printf "Exec Error: %s\n" msg
  | OptionHelp ->
    print_endline "usage: ft_turing [-h] jsonfile input\n
positional arguments:
  jsonfile\t json description of the machine\n
  input\t\t input of the machine\n
optional arguments:
  -h, --help\t show this help message and exit"
  | e ->
    print_endline "error"