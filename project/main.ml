open Turing_types
open Turing_parsing
open Turing_display
open Turing_exec
open Bonus_complexity



let () =

  try
    let machine = validate_params Sys.argv in
    display_machine machine Sys.argv.(2);
    let tape = create_input_tape Sys.argv.(2) in
    let steps = make_step machine machine.initial tape 0 false in
    Printf.printf "The machine HALTED with %d steps\n" steps 

  with
  | InvalidParams msg ->
    Printf.printf "Error: %s: Expected: ft_turing [-h] jsonfile input\n" msg
  | InvalidArgs msg ->
    Printf.printf "Parser Error: %s\n" msg
  | ExecError msg ->
    Printf.printf "Exec Error: %s\n" msg
  | OptionBenchmark ->
    run_benchmark_suite ()
  | OptionHelp ->
    display_help ()
  | e ->
    print_endline "error"