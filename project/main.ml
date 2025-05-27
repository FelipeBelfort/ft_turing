open Yojson.Basic.Util

exception InvalidParams of string
exception InvalidArgs of string
exception OptionHelp

let validate_args json_path input =
  
  let json = Yojson.Basic.from_file json_path in

  let name = json |> member "name" |> to_string in
  let alphabet = json |> member "alphabet" |> to_list |> List.map to_string in
  let blank = json |> member "blank" |> to_string in
  let states = json |> member "states" |> to_list |> List.map to_string in
  let initial = json |> member "initial" |> to_string in
  let finals = json |> member "finals" |> to_list |> List.map to_string in
  
  if (name = "" || blank = "" || initial = "" || Array.length alphabet = 0 || Array.length states = 0 || Array.length finals = 0) then
    raise (InvalidArgs "The machine should not have empty fields") else ();
  if not String.contains alphabet blank.[0] then raise (InvalidArgs "The blank must be part of the alphabet") else ();
  if String.contains input blank.[0] then raise (InvalidArgs "The input must not contains the blank sign") else ();


let validate_params args =
  if Array.length args = 2 && (args.(1) = "-h" || args.(1) = "--help") then
    raise OptionHelp
  else
    if Array.length args <> 3 then
      raise (InvalidParams "")
    else
      let param1 = args.(1) in
      let param2 = args.(2) in
      if param1 = "" || param2 = "" then
        raise (InvalidParams "Parameters must not be empty")
      else
        validate_args args.(1) args.(2);

let () =

  try
    validate_params Sys.argv;

    let json = Yojson.Basic.from_file "unary_sub.json" in

    let name = json |> member "name" |> to_string in 
    Printf.printf "Machine Name: %s\n" name;

    let alphabet = json |> member "alphabet" |> to_list |> List.map to_string in 
    Printf.printf "Machine alphabet: [%s]\n" (String.concat ", " alphabet);

    let blank = json |> member "blank" |> to_string in 
    Printf.printf "Machine blank: '%s'\n" blank;

    let states = json |> member "states" |> to_list |> List.map to_string in 
    Printf.printf "Machine states: [%s]\n" (String.concat ", " states);

    let initial = json |> member "initial" |> to_string in 
    Printf.printf "Machine initial: %s\n" initial;

    let finals = json |> member "finals" |> to_list |> List.map to_string in 
    Printf.printf "Machine finals: [%s]\n" (String.concat ", " finals);

    print_endline "END";

  with
  | InvalidParams msg ->
    Printf.printf "Error: %s: Expected: ft_turing [-h] jsonfile input\n" msg
  | InvalidArgs msg ->
    Printf.printf "Parser Error: %s\n" msg
  | OptionHelp ->
    print_endline "usage: ft_turing [-h] jsonfile input\n
positional arguments:
  jsonfile\t json description of the machine\n
  input\t\t input of the machine\n
optional arguments:
  -h, --help\t show this help message and exit"
  | e ->
    print_endline "error"