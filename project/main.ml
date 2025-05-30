open Yojson.Basic.Util

exception InvalidParams of string
exception InvalidArgs of string
exception ExecError of string
exception OptionHelp

type tape = {
  left: string list;
  curr: string;
  right: string list;
} 

type transition = {
  read: string;
  to_state: string;
  write: string;
  action: string;
}

type machine = {
  name: string;
  alphabet: string list;
  blank: string;
  states: string list;
  initial: string;
  finals: string list;
  transitions: (string * transition list) list;
}


let parse_transition json =
  {
    read = json |> member "read" |> to_string;
    to_state = json |> member "to_state" |> to_string;
    write = json |> member "write" |> to_string;
    action = json |> member "action" |> to_string;
  }


let parse_transitions json =
  json |> to_assoc
       |> List.map (fun (state, list_json) ->
            let transitions = list_json |> to_list |> List.map parse_transition in
            (state, transitions)
          )


let create_machine json =
  {
    name = json |> member "name" |> to_string;
    alphabet = json |> member "alphabet" |> to_list |> List.map to_string;
    blank = json |> member "blank" |> to_string;
    states = json |> member "states" |> to_list |> List.map to_string;
    initial = json |> member "initial" |> to_string;
    finals = json |> member "finals" |> to_list |> List.map to_string;
    transitions = json |> member "transitions" |> parse_transitions;
  }


let validate_args json_path input =
  
  let json = Yojson.Basic.from_file json_path in

  let name = json |> member "name" |> to_string in
  let alphabet = json |> member "alphabet" |> to_list |> List.map to_string in
  let blank = json |> member "blank" |> to_string in
  let states = json |> member "states" |> to_list |> List.map to_string in
  let initial = json |> member "initial" |> to_string in
  let finals = json |> member "finals" |> to_list |> List.map to_string in
  
  (* Fields aren't empty *)
  if (name = "" || blank = "" || initial = "" || List.is_empty alphabet || List.is_empty states || List.is_empty finals) then
    raise (InvalidArgs "The machine should not have empty fields") else ();
  (* Blank is not part of the alphabet *)
  if not (String.contains (String.concat "" alphabet) blank.[0]) then raise (InvalidArgs "The blank must be part of the alphabet") else ();
  (* Input has only char from the alphabet *)
  (* States must contains Initial and Finals *)
  (* All fields exist or is a valid json *)
  (* Transitions exists *)
  (* All States (except finals) have a transition rule *)

  (* Blank is part of the input *)
  if String.contains input blank.[0] then raise (InvalidArgs "The input must not contains the blank sign") else ()


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
        validate_args args.(1) args.(2)


let display_machine machine =
  
  Printf.printf "Machine Name: %s\n" machine.name;
  Printf.printf "Machine alphabet: [%s]\n" (String.concat ", " machine.alphabet);
  Printf.printf "Machine blank: '%s'\n" machine.blank;
  Printf.printf "Machine states: [%s]\n" (String.concat ", " machine.states);
  Printf.printf "Machine initial: %s\n" machine.initial;
  Printf.printf "Machine finals: [%s]\n" (String.concat ", " machine.finals);
  List.iter (fun (state, transitions) ->
    List.iter (fun t ->
      Printf.printf "(%s, %s) -> (%s, %s, %s)\n" 
        state t.read t.to_state t.write t.action 
    ) transitions
  ) machine.transitions


let find_transition transitions state symbol =
  try
    let options = List.assoc state transitions in
    List.find (fun t -> t.read = symbol) options
  with _ -> raise (ExecError ("No transition for state " ^ state ^ " with the symbol '" ^ symbol ^ "'"))


let create_input_tape input =
  match List.init (String.length input) (fun i -> String.make 1 input.[i]) with
  | [] -> raise (ExecError "Input can not be empty")
  | head :: tail -> {left = []; curr = head; right = tail}


let move_left tape trans blank =
  match tape.left with
  | [] -> { left = []; curr = blank; right = trans.write :: tape.right }
  | x :: xs -> { left = xs; curr = x; right = trans.write :: tape.right }


let move_right tape trans blank =
  match tape.right with
  | [] -> { left = trans.write :: tape.left; curr = blank; right = [] }
  | x :: xs -> { left = trans.write :: tape.left; curr = x; right = xs }


let rec make_step machine state tape =

  if List.mem state machine.finals then
    print_endline "END OF THE PROGRAM!"
  else
    let trans = find_transition machine.transitions state tape.curr in
    Printf.printf "[%s<%s>%s] (%s, %s) -> (%s, %s, %s)\n" 
      (String.concat "" (List.rev tape.left)) tape.curr (String.concat "" tape.right) state tape.curr trans.to_state trans.write trans.action;
    let tape' =
    match trans.action with
    | "LEFT" -> move_left tape trans machine.blank
    | "RIGHT" -> move_right tape trans machine.blank
    | _ -> raise (ExecError "Invalid action in the transition");
    in
    make_step machine trans.to_state tape'


let () =

  try
    validate_params Sys.argv;

    let json = Yojson.Basic.from_file "unary_sub.json" in
    
    let machine = create_machine json in
    display_machine machine;
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