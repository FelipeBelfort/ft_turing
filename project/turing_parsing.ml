open Yojson.Basic.Util
open Turing_types

exception InvalidArgs of string
exception InvalidParams of string
exception OptionHelp
exception OptionBenchmark

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



let is_input_valid input alphabet =
  let allowed = String.concat "" alphabet in
  String.for_all (fun ch -> String.contains allowed ch) input


let validate_initial_and_finals states initial finals =
  let in_states s = List.mem s states in
  if not (in_states initial) then
    false
  else if not (List.for_all in_states finals) then
    false
  else
    true

let is_symbol_in_alphabet alphabet symbol = 
  if not (List.mem symbol alphabet) then
    raise (InvalidArgs ("Symbol '" ^ symbol ^ "' is not in the alphabet"))

let is_action_valid action =
  if not (action = "RIGHT" || action = "LEFT") then
    raise (InvalidArgs ("Invalid action '" ^ action ^ "'. Must be 'RIGHT' or 'LEFT'"))

let validate_transition_properties (machine : machine) (rule : transition) : unit =
  is_symbol_in_alphabet machine.alphabet rule.read;
  is_symbol_in_alphabet machine.alphabet rule.write;
  is_action_valid rule.action

let are_transition_states_valid (states : string list) (from_state : string) (rules : transition list) : unit =
  let in_states s = List.mem s states in
  if not (in_states from_state) then
    raise (InvalidArgs ("State '" ^ from_state ^ "' is not in the list of valid states"));
  List.iter (fun rule -> 
    if not (in_states rule.to_state) then
      raise (InvalidArgs ("Target state '" ^ rule.to_state ^ "' is not in the list of valid states"))
  ) rules

let are_read_symbols_unique (from_state : string) (rules : transition list) : unit =
  let read_symbols = List.map (fun rule -> rule.read) rules in
  let rec find_duplicate seen = function
    | [] -> ()
    | x :: xs -> 
        if List.mem x seen then
          raise (InvalidArgs ("Duplicate 'read' symbol '" ^ x ^ "' found in state '" ^ from_state ^ "'"))
        else
          find_duplicate (x :: seen) xs
  in
  find_duplicate [] read_symbols

let validate_transitions (machine : machine) : bool =
  List.iter (fun (from_state, rules) ->
    are_transition_states_valid machine.states from_state rules;
    are_read_symbols_unique from_state rules;
    List.iter (fun rule -> 
      validate_transition_properties machine rule
    ) rules
  ) machine.transitions;
  true


let validate_transition_coverage states finals transitions =
  let covered = List.map fst transitions in
  let non_final_states = List.filter (fun s -> not (List.mem s finals)) states in
  let uncovered = List.filter (fun s -> not (List.mem s covered)) non_final_states in
  match uncovered with
  | [] -> true
  | lst -> false

  
let load_machine json_path =
  
  let json = Yojson.Basic.from_file json_path in
  let name = json |> member "name" |> to_string in
  let alphabet = json |> member "alphabet" |> to_list |> List.map to_string in
  let blank = json |> member "blank" |> to_string in
  let states = json |> member "states" |> to_list |> List.map to_string in
  let initial = json |> member "initial" |> to_string in
  let finals = json |> member "finals" |> to_list |> List.map to_string in
  let transitions = json |> member "transitions" |> parse_transitions in
  {
    name = name;
    alphabet = alphabet;
    blank = blank;
    states = states;
    initial = initial;
    finals = finals;
    transitions = transitions;
  }

let validate_args json_path input =
  
  try
    let machine = load_machine json_path in
    
    (* Fields aren't empty *)
    if (machine.name = "" || machine.blank = "" || machine.initial = "" || List.is_empty machine.alphabet || List.is_empty machine.states || List.is_empty machine.finals) then
      raise (InvalidArgs "The machine should not have empty fields");
    (* Blank is not part of the alphabet *)
    if not (String.contains (String.concat "" machine.alphabet) machine.blank.[0]) then raise (InvalidArgs "The blank must be part of the alphabet");
    (* Input has only char from the alphabet *)
    if not (is_input_valid input machine.alphabet) then raise (InvalidArgs "The input must contains only characters from the given alphabet");
    (* States must contains Initial and Finals *)
    if not (validate_initial_and_finals machine.states machine.initial machine.finals) then raise (InvalidArgs "The initial and finals states must be part of the states");
    (* To_state exists *)
    if not (validate_transitions machine) then raise (InvalidArgs "All to_state in transitions must exist");
    (* All States (except finals) have a transition rule *)
    if not (validate_transition_coverage machine.states machine.finals machine.transitions) then raise (InvalidArgs "All states must have a transition");    
    (* Blank is part of the input *)
    if String.contains input machine.blank.[0] then raise (InvalidArgs "The input must not contain the blank sign");
    machine

  with
  | InvalidArgs msg -> raise (InvalidArgs msg);
    (* All fields exist or is a valid json *)
  (* | _ -> raise (InvalidArgs "The machine has to be in a json format with all the correct fields") *)
  |  _ -> raise (InvalidArgs ("The file '" ^ json_path ^ "' is not in correct JSON format, or could not be opened"))


let validate_params args =
  if Array.length args = 2 && (args.(1) = "-h" || args.(1) = "--help") then
    raise OptionHelp
  else
    if Array.length args = 2 && (args.(1) = "-b" || args.(1) = "--benchmark") then
      raise OptionBenchmark;
    if Array.length args <> 3 then
      raise (InvalidArgs "")
    else
      let param1 = args.(1) in
      let param2 = args.(2) in
      if param1 = "" || param2 = "" then
        raise (InvalidArgs "Parameters must not be empty")
      else
        validate_args args.(1) args.(2)
