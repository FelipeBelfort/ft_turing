open Yojson.Basic.Util

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

exception InvalidParams of string
exception InvalidArgs of string
exception OptionHelp

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


let validate_transitions (states : string list) (transitions : (string * transition list) list) : bool =
  let in_states s = List.mem s states in
  List.for_all (fun (from_state, rules) ->
    in_states from_state &&
    List.for_all (fun rule -> in_states rule.to_state) rules
  ) transitions


let validate_transition_coverage states finals transitions =
  let covered = List.map fst transitions in
  let non_final_states = List.filter (fun s -> not (List.mem s finals)) states in
  let uncovered = List.filter (fun s -> not (List.mem s covered)) non_final_states in
  match uncovered with
  | [] -> true
  | lst -> false


let validate_args json_path input =
  
  try
    let json = Yojson.Basic.from_file json_path in
    let name = json |> member "name" |> to_string in
    let alphabet = json |> member "alphabet" |> to_list |> List.map to_string in
    let blank = json |> member "blank" |> to_string in
    let states = json |> member "states" |> to_list |> List.map to_string in
    let initial = json |> member "initial" |> to_string in
    let finals = json |> member "finals" |> to_list |> List.map to_string in
    let transitions = json |> member "transitions" |> parse_transitions in
    
    (* Fields aren't empty *)
    if (name = "" || blank = "" || initial = "" || List.is_empty alphabet || List.is_empty states || List.is_empty finals) then
      raise (InvalidArgs "The machine should not have empty fields");
    (* Blank is not part of the alphabet *)
    if not (String.contains (String.concat "" alphabet) blank.[0]) then raise (InvalidArgs "The blank must be part of the alphabet");
    (* Input has only char from the alphabet *)
    if not (is_input_valid input alphabet) then raise (InvalidArgs "The input must contains only characters from the given alphabet");
    (* States must contains Initial and Finals *)
    if not (validate_initial_and_finals states initial finals) then raise (InvalidArgs "The initial and finals states must be part of the states");
    (* To_state exists *)
    if not (validate_transitions states transitions) then raise (InvalidArgs "All to_state in transitions must exist");
    (* All States (except finals) have a transition rule *)
    if not (validate_transition_coverage states finals transitions) then raise (InvalidArgs "All states must have a transition");
    
    (* Blank is part of the input *)
    if String.contains input blank.[0] then raise (InvalidArgs "The input must not contains the blank sign");
    {
      name = name;
      alphabet = alphabet;
      blank = blank;
      states = states;
      initial = initial;
      finals = finals;
      transitions = transitions;
    }

  with
  | InvalidArgs msg -> raise (InvalidArgs msg);
    (* All fields exist or is a valid json *)
  | _ -> raise (InvalidArgs "The machine has to be in a json format with all the correct fields")


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
