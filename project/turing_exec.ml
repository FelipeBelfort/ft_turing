open Turing_types
open Turing_display

exception ExecError of string


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



let rec make_step machine state tape steps testing =

  let steps = steps + 1 in
  if List.mem state machine.finals then
    steps
  else
    let trans = find_transition machine.transitions state tape.curr in
    if not testing then
    display_step state trans tape;
    let tape' =
    match trans.action with
    | "LEFT" -> move_left tape trans machine.blank
    | "RIGHT" -> move_right tape trans machine.blank
    | _ -> raise (ExecError "Invalid action in the transition");
    in
    make_step machine trans.to_state tape' steps testing
