open Turing_types

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