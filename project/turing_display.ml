open Turing_types

let display_machine machine input =
  let size = 65 in
  print_endline (String.init size (fun _ -> '#'));
  print_endline (String.init size (fun _ -> '#'));
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
  ) machine.transitions;
  print_endline (String.init size (fun _ -> '#'));
  print_endline input;
  print_endline (String.init size (fun _ -> '#'))


let display_step state trans tape =
  Printf.printf "[%s<%s>%s] (%s, %s) -> (%s, %s, %s)\n" 
    (String.concat "" (List.rev tape.left)) tape.curr (String.concat "" tape.right) state tape.curr trans.to_state trans.write trans.action
