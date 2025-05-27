open Yojson.Basic.Util

let 

let () =
  let json = Yojson.Basic.from_file "unary_sub.json" in

  let name = json |> member "name" |> to_string in 
  Printf.printf "Machine Name: %s\n" name;

  let alphabet = json |> member "alphabet" |> to_list |> List.map to_string in 
  Printf.printf "Machine alphabet: [%s]\n" (String.concat ", " alphabet);

  let blank = json |> member "blank" |> to_string in 
  Printf.printf "Machine blank: "%s"\n" blank;

  let states = json |> member "states" |> to_list |> List.map to_string in 
  Printf.printf "Machine states: [%s]\n" (String.concat ", " states);

  let initial = json |> member "initial" |> to_string in 
  Printf.printf "Machine initial: %s\n" initial;

  let finals = json |> member "finals" |> to_list |> List.map to_string in 
  Printf.printf "Machine finals: [%s]\n" (String.concat ", " finals);

  print_endline "END"