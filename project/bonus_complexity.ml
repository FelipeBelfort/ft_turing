open Turing_types
open Turing_parsing
open Yojson.Basic.Util
open Turing_exec



type result = {
  input : string;
  input_len : int;
  steps : int;
}

type machine_entry = {
  name : string;
  machine_path : string;
  inputs : string list;
}

type benchmark_suite = machine_entry list



let run_and_record machine input =
  let input_list = List.init (String.length input) (String.get input) in
  match input_list with
  | [] -> failwith "Empty input"
  | h :: t ->
    let tape = create_input_tape input in
    let steps = make_step machine machine.initial tape 0 true in
    { input; input_len = String.length input; steps = steps }

let print_results name results =
  Printf.printf "\n=== Results for %s ===\n" name;
  Printf.printf "%-12s %-12s %-12s %-12s\n" "Input" "Length" "Steps" "Steps/n";
  List.iter (fun r ->
    Printf.printf "%-12s %-12d %-12d %-12.2f\n"
      r.input r.input_len r.steps (float_of_int r.steps /. float_of_int r.input_len)
  ) results


let estimate_big_o results =
  let open Float in
  let logs = List.map (fun r ->
    (log (of_int r.input_len), log (of_int r.steps))
  ) results in
  let n = of_int (List.length logs) in
  let sum_x = List.fold_left (fun acc (x, _) -> acc +. x) 0.0 logs in
  let sum_y = List.fold_left (fun acc (_, y) -> acc +. y) 0.0 logs in
  let sum_xy = List.fold_left (fun acc (x, y) -> acc +. x *. y) 0.0 logs in
  let sum_xx = List.fold_left (fun acc (x, _) -> acc +. x *. x) 0.0 logs in
  let k = (n *. sum_xy -. sum_x *. sum_y) /. (n *. sum_xx -. sum_x *. sum_x) in
  let b = (sum_y -. k *. sum_x) /. n in
  let c = exp b in
  Printf.printf "\n--- Complexity Estimation ---\n";
  Printf.printf "Fitted model: steps ≈ %.2f × n^%.2f\n" c k;
  let rounded =
    if k < 1.3 then "O(n)"
    else if k < 2.3 then "O(n^2)"
    else if k < 3.3 then "O(n^3)"
    else "O(n^k)"
  in
  Printf.printf "Rounded complexity: %s\n" rounded

let load_suite_from_json  =
  let json = Yojson.Basic.from_file "tests_benchmark.json" in
  json |> to_list |> List.map (fun entry ->
    {
      name = entry |> member "name" |> to_string;
      machine_path = entry |> member "machine_path" |> to_string;
      inputs = entry |> member "inputs" |> to_list |> List.map to_string
    })

let run_benchmark_suite () =
  let suite = load_suite_from_json in
  List.iter (fun entry ->
    let machine = validate_args entry.machine_path (List.hd entry.inputs) in
    let results = List.map (run_and_record machine) entry.inputs in
    print_results entry.name results;
    estimate_big_o results
  ) suite 
