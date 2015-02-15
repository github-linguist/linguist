exception Found of int

let () =
  (* search the first number in a list greater than 50 *)
  try
    let nums = [36; 23; 44; 51; 28; 63; 17] in
    List.iter (fun v -> if v > 50 then raise(Found v)) nums;
    print_endline "nothing found"
  with Found res ->
    Printf.printf "found %d\n" res
