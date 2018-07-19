let rec input () =
  let s = read_line () in
  try
    if String.length s <> 4 then raise Exit;
    String.iter (function
      | '1'..'9' -> ()
      | _ -> raise Exit
    ) s;
    let t = [ s.[0]; s.[1]; s.[2]; s.[3] ] in
    let _ = List.fold_left  (* reject entry with duplication *)
              (fun ac b -> if List.mem b ac then raise Exit; (b::ac))
              [] t in
    List.map (fun c -> int_of_string (String.make 1 c)) t
  with Exit ->
    prerr_endline "That is an invalid entry. Please try again.";
    input ()
;;

let print_score g t =
  let bull = ref 0 in
  List.iter2 (fun x y ->
    if x = y then incr bull
  ) g t;
  let cow = ref 0 in
  List.iter (fun x ->
    if List.mem x t then incr cow
  ) g;
  cow := !cow - !bull;
  Printf.printf "%d bulls, %d cows\n%!" !bull !cow
;;

let () =
  Random.self_init ();
  let rec mkgoal acc = function 4 -> acc
  | i ->
      let n = succ(Random.int 9) in
      if List.mem n acc
      then mkgoal acc i
      else mkgoal (n::acc) (succ i)
  in
  let g = mkgoal [] 0 in
  let found = ref false in
  while not !found do
    let t = input () in
    if t = g
    then found := true
    else print_score g t
  done;
  print_endline "Congratulations you guessed correctly";
;;
