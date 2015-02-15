let generate_brackets n =
  let rec aux i acc =
    if i <= 0 then acc else
      aux (pred i) ('['::']'::acc)
  in
  let brk = aux n [] in
  List.sort (fun _ _ -> (Random.int 3) - 1) brk

let is_balanced brk =
  let rec aux = function
    | [], 0 -> true
    | '['::brk, level -> aux (brk, succ level)
    | ']'::brk, 0 -> false
    | ']'::brk, level -> aux (brk, pred level)
    | _ -> assert false
  in
  aux (brk, 0)

let () =
  let n = int_of_string Sys.argv.(1) in
  Random.self_init();
  let brk = generate_brackets n in
  List.iter print_char brk;
  Printf.printf " %B\n" (is_balanced brk);
;;
