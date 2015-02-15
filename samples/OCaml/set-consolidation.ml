let join a b =
  List.fold_left (fun acc v ->
    if List.mem v acc then acc else v::acc
  ) b a

let share a b = List.exists (fun x -> List.mem x b) a

let extract p lst =
  let rec aux acc = function
  | x::xs -> if p x then Some (x, List.rev_append acc xs) else aux (x::acc) xs
  | [] -> None
  in
  aux [] lst

let consolidate sets =
  let rec aux acc = function
  | [] -> List.rev acc
  | x::xs ->
      match extract (share x) xs with
      | Some (y, ys) -> aux acc ((join x y) :: ys)
      | None -> aux (x::acc) xs
  in
  aux [] sets

let print_sets sets =
  print_string "{ ";
  List.iter (fun set ->
    print_string "{";
    print_string (String.concat " " set);
    print_string "} "
  ) sets;
  print_endline "}"

let () =
  print_sets (consolidate [["A";"B"]; ["C";"D"]]);
  print_sets (consolidate [["A";"B"]; ["B";"C"]]);
  print_sets (consolidate [["A";"B"]; ["C";"D"]; ["D";"B"]]);
  print_sets (consolidate [["H";"I";"K"]; ["A";"B"]; ["C";"D"]; ["D";"B"];
                           ["F";"G";"H"]]);
;;
