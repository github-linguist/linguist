let lst = [ -7; 1; 5; 2; -4; 3; 0 ]
let sum = List.fold_left ( + ) 0 lst

let () =
  let rec aux acc i left right = function
  | x::xs ->
      let right = right - x in
      let acc = if left = right then i::acc else acc in
      aux acc (succ i) (left + x) right xs
  | [] -> List.rev acc
  in
  let res = aux [] 0 0 sum lst in
  print_string "Results:";
  List.iter (Printf.printf " %d") res;
  print_newline()
