#load "str.cma"

let range a b =
  if b < a then invalid_arg "range";
  let rec aux i acc =
    if i = b then List.rev(i::acc)
    else aux (succ i) (i::acc)
  in
  aux a []

let parse_piece s =
  try Scanf.sscanf s "%d-%d" (fun a b -> range a b)
  with _ -> [int_of_string s]

let range_expand rng =
  let ps = Str.split (Str.regexp_string ",") rng in
  List.flatten (List.map parse_piece ps)

let () =
  let rng = "-6,-3--1,3-5,7-11,14,15,17-20" in
  let exp = range_expand rng in
  List.iter (Printf.printf " %d") exp;
  print_newline()
