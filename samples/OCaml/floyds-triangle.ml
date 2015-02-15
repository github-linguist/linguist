let ( |> ) f g x = g (f x)
let rec last = function x::[] -> x | _::tl -> last tl | [] -> raise Not_found
let rec list_map2 f l1 l2 =
  match (l1, l2) with
  | ([], _) | (_, []) -> []
  | (x::xs, y::ys) -> (f x y) :: list_map2 f xs ys

let floyd n =
  let rec aux acc cur len i j =
    if (List.length acc) = n then (List.rev acc) else
      if j = len
      then aux ((List.rev cur)::acc) [] (succ len) i 0
      else aux acc (i::cur) len (succ i) (succ j)
  in
  aux [] [] 1 1 0

let print_floyd f =
  let lens = List.map (string_of_int |> String.length) (last f) in
  List.iter (fun row ->
    print_endline (
      String.concat " " (
        list_map2 (Printf.sprintf "%*d") lens row))
  ) f

let () =
  print_floyd (floyd (int_of_string Sys.argv.(1)))
