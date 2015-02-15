let rec sorted = function
  | [] -> (true)
  | x::y::_ when x > y -> (false)
  | x::xs -> sorted xs

let rev_until_max li =
  let rec aux acc greater prefix suffix = function
  | x::xs when x > greater -> aux (x::acc) x acc xs xs
  | x::xs -> aux (x::acc) greater prefix suffix xs
  | [] -> (greater, (prefix @ suffix))
  in
  aux [] min_int [] li li

let pancake_sort li =
  let rec aux i li suffix =
    let greater, li = rev_until_max li in
    let suffix = greater :: suffix
    and li = List.rev li in
    if sorted li
    then (li @ suffix), i
    else aux (succ i) li suffix
  in
  aux 0 li []

let print_list li =
  List.iter (Printf.printf " %d") li;
  print_newline()

let make_rand_list n bound =
  let rec aux acc i =
    if i >= n then (acc)
    else aux ((Random.int bound)::acc) (succ i)
  in
  aux [] 0

let () =
  Random.self_init();
  let li = make_rand_list 8 100 in
  print_list li;
  let res, n = pancake_sort li in
  print_list res;
  Printf.printf " sorted in %d loops\n" n;
;;
