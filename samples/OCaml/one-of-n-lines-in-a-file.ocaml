let one_of_n n =
  let rec aux i r =
    if i >= n then r else
      if Random.int (i + 1) = 0
      then aux (succ i) i
      else aux (succ i) r
  in
  aux 1 0

let test ~n ~trials =
  let ar = Array.make n 0 in
  for i = 1 to trials do
    let d = one_of_n n in
    ar.(d) <- succ ar.(d)
  done;
  Array.iter (Printf.printf " %d") ar;
  print_newline ()

let () =
  Random.self_init ();
  test ~n:10 ~trials:1_000_000
