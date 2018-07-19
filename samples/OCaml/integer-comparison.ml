let my_compare a b =
  if      a < b then "A is less than B"
  else if a > b then "A is greater than B"
  else if a = b then "A equals B"
  else "cannot compare NANs"

let () =
  let a = read_int ()
  and b = read_int () in
  print_endline (my_compare a b)
