let bin_of_int d =
  if d < 0 then invalid_arg "bin_of_int" else
  if d = 0 then "0" else
  let rec aux acc d =
    if d = 0 then acc else
    aux (string_of_int (d land 1) :: acc) (d lsr 1)
  in
  String.concat "" (aux [] d)

let () =
  let d = read_int () in
  Printf.printf "%8s\n" (bin_of_int d)
