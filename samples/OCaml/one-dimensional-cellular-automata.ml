let get g i =
  try g.(i)
  with _ -> 0

let next_cell g i =
  match get g (i-1), get g (i), get g (i+1) with
  | 0, 0, 0 -> 0
  | 0, 0, 1 -> 0
  | 0, 1, 0 -> 0
  | 0, 1, 1 -> 1
  | 1, 0, 0 -> 0
  | 1, 0, 1 -> 1
  | 1, 1, 0 -> 1
  | 1, 1, 1 -> 0
  | _ -> assert(false)

let next g =
  let old_g = Array.copy g in
  for i = 0 to pred(Array.length g) do
    g.(i) <- (next_cell old_g i)
  done

let print_g g =
  for i = 0 to pred(Array.length g) do
    if g.(i) = 0
    then print_char '_'
    else print_char '#'
  done;
  print_newline()
