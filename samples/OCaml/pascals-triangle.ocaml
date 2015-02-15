(* generate next row from current row *)
let next_row row =
  List.map2 (+) ([0] @ row) (row @ [0])

(* returns the first n rows *)
let pascal n =
  let rec loop i row =
    if i = n then []
    else row :: loop (i+1) (next_row row)
  in loop 0 [1]
