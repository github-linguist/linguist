let rec agm a g tol =
  if tol > abs_float (a -. g) then a else
  agm (0.5*.(a+.g)) (sqrt (a*.g)) tol

let _ = Printf.printf "%.16f\n" (agm 1.0 (sqrt 0.5) 1e-15)
