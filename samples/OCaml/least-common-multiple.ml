let rec gcd u v =
  if v <> 0 then (gcd v (u mod v))
  else (abs u)

let lcm m n =
  match m, n with
  | 0, _ | _, 0 -> 0
  | m, n -> abs (m * n) / (gcd m n)

let () =
  Printf.printf "lcm(35, 21) = %d\n" (lcm 21 35)
