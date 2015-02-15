open Big_int;;

let prime_decomposition x =
  let rec inner c p =
    if lt_big_int p (square_big_int c) then
      [p]
    else if eq_big_int (mod_big_int p c) zero_big_int then
      c :: inner c (div_big_int p c)
    else
      inner (succ_big_int c) p
  in
  inner (succ_big_int (succ_big_int zero_big_int)) x;;
