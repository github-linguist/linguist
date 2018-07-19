let is_prime n =
  if n = 2 then true
  else if n < 2 || n mod 2 = 0 then false
  else
    let rec loop k =
      if k * k > n then true
      else if n mod k = 0 then false
      else loop (k+2)
    in loop 3
