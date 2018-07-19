let sum_digits ~digits ~base =
  let rec aux sum x =
    if x <= 0 then sum else
    aux (sum + x mod base) (x / base)
  in
  aux 0 digits

let () =
  Printf.printf "%d %d %d %d %d\n"
    (sum_digits 1 10)
    (sum_digits 12345 10)
    (sum_digits 123045 10)
    (sum_digits 0xfe 16)
    (sum_digits 0xf0e 16)
