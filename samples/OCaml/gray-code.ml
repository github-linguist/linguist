let gray_encode b =
  b lxor (b lsr 1)

let gray_decode n =
  let rec aux p n =
    if n = 0 then p
    else aux (p lxor n) (n lsr 1)
  in
  aux n (n lsr 1)

let bool_string len n =
  let s = String.make len '0' in
  let rec aux i n =
    if n land 1 = 1 then s.[i] <- '1';
    if i <= 0 then s
    else aux (pred i) (n lsr 1)
  in
  aux (pred len) n

let () =
  let s = bool_string 5 in
  for i = 0 to pred 32 do
    let g = gray_encode i in
    let b = gray_decode g in
    Printf.printf "%2d : %s => %s => %s : %2d\n" i (s i) (s g) (s b) b
  done
