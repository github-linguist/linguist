# let nonsqr n = n + truncate (0.5 +. sqrt (float n));;
val nonsqr : int -> int = <fun>
# (* first 22 values (as a list) has no squares: *)
  for i = 1 to 22 do
    Printf.printf "%d " (nonsqr i)
  done;
  print_newline ();;
2 3 5 6 7 8 10 11 12 13 14 15 17 18 19 20 21 22 23 24 26 27
- : unit = ()
# (* The following check shows no squares up to one million: *)
  for i = 1 to 1_000_000 do
    let j = sqrt (float (nonsqr i)) in
      assert (j <> floor j)
  done;;
- : unit = ()
