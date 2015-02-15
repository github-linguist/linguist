let _ =
  let a = read_int ()
  and b = read_int () in

  Printf.printf "a + b = %d\n" (a + b);
  Printf.printf "a - b = %d\n" (a - b);
  Printf.printf "a * b = %d\n" (a * b);
  Printf.printf "a / b = %d\n" (a / b);    (* truncates towards 0 *)
  Printf.printf "a mod b = %d\n" (a mod b) (* same sign as first operand *)
