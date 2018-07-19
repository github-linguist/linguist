let bitwise a b =
  Printf.printf "a and b: %d\n" (a land b);
  Printf.printf "a or b: %d\n" (a lor b);
  Printf.printf "a xor b: %d\n" (a lxor b);
  Printf.printf "not a: %d\n" (lnot a);
  Printf.printf "a lsl b: %d\n" (a lsl b);  (* left shift *)
  Printf.printf "a asr b: %d\n" (a asr b);  (* arithmetic right shift *)
  Printf.printf "a lsr b: %d\n" (a lsr b);  (* logical right shift *)
;;
