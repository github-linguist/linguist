let pow one mul a n =
  let rec g p x = function
  | 0 -> x
  | i ->
      g (mul p p) (if i mod 2 = 1 then mul p x else x) (i/2)
  in
  g a one n
;;

pow 1 ( * ) 2 16;;  (* 65536 *)
pow 1.0 ( *. ) 2.0 16;; (* 65536. *)

(* pow is not limited to exponentiation *)
pow 0 ( + ) 2 16;;  (* 32 *)
pow "" ( ^ ) "abc " 10;;  (* "abc abc abc abc abc abc abc abc abc abc " *)
pow [ ] ( @ ) [ 1; 2 ] 10;;  (* [1; 2; 1; 2; 1; 2; 1; 2; 1; 2; 1; 2; 1; 2; 1; 2; 1; 2; 1; 2] *)

(* Thue-Morse sequence *)
Array.init 32 (fun n -> (1 - pow 1 ( - ) 0 n) lsr 1);;

(* [|0; 1; 1; 0; 1; 0; 0; 1; 1; 0; 0; 1; 0; 1; 1; 0;
     1; 0; 0; 1; 0; 1; 1; 0; 0; 1; 1; 0; 1; 0; 0; 1|]

See http://en.wikipedia.org/wiki/Thue-Morse_sequence
*)
