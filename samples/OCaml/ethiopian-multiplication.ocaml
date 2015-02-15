(* We optimize a bit by not keeping the intermediate lists, and summing
   the right column on-the-fly, like in the C version.
   The function takes "halve" and "double" operators and "is_even" predicate as arguments,
   but also "is_zero", "zero" and "add". This allows for more general uses of the
   ethiopian multiplication. *)
let ethiopian is_zero is_even halve zero double add b a =
  let rec g a b r =
    if is_zero a
    then (r)
    else g (halve a) (double b) (if not (is_even a) then (add b r) else (r))
  in
  g a b zero
;;

let imul =
  ethiopian (( = ) 0) (fun x -> x mod 2 = 0) (fun x -> x / 2) 0 (( * ) 2) ( + );;

imul 17 34;;
(* - : int = 578 *)

(* Now, we have implemented the same algorithm as "rapid exponentiation",
   merely changing operator names *)
let ipow =
  ethiopian (( = ) 0) (fun x -> x mod 2 = 0) (fun x -> x / 2) 1 (fun x -> x*x) ( * )
;;

ipow 2 16;;
(* - : int = 65536 *)

(* still renaming operators, if "halving" is just subtracting one,
   and "doubling", adding one, then we get an addition *)
let iadd a b =
  ethiopian (( = ) 0) (fun x -> false) (pred) b (function x -> x) (fun x y -> succ y) 0 a
;;

iadd 421 1000;;
(* - : int = 1421 *)

(* One can do much more with "ethiopian multiplication",
   since the two "multiplicands" and the result may be of three different types,
   as shown by the typing system of ocaml *)

ethiopian;;
- : ('a -> bool) ->          (* is_zero *)
    ('a -> bool) ->          (* is_even *)
    ('a -> 'a) ->            (* halve *)
    'b ->                    (* zero *)
    ('c -> 'c) ->            (* double *)
    ('c -> 'b -> 'b) ->      (* add *)
    'c ->                    (* b *)
    'a ->                    (* a *)
    'b                       (* result *)
= <fun>

(* Here zero is the starting value for the accumulator of the sums
   of values in the right column in the original algorithm. But the "add"
   me do something else, see for example the RosettaCode page on
   "Exponentiation operator". *)
