let a = (3.0, 4.0, 5.0)
let b = (4.0, 3.0, 5.0)
let c = (-5.0, -12.0, -13.0)

let string_of_vector (x,y,z) =
  Printf.sprintf "(%g, %g, %g)" x y z

let dot (a1, a2, a3) (b1, b2, b3) =
  (a1 *. b1) +. (a2 *. b2) +. (a3 *. b3)

let cross (a1, a2, a3) (b1, b2, b3) =
  (a2 *. b3 -. a3 *. b2,
   a3 *. b1 -. a1 *. b3,
   a1 *. b2 -. a2 *. b1)

let scalar_triple a b c =
  dot a (cross b c)

let vector_triple a b c =
  cross a (cross b c)

let () =
  Printf.printf "a: %s\n" (string_of_vector a);
  Printf.printf "b: %s\n" (string_of_vector b);
  Printf.printf "c: %s\n" (string_of_vector c);
  Printf.printf "a . b = %g\n" (dot a b);
  Printf.printf "a x b = %s\n" (string_of_vector (cross a b));
  Printf.printf "a . (b x c) = %g\n" (scalar_triple a b c);
  Printf.printf "a x (b x c) = %s\n" (string_of_vector (vector_triple a b c));
;;
