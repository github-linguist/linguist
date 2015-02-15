let even x = (x land 1) <> 1

let middle_three_digits x =
  let s = string_of_int (abs x) in
  let n = String.length s in
  if n < 3 then failwith "need >= 3 digits" else
  if even n then failwith "need odd number of digits" else
  String.sub s (n / 2 - 1) 3

let passing = [123; 12345; 1234567; 987654321; 10001; -10001; -123; -100; 100; -12345]
let failing = [1; 2; -1; -10; 2002; -2002; 0]

let print x =
  let res =
    try (middle_three_digits x)
    with Failure e -> "failure: " ^ e
  in
  Printf.printf "%d: %s\n" x res

let () =
  print_endline "Should pass:";
  List.iter print passing;
  print_endline "Should fail:";
  List.iter print failing;
;;
