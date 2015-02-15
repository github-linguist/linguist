let a r = print_endline " > function a called"; r
let b r = print_endline " > function b called"; r

let test_and b1 b2 =
  Printf.printf "# testing (%b && %b)\n" b1 b2;
  ignore (a b1 && b b2)

let test_or b1 b2 =
  Printf.printf "# testing (%b || %b)\n" b1 b2;
  ignore (a b1 || b b2)

let test_this test =
  test true true;
  test true false;
  test false true;
  test false false;
;;

let () =
  print_endline "==== Testing and ====";
  test_this test_and;
  print_endline "==== Testing or ====";
  test_this test_or;
;;
