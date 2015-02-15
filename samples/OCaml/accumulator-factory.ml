let accumulator sum0 =
  let sum = ref sum0 in
  fun n ->
    sum := !sum +. n;
    !sum;;

let _ =
  let x = accumulator 1.0 in
  ignore (x 5.0);
  let _ = accumulator 3.0 in
  Printf.printf "%g\n" (x 2.3)
;;
