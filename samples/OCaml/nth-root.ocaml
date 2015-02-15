let nthroot ~n ~a ?(tol=0.001) () =
   let nf = float n in let nf1 = nf -. 1.0 in
   let rec iter x =
      let x' = (nf1 *. x +. a /. (x ** nf1)) /. nf in
      if tol > abs_float (x -. x') then x' else iter x' in
   iter 1.0
;;

let () =
  Printf.printf "%g\n" (nthroot 10 (7131.5 ** 10.0) ());
  Printf.printf "%g\n" (nthroot 5 34.0 ());
;;
