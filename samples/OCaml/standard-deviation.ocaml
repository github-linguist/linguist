let sqr x = x *. x

let stddev l =
  let n, sx, sx2 =
    List.fold_left
      (fun (n, sx, sx2) x -> succ n, sx +. x, sx2 +. sqr x)
      (0, 0., 0.) l
  in
  sqrt ((sx2 -. sqr sx /. float n) /. float n)

let _ =
  let l = [ 2.;4.;4.;4.;5.;5.;7.;9. ] in
  Printf.printf "List: ";
  List.iter (Printf.printf "%g  ") l;
  Printf.printf "\nStandard deviation: %g\n" (stddev l)
