open Complex

let pi = 4. *. atan 1.

let () =
  for n = 1 to 10 do
    Printf.printf "%2d " n;
    for k = 1 to n do
      let ret = polar 1. (2. *. pi *. float_of_int k /. float_of_int n) in
        Printf.printf "(%f + %f i)" ret.re ret.im
    done;
    print_newline ()
  done
