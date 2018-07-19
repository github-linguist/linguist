open Complex

let fac k n =
   let m2pi = -4.0 *. acos 0.0 in
   polar 1.0 (m2pi*.(float k)/.(float n))

let merge l r n =
   let f (k,t) x = (succ k, (mul (fac k n) x) :: t) in
   let z = List.rev (snd (List.fold_left f (0,[]) r)) in
   (List.map2 add l z) @ (List.map2 sub l z)

let fft lst =
   let rec ditfft2 a n s =
      if n = 1 then [List.nth lst a] else
      let odd = ditfft2 a (n/2) (2*s) in
      let even = ditfft2 (a+s) (n/2) (2*s) in
      merge odd even n in
   ditfft2 0 (List.length lst) 1;;

let show l =
   let pr x = Printf.printf "(%f %f) " x.re x.im in
   (List.iter pr l; print_newline ()) in
let indata = [one;one;one;one;zero;zero;zero;zero] in
show indata;
show (fft indata)
