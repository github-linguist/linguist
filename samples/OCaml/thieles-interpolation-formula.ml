let xv, fv = fst, snd

let rec rdiff a l r =
   if l > r then 0.0 else
   if l = r then fv a.(l) else
   if l+1 = r then (xv a.(l) -. xv a.(r)) /. (fv a.(l) -. fv a.(r)) else
   (xv a.(l) -. xv a.(r)) /. (rdiff a l (r-1) -. rdiff a (l+1) r) +. rdiff a (l+1) (r-1)

let rec thiele x a a0 k n =
   if k = n then 1.0 else
   rdiff a a0 (a0+k) -. rdiff a a0 (a0+k-2) +. (x -. xv a.(a0+k)) /. thiele x a a0 (k+1) n

let interpolate x a n =
   let m = Array.length a in
   let dist i = abs_float (x -. xv a.(i)) in
   let nearer i j = if dist j < dist i then j else i in
   let rec closest i j = if j = m then i else closest (nearer i j) (j+1) in
   let c = closest 0 1 in
   let c' = if c < n/2 then 0 else if c > m-n then m-n else c-(n/2) in
   thiele x a c' 0 n

let table a b n f =
   let g i =
      let x = a +. (b-.a)*.(float i)/.(float (n-1)) in
      (f x, x) in
   Array.init n g

let [sin_tab; cos_tab; tan_tab] = List.map (table 0.0 1.55 32) [sin; cos; tan]

let test n =
   Printf.printf "\nDegree %d interpolation:\n" n;
   Printf.printf "6*arcsin(0.5) = %.15f\n" (6.0*.(interpolate 0.5 sin_tab n));
   Printf.printf "3*arccos(0.5) = %.15f\n" (3.0*.(interpolate 0.5 cos_tab n));
   Printf.printf "4*arctan(1.0) = %.15f\n" (4.0*.(interpolate 1.0 tan_tab n));;

List.iter test [8; 12; 16]
