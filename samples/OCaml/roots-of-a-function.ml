let bracket u v =
  ((u > 0.0) && (v < 0.0)) || ((u < 0.0) && (v > 0.0));;

let xtol a b = (a = b);; (* or use |a-b| < epsilon *)

let rec regula_falsi a b fa fb f =
  if xtol a b then (a, fa) else
  let c = (fb*.a -. fa*.b) /. (fb -. fa) in
  let fc = f c in
  if fc = 0.0 then (c, fc) else
  if bracket fa fc then
    regula_falsi a c fa fc f
  else
    regula_falsi c b fc fb f;;

let search lo hi step f =
  let rec next x fx =
    if x > hi then [] else
      let y = x +. step in
      let fy = f y in
      if fx = 0.0 then
        (x,fx) :: next y fy
      else if bracket fx fy then
        (regula_falsi x y fx fy f) :: next y fy
      else
        next y fy in
  next lo (f lo);;

let showroot (x,fx) =
  Printf.printf "f(%.17f) = %.17f [%s]\n"
    x fx (if fx = 0.0 then "exact" else "approx") in
let f x = ((x -. 3.0)*.x +. 2.0)*.x  in
List.iter showroot (search (-5.0) 5.0 0.1 f);;
