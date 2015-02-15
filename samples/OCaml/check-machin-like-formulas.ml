open Num;; (* use exact rationals for results *)

let tadd p q = (p +/ q) // ((Int 1) -/ (p */ q)) in

(* tan(n*arctan(a/b)) *)
let rec tan_expr (n,a,b) =
  if n = 1 then (Int a)//(Int b) else
  if n = -1 then (Int (-a))//(Int b) else
    let m = n/2 in
    let tm = tan_expr (m,a,b) in
    let m2 = tadd tm tm and k = n-m-m in
    if k = 0 then m2 else tadd (tan_expr (k,a,b)) m2 in

let verify (k, tlist) =
  Printf.printf "Testing: pi/%d = " k;
  let t_str = List.map (fun (x,y,z) -> Printf.sprintf "%d*atan(%d/%d)" x y z) tlist in
  print_endline (String.concat " + " t_str);
  let ans_terms = List.map tan_expr tlist in
  let answer = List.fold_left tadd (Int 0) ans_terms in
  Printf.printf "  tan(RHS) is %s\n" (if answer = (Int 1) then "one" else "not one") in

(* example: prog 4 5 29 278 7 3 79 represents pi/4 = 5*atan(29/278) + 7*atan(3/79) *)
let args = Sys.argv in
let nargs = Array.length args in
let v k = int_of_string args.(k) in
let rec triples n =
  if n+2 > nargs-1 then []
  else (v n, v (n+1), v (n+2)) :: triples (n+3) in
if nargs > 4 then
let dat = (v 1, triples 2) in
verify dat
else
List.iter verify [
  (4,[(1,1,2);(1,1,3)]);
  (4,[(2,1,3);(1,1,7)]);
  (4,[(4,1,5);(-1,1,239)]);
  (4,[(5,1,7);(2,3,79)]);
  (4,[(5,29,278);(7,3,79)]);
  (4,[(1,1,2);(1,1,5);(1,1,8)]);
  (4,[(4,1,5);(-1,1,70);(1,1,99)]);
  (4,[(5,1,7);(4,1,53);(2,1,4443)]);
  (4,[(6,1,8);(2,1,57);(1,1,239)]);
  (4,[(8,1,10);(-1,1,239);(-4,1,515)]);
  (4,[(12,1,18);(8,1,57);(-5,1,239)]);
  (4,[(16,1,21);(3,1,239);(4,3,1042)]);
  (4,[(22,1,28);(2,1,443);(-5,1,1393);(-10,1,11018)]);
  (4,[(22,1,38);(17,7,601);(10,7,8149)]);
  (4,[(44,1,57);(7,1,239);(-12,1,682);(24,1,12943)]);
  (4,[(88,1,172);(51,1,239);(32,1,682);(44,1,5357);(68,1,12943)]);
  (4,[(88,1,172);(51,1,239);(32,1,682);(44,1,5357);(68,1,12944)])
]
