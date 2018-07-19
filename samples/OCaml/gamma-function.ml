let e = exp 1.
let pi = 4. *. atan 1.
let sqrttwopi = sqrt (2. *. pi)

module Lanczos = struct
  (* Lanczos method *)
  (* Coefficients used by the GNU Scientific Library *)
  let g = 7.
  let c = [|0.99999999999980993; 676.5203681218851; -1259.1392167224028;
	    771.32342877765313; -176.61502916214059; 12.507343278686905;
	    -0.13857109526572012; 9.9843695780195716e-6; 1.5056327351493116e-7|]

  let rec ag z d =
    if d = 0 then c.(0) +. ag z 1
    else if d < 8 then c.(d) /. (z +. float d) +. ag z (succ d)
    else c.(d) /. (z +. float d)

  let gamma z =
    let z = z -. 1. in
    let p = z +. g +. 0.5 in
    sqrttwopi *. p ** (z +. 0.5) *. exp (-. p) *. ag z 0
end

module Stirling = struct
  (* Stirling method *)
  let gamma z =
    sqrttwopi /. sqrt z *. (z /. e) ** z

end

module Stirling2 = struct
  (* Extended Stirling method seen in Abramowitz and Stegun *)
  let d = [|1./.12.; 1./.288.; -139./.51840.; -571./.2488320.|]

  let rec corr z x n =
    if n < Array.length d - 1 then d.(n) /. x +. corr z (x *. z) (succ n)
    else d.(n) /. x

  let gamma z = Stirling.gamma z *. (1. +. corr z z 0)
end

let mirror gma z =
  if z > 0.5 then gma z
  else pi /. sin (pi *. z) /. gma (1. -. z)

let _ =
  Printf.printf "z\t\tLanczos\t\tStirling\tStirling2\n";
  for i = 1 to 20 do
    let z = float i /. 10. in
    Printf.printf "%-10.8g\t%10.8e\t%10.8e\t%10.8e\n"
    		  z
		  (mirror Lanczos.gamma z)
		  (mirror Stirling.gamma z)
		  (mirror Stirling2.gamma z)
  done;
  for i = 1 to 7 do
    let z = 10. *. float i in
    Printf.printf "%-10.8g\t%10.8e\t%10.8e\t%10.8e\n"
    		  z
		  (Lanczos.gamma z)
		  (Stirling.gamma z)
		  (Stirling2.gamma z)
  done
