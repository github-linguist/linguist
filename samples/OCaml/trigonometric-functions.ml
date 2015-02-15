let pi = 4. *. atan 1.

let radians = pi /. 4.
let degrees = 45.;;

Printf.printf "%f %f\n" (sin radians) (sin (degrees *. pi /. 180.));;
Printf.printf "%f %f\n" (cos radians) (cos (degrees *. pi /. 180.));;
Printf.printf "%f %f\n" (tan radians) (tan (degrees *. pi /. 180.));;
let arcsin = asin (sin radians);;
Printf.printf "%f %f\n" arcsin (arcsin *. 180. /. pi);;
let arccos = acos (cos radians);;
Printf.printf "%f %f\n" arccos (arccos *. 180. /. pi);;
let arctan = atan (tan radians);;
Printf.printf "%f %f\n" arctan (arctan *. 180. /. pi);;
