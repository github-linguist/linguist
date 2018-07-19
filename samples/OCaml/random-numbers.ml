let pi = 4. *. atan 1.;;
let random_gaussian () =
  1. +. sqrt (-2. *. log (Random.float 1.)) *. cos (2. *. pi *. Random.float 1.);;
let a = Array.init 1000 (fun _ -> random_gaussian ());;
