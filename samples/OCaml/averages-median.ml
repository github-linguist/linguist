(* note: this modifies the input array *)
let median array =
  let len = Array.length array in
    Array.sort compare array;
    (array.((len-1)/2) +. array.(len/2)) /. 2.0;;

let a = [|4.1; 5.6; 7.2; 1.7; 9.3; 4.4; 3.2|];;
median a;;
let a = [|4.1; 7.2; 1.7; 9.3; 4.4; 3.2|];;
median a;;
