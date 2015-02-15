(* There is no undefined value in OCaml,
   but if you really need this you can use the built-in "option" type.
   It is defined like this: type 'a option = None | Some of 'a *)

let inc = function
  Some n -> Some (n+1)
| None -> failwith "Undefined argument";;

inc (Some 0);;
(* - : value = Some 1 *)

inc None;;
(* Exception: Failure "Undefined argument". *)
