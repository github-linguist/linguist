let address_of (x:'a) : nativeint =
  if Obj.is_block (Obj.repr x) then
    Nativeint.shift_left (Nativeint.of_int (Obj.magic x)) 1 (* magic *)
  else
    invalid_arg "Can only find address of boxed values.";;

let () =
  let a = 3.14 in
  Printf.printf "%nx\n" (address_of a);;
  let b = ref 42 in
  Printf.printf "%nx\n" (address_of b);;
  let c = 17 in
  Printf.printf "%nx\n" (address_of c);; (* error, because int is unboxed *)
