# let x = 2.0;;

# let y = 4.0;;

# let z = x +. y;;

# let coll = [ x; y; z];;

# let inv_coll = List.map (fun x -> 1.0 /. x) coll;;

# let multiplier n1 n2 = (fun t -> n1 *. n2 *. t);;

(* create a list of new functions *)
# let func_list = List.map2 (fun n inv -> (multiplier n inv)) coll inv_coll;;

# List.map (fun f -> f 0.5) func_list;;
- : float list = [0.5; 0.5; 0.5]

(* or just apply the generated function immediately... *)
# List.map2 (fun n inv -> (multiplier n inv) 0.5) coll inv_coll;;
- : float list = [0.5; 0.5; 0.5]
