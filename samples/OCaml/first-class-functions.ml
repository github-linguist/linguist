# let cube x = x ** 3. ;;
val cube : float -> float = <fun>

# let croot x = x ** (1. /. 3.) ;;
val croot : float -> float = <fun>

# let compose f g = fun x -> f (g x) ;;  (* we could have written "let compose f g x = f (g x)" but we show this for clarity *)
val compose : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b = <fun>

# let funclist = [sin; cos; cube] ;;
val funclist : (float -> float) list = [<fun>; <fun>; <fun>]

# let funclisti = [asin; acos; croot] ;;
val funclisti : (float -> float) list = [<fun>; <fun>; <fun>]

# List.map2 (fun f inversef -> (compose inversef f) 0.5) funclist funclisti ;;
- : float list = [0.5; 0.499999999999999889; 0.5]
