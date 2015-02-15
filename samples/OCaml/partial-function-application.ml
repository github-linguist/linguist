#
let fs f s = List.map f s
let f1 value = value * 2
let f2 value = value * value

let fsf1 = fs f1
let fsf2 = fs f2
;;
val fs : ('a -> 'b) -> 'a list -> 'b list = <fun>
val f1 : int -> int = <fun>
val f2 : int -> int = <fun>
val fsf1 : int list -> int list = <fun>
val fsf2 : int list -> int list = <fun>

# fsf1 [0; 1; 2; 3];;
- : int list = [0; 2; 4; 6]
# fsf2 [0; 1; 2; 3];;
- : int list = [0; 1; 4; 9]
# fsf1 [2; 4; 6; 8];;
- : int list = [4; 8; 12; 16]
# fsf2 [2; 4; 6; 8];;
- : int list = [4; 16; 36; 64]
