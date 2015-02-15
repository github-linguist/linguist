type color = R | B
type 'a tree = E | T of color * 'a tree * 'a * 'a tree

(** val balance : color * 'a tree * 'a * 'a tree -> 'a tree *)
let balance = function
  | B, T (R, T (R,a,x,b), y, c), z, d
  | B, T (R, a, x, T (R,b,y,c)), z, d
  | B, a, x, T (R, T (R,b,y,c), z, d)
  | B, a, x, T (R, b, y, T (R,c,z,d)) -> T (R, T (B,a,x,b), y, T (B,c,z,d))
  | col, a, x, b                      -> T (col, a, x, b)

(** val insert : 'a -> 'a tree -> 'a tree *)
let insert x s =
  let rec ins = function
    | E                  -> T (R,E,x,E)
    | T (col,a,y,b) as s ->
	if x < y then
	  balance (col, ins a, y, b)
	else if x > y then
	  balance (col, a, y, ins b)
	else
	  s
  in let T (_,a,y,b) = ins s
  in T (B,a,y,b)
