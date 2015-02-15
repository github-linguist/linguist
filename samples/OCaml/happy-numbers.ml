open Num

let step =
	let rec aux s n =
	if n =/ Int 0 then s else
		let q = quo_num n (Int 10)
		and r = mod_num n (Int 10)
		in aux (s +/ (r */ r)) q
	in aux (Int 0) ;;

let happy n =
	let rec aux x y =
		if x =/ y then x else aux (step x) (step (step y))
	in (aux n (step n)) =/ Int 1 ;;

let first n =
	let rec aux v x n =
		if n = 0 then v else
			if happy x
			then aux (x::v) (x +/ Int 1) (n - 1)
			else aux v (x +/ Int 1) n
	in aux [ ] (Int 1) n ;;

List.iter print_endline (
	List.rev_map string_of_num (first 8)) ;;
