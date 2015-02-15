# let horner coeffs x =
    List.fold_left (fun acc coef -> acc * x + coef) 0 (List.rev coeffs) ;;
val horner : int list -> int -> int = <fun>

# let coeffs = [-19; 7; -4; 6] in
  horner coeffs 3 ;;
- : int = 128
