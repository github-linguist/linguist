(* binop : ('a -> 'a -> 'a) -> 'a list -> 'a list *)
let binop op = function
  | b::a::r -> (op a b)::r
  | _ -> failwith "invalid expression"

(* interp : float list -> string -> string * float list *)
let interp s = function
  | "+" -> "add",    binop ( +. ) s
  | "-" -> "subtr",  binop ( -. ) s
  | "*" -> "mult",   binop ( *. ) s
  | "/" -> "divide", binop ( /. ) s
  | "^" -> "exp",    binop ( ** ) s
  | str -> "push", (float_of_string str) :: s

(* interp_and_show : float list -> string -> float list *)
let interp_and_show s inp =
  let op,s' = interp s inp in
  Printf.printf "%s\t%s\t" inp op;
  List.(iter (Printf.printf "%F ") (rev s'));
  print_newline ();
  s'

(* rpn_eval : string -> float list *)
let rpn_eval str =
  Printf.printf "Token\tAction\tStack\n";
  let ss = Str.(split (regexp_string " ") str) in
  List.fold_left interp_and_show [] ss
