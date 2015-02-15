let pf = Printf.printf ;;

let looses a b = match a, b with
     `R, `P -> true
   | `P, `S -> true
   | `S, `R -> true
   |  _,  _ -> false ;;

let rec get_move () =
   pf "[R]ock, [P]aper, [S]cisors [Q]uit? " ;
   match String.uppercase (read_line ()) with
        "P" -> `P
      | "S" -> `S
      | "R" -> `R
      | "Q" -> exit 0
      | _   -> get_move () ;;

let str_of_move = function
        `P -> "paper"
      | `R -> "rock"
      | `S -> "scisors" ;;

let comp_move r p s =
   let tot = r +. p +. s in
   let n = Random.float 1.0 in
   if n < r /. tot then
      `R
   else
      if n < (r +. p) /. tot then
         `P
      else
         `S ;;

let rec make_moves r p s =
   let cm = comp_move r p s in   (* Computer move is based on game history. *)
   let hm = get_move () in       (* Human move is requested. *)
   pf "Me: %s. You: %s. " (str_of_move cm) (str_of_move hm);
   let outcome =
      if looses hm cm then
         "I win. You loose.\n"
      else if cm = hm then
         "We draw.\n"
      else
         "You win. I loose.\n"
   in pf "%s" outcome;
   match hm with (* Play on with adapted strategy. *)
        `S -> make_moves (r +. 1.) p s
      | `R -> make_moves r (p +. 1.) s
      | `P -> make_moves r p (s +. 1.) ;;

(* Main loop. *)
make_moves 1. 1. 1. ;;
