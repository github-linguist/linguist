type expression =
  | Const of float
  | Sum  of expression * expression   (* e1 + e2 *)
  | Diff of expression * expression   (* e1 - e2 *)
  | Prod of expression * expression   (* e1 * e2 *)
  | Quot of expression * expression   (* e1 / e2 *)

let rec eval = function
  | Const c -> c
  | Sum (f, g) -> eval f +. eval g
  | Diff(f, g) -> eval f -. eval g
  | Prod(f, g) -> eval f *. eval g
  | Quot(f, g) -> eval f /. eval g

let print_expr expr =
  let open_paren prec op_prec =
    if prec > op_prec then print_string "(" in
  let close_paren prec op_prec =
    if prec > op_prec then print_string ")" in
  let rec print prec = function   (* prec is the current precedence *)
    | Const c -> Printf.printf "%g" c
    | Sum(f, g) ->
        open_paren prec 0;
        print 0 f; print_string " + "; print 0 g;
        close_paren prec 0
    | Diff(f, g) ->
        open_paren prec 0;
        print 0 f; print_string " - "; print 1 g;
        close_paren prec 0
    | Prod(f, g) ->
        open_paren prec 2;
        print 2 f; print_string " * "; print 2 g;
        close_paren prec 2
    | Quot(f, g) ->
        open_paren prec 2;
        print 2 f; print_string " / "; print 3 g;
        close_paren prec 2
  in
  print 0 expr

let rec insert v = function
  | [] -> [[v]]
  | x::xs as li -> (v::li) :: (List.map (fun y -> x::y) (insert v xs))

let permutations li =
  List.fold_right (fun x z -> List.concat (List.map (insert x) z)) li [[]]

let rec comp expr = function
  | x::xs ->
      comp (Sum (expr, x)) xs;
      comp (Diff(expr, x)) xs;
      comp (Prod(expr, x)) xs;
      comp (Quot(expr, x)) xs;
  | [] ->
      if (eval expr) = 24.0
      then (print_expr expr; print_newline())
;;

let () =
  Random.self_init();
  let digits = Array.init 4 (fun _ -> 1 + Random.int 9) in
  print_string "Input digits: ";
  Array.iter (Printf.printf " %d") digits; print_newline();
  let digits = Array.to_list(Array.map float_of_int digits) in
  let digits = List.map (fun v -> Const v) digits in
  let all = permutations digits in
  List.iter (function
    | x::xs -> comp x xs
    | [] -> assert false
  ) all
