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

let rec extract acc = function
  | Const c -> (c::acc)
  | Sum (f, g) -> (extract acc f) @ (extract [] g)
  | Diff(f, g) -> (extract acc f) @ (extract [] g)
  | Prod(f, g) -> (extract acc f) @ (extract [] g)
  | Quot(f, g) -> (extract acc f) @ (extract [] g)

open Genlex

let lexer = make_lexer ["("; ")"; "+"; "-"; "*"; "/"]

let rec parse_expr = parser
     [< e1 = parse_mult; e = parse_more_adds e1 >] -> e
 and parse_more_adds e1 = parser
     [< 'Kwd "+"; e2 = parse_mult; e = parse_more_adds (Sum(e1, e2)) >] -> e
   | [< 'Kwd "-"; e2 = parse_mult; e = parse_more_adds (Diff(e1, e2)) >] -> e
   | [< >] -> e1
 and parse_mult = parser
     [< e1 = parse_simple; e = parse_more_mults e1 >] -> e
 and parse_more_mults e1 = parser
     [< 'Kwd "*"; e2 = parse_simple; e = parse_more_mults (Prod(e1, e2)) >] -> e
   | [< 'Kwd "/"; e2 = parse_simple; e = parse_more_mults (Quot(e1, e2)) >] -> e
   | [< >] -> e1
 and parse_simple = parser
   | [< 'Int i >] -> Const(float i)
   | [< 'Float f >] -> Const f
   | [< 'Kwd "("; e = parse_expr; 'Kwd ")" >] -> e


let parse_expression = parser [< e = parse_expr; _ = Stream.empty >] -> e

let read_expression s = parse_expression(lexer(Stream.of_string s))


let () =
  Random.self_init();
  print_endline "
  The 24 Game

  Given any four digits in the range 1 to 9, which may have repetitions,
  Using just the +, -, *, and / operators; and the possible use of
  brackets, (), show how to make an answer of 24.

  An answer of 'q' will quit the game.
  An answer of '!' will generate a new set of four digits.
  Otherwise you are repeatedly asked for an expression until it evaluates to 24

  Note: you cannot form multiple digit numbers from the supplied digits,
  so an answer of 12+12 when given 1, 2, 2, and 1 would not be allowed.\n";

  let sort = List.sort compare in
  let digits = ref [] in
  let digit_set () =
    let ar = Array.init 4 (fun _ -> 1 + Random.int 9) in
    digits := Array.to_list(Array.map float_of_int ar);
    print_string "The four digits: ";
    List.iter (Printf.printf " %g") !digits;
    print_newline();
  in

  digit_set();
  while true do
    print_string "Expression: ";
    let str = read_line() in
    if str = "q" then exit 0;
    if str = "!" then digit_set()
    else begin
      let expr = read_expression str in
      let res = eval expr in
      Printf.printf " = %g\n%!" res;
      if res = 24.
      && (sort !digits) = (sort (extract [] expr))
      then (print_endline "Congratulations!"; digit_set())
      else print_endline "Try again"
    end
  done
