type btdigit = Pos | Zero | Neg
type btern = btdigit list

let to_string n =
   String.concat ""
      (List.rev_map (function Pos -> "+" | Zero -> "0" | Neg -> "-") n)

let from_string s =
   let sl = ref [] in
   let digit = function '+' -> Pos | '-' -> Neg | '0' -> Zero
     | _ -> failwith "invalid digit" in
    String.iter (fun c -> sl := (digit c) :: !sl) s; !sl

let rec to_int = function
   | [Zero] | [] -> 0
   | Pos :: t -> 1 + 3 * to_int t
   | Neg :: t -> -1 + 3 * to_int t
   | Zero :: t -> 3 * to_int t

let rec from_int n =
   if n = 0 then [] else
   match n mod 3 with
      | 0 -> Zero :: from_int (n/3)
      | 1 | -2 -> Pos :: from_int ((n-1)/3)
      | 2 | -1 -> Neg :: from_int ((n+1)/3)

let rec (+~) n1 n2 = match (n1,n2) with
   | ([], a) | (a,[]) -> a
   | (Pos::t1, Neg::t2) | (Neg::t1, Pos::t2) | (Zero::t1, Zero::t2) ->
      let sum = t1 +~ t2 in if sum = [] then [] else Zero :: sum
   | (Pos::t1, Pos::t2) -> Neg :: t1 +~ t2 +~ [Pos]
   | (Neg::t1, Neg::t2) -> Pos :: t1 +~ t2 +~ [Neg]
   | (Zero::t1, h::t2) | (h::t1, Zero::t2) -> h :: t1 +~ t2

let neg = List.map (function Pos -> Neg | Neg -> Pos | Zero -> Zero)
let (-~) a b = a +~ (neg b)

let rec ( *~) n1 = function
   | [] -> []
   | [Pos] -> n1
   | [Neg] -> neg n1
   | Pos::t -> (Zero :: t *~ n1) +~ n1
   | Neg::t -> (Zero :: t *~ n1) -~ n1
   | Zero::t -> Zero :: t *~ n1

let a = from_string "+-0++0+"
let b = from_int (-436)
let c = from_string "+-++-"
let d = a *~ (b -~ c)
let _ =
  Printf.printf "a = %d\nb = %d\nc = %d\na * (b - c) = %s = %d\n"
   (to_int a) (to_int b) (to_int c) (to_string d) (to_int d);
