let div_mod n d = (n / d, n mod d)
let join = String.concat ", " ;;

let rec nonzero = function
  | _, 0 -> ""
  | c, n -> c ^ (spell_integer n)

and tens n =
  [| ""; ""; "twenty"; "thirty"; "forty"; "fifty";
             "sixty"; "seventy"; "eighty"; "ninety" |].(n)

and small n =
  [| "zero"; "one"; "two"; "three"; "four"; "five";
     "six"; "seven"; "eight"; "nine"; "ten"; "eleven";
     "twelve"; "thirteen"; "fourteen"; "fifteen";
     "sixteen";"seventeen"; "eighteen"; "nineteen" |].(n)

and bl = [| ""; ""; "m"; "b"; "tr"; "quadr"; "quint";
                    "sext"; "sept"; "oct"; "non"; "dec" |]

and big = function
  | 0, n -> (spell_integer n)
  | 1, n -> (spell_integer n) ^ " thousand"
  | e, n -> (spell_integer n) ^ " " ^ bl.(e) ^ "illion"

and uff acc = function
  | 0 -> List.rev acc
  | n ->
      let a, b = div_mod n 1000 in
      uff (b::acc) a

and spell_integer = function
  | n when n < 0 -> invalid_arg "spell_integer: negative input"
  | n when n < 20 -> small n
  | n when n < 100 ->
      let a, b = div_mod n 10 in
      (tens a) ^ nonzero("-", b)
  | n when n < 1000 ->
      let a, b = div_mod n 100 in
      (small a) ^ " hundred" ^ nonzero(" ", b)
  | n ->
      let seg = (uff [] n) in
      let _, segn =
        (* just add the index of the item in the list *)
        List.fold_left
          (fun (i,acc) v -> (succ i, (i,v)::acc))
          (0,[])
          seg
      in
      let fsegn =
        (* remove right part "zero" *)
        List.filter
          (function (_,0) -> false | _ -> true)
          segn
      in
      join(List.map big fsegn)
;;
