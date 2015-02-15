#load "nums.cma";;
open Num;;

(* generate Hailstone sequence *)
let hailstone n =
  let one = Int 1
  and two = Int 2
  and three = Int 3 in
  let rec g s x =
    if x =/ one
    then x::s
    else g (x::s) (if mod_num x two =/ one
                   then three */ x +/ one
                   else x // two)
  in
  g [] (Int n)
;;

(* compute only sequence length *)
let haillen n =
  let one = Int 1
  and two = Int 2
  and three = Int 3 in
  let rec g s x =
    if x =/ one
    then s+1
    else g (s+1) (if mod_num x two =/ one
                  then three */ x +/ one
                  else x // two)
  in
  g 0 (Int n)
;;

(* max length for starting values in 1..n *)
let hailmax =
  let rec g idx len = function
  | 0 -> (idx, len)
  | i ->
      let a = haillen i in
      if a > len
      then g i a (i-1)
      else g idx len (i-1)
  in
  g 0 0
;;

hailmax 100000 ;;
(* - : int * int = (77031, 351) *)

List.rev_map string_of_num (hailstone 27) ;;

(* - : string list =
["27"; "82"; "41"; "124"; "62"; "31"; "94"; "47"; "142"; "71"; "214"; "107";
 "322"; "161"; "484"; "242"; "121"; "364"; "182"; "91"; "274"; "137"; "412";
 "206"; "103"; "310"; "155"; "466"; "233"; "700"; "350"; "175"; "526"; "263";
 "790"; "395"; "1186"; "593"; "1780"; "890"; "445"; "1336"; "668"; "334";
 "167"; "502"; "251"; "754"; "377"; "1132"; "566"; "283"; "850"; "425";
 "1276"; "638"; "319"; "958"; "479"; "1438"; "719"; "2158"; "1079"; "3238";
 "1619"; "4858"; "2429"; "7288"; "3644"; "1822"; "911"; "2734"; "1367";
 "4102"; "2051"; "6154"; "3077"; "9232"; "4616"; "2308"; "1154"; "577";
 "1732"; "866"; "433"; "1300"; "650"; "325"; "976"; "488"; "244"; "122";
 "61"; "184"; "92"; "46"; "23"; "70"; "35"; "106"; "53"; "160"; "80"; "40";
 "20"; "10"; "5"; "16"; "8"; "4"; "2"; "1"] *)
