let items =
  [ "beef",     3.8,  36;
    "pork",     5.4,  43;
    "ham",      3.6,  90;
    "greaves",  2.4,  45;
    "flitch",   4.0,  30;
    "brawn",    2.5,  56;
    "welt",     3.7,  67;
    "salami",   3.0,  95;
    "sausage",  5.9,  98; ]

let () =
  let items = List.map (fun (name, w, p) -> (name, w, p, float p /. w)) items in
  let items = List.sort (fun (_,_,_,v1) (_,_,_,v2) -> compare v2 v1) items in
  let rec loop acc weight = function
  | ((_,w,_,_) as item) :: tl ->
      if w +. weight > 15.0
      then (weight, acc, item)
      else loop (item::acc) (w +. weight) tl
  | [] -> assert false
  in
  let weight, res, (last,w,p,v) = loop [] 0.0 items in
  print_endline "    Items  Weight Price";
  let price =
    List.fold_left (fun price (name,w,p,_) ->
      Printf.printf " %7s: %6.2f %3d\n" name w p;
      (p + price)
    ) 0 res
  in
  let rem_weight = 15.0 -. weight in
  let last_price = v *. rem_weight in
  Printf.printf " %7s: %6.2f %6.2f\n" last rem_weight last_price;
  Printf.printf " Total Price: %.3f\n" (float price +. last_price);
;;
