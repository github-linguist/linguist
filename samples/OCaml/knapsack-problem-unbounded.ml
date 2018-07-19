type bounty = { name:string; value:int; weight:float; volume:float }

let bounty n d w v = { name = n; value = d; weight = w; volume = v }

let items =
  [ bounty "panacea"  3000  0.3  0.025;
    bounty "ichor"    1800  0.2  0.015;
    bounty "gold"     2500  2.0  0.002; ]

let max_wgt = 25.0 and max_vol = 0.25

let itmax =
  let f it =
    let rec aux n =
      if float n *. it.weight >= max_wgt
      || float n *. it.volume >= max_vol
      then (n)
      else aux (succ n)
    in
    aux 0
  in
  List.map f items

let mklist n m =
  let rec aux i acc =
    if i > m then (List.rev acc)
    else aux (succ i) (i::acc)
  in
  aux n []

let comb_items = List.map (mklist 0) itmax

let combs ll =
  let f hd acc =
    List.concat
      (List.map (fun l -> List.map (fun v -> (v::l)) hd) acc)
  in
  List.fold_right f ll [[]]

let possibles = combs comb_items

let packs =
  let f l =
    let g (v, wgt, vol) n it =
      (v + n * it.value,
       wgt +. float n *. it.weight,
       vol +. float n *. it.volume)
    in
    List.fold_left2 g (0, 0.0, 0.0) l items
  in
  List.map f possibles

let packs = List.combine packs possibles

let results =
  let f (_, wgt, vol) = (wgt <= max_wgt && vol <= max_vol) in
  List.filter (fun v -> f(fst v)) packs

let best_results =
  let max_value = List.fold_left (fun v1 ((v2,_,_),_) -> max v1 v2) 0 results in
  List.filter (fun ((v,_,_),_) -> v = max_value) results

let items_name = List.map (fun it -> it.name) items

let print ((v, wgt, vol), ns) =
  Printf.printf "\
    Maximum value: %d \n \
    Total weight:  %g \n \
    Total volume:  %g \n \
    Containing:    " v wgt vol;
  let f n name = string_of_int n ^ " " ^ name in
  let ss = List.map2 f ns items_name in
  print_endline(String.concat ", " ss);
  print_newline()

let () = List.iter print best_results
