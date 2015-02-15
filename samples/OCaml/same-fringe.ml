type 'a btree = Leaf of 'a | BTree of ('a btree * 'a btree)

let rec next = function
  | [] -> None
  | h :: t -> match h with
    | Leaf x -> Some (x,t)
    | BTree(a,b) -> next (a::b::t)

let samefringe t1 t2 =
  let rec aux s1 s2 = match (next s1, next s2) with
    | None, None -> true
    | None, _ | _, None -> false
    | Some(a,b), Some(c,d) -> (a=c) && aux b d in
  aux [t1] [t2]

(* Test: *)
let () =
  let u = BTree(Leaf 1, BTree(Leaf 2, Leaf 3)) in
  let v = BTree(BTree(Leaf 1, Leaf 2), Leaf 3) in
  let w = BTree(BTree(Leaf 3, Leaf 2), Leaf 1) in
  let check a b =
    print_endline (if samefringe a b then "same" else "different") in
  check u v; check v u; check v w;
