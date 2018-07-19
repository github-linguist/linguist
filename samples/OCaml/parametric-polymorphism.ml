type 'a tree = Empty | Node of 'a * 'a tree * 'a tree

(** val map_tree : ('a -> 'b) -> 'a tree -> 'b tree *)
let rec map_tree f = function
  | Empty        -> Empty
  | Node (x,l,r) -> Node (f x, map_tree f l, map_tree f r)
