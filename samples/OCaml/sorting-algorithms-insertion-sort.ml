let rec insert x = function
  [] -> [x]
| y :: ys ->
   if x <= y then x :: y :: ys
   else y :: insert x ys
;;
let insertion_sort lst = List.fold_right insert lst [];;

insertion_sort [6;8;5;9;3;2;1;4;7];;
