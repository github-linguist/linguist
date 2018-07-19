let my_max = function
    [] -> invalid_arg "empty list"
  | x::xs -> List.fold_left max x xs
