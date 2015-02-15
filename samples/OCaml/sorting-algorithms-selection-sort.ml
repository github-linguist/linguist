let rec selection_sort = function
    [] -> []
  | first::lst ->
      let rec select_r small output = function
          [] -> small :: selection_sort output
        | x::xs when x < small -> select_r x (small::output) xs
        | x::xs                -> select_r small (x::output) xs
      in
      select_r first [] lst
