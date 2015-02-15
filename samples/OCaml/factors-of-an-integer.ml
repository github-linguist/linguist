let rec range = function 0 -> [] | n -> range(n-1) @ [n]

let factors n =
  List.filter (fun v -> (n mod v) = 0) (range n)
