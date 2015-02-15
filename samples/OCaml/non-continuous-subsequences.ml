let rec fence s = function
    [] ->
      if s >= 3 then
        [[]]
      else
        []

  | x :: xs ->
      if s mod 2 = 0 then
        List.map
          (fun ys -> x :: ys)
          (fence (s + 1) xs)
        @
          fence s xs
      else
        List.map
          (fun ys -> x :: ys)
          (fence s xs)
        @
          fence (s + 1) xs

let ncsubseq = fence 0
