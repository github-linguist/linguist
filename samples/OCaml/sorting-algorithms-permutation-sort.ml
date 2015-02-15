let rec sorted = function
 | e1 :: e2 :: r -> e1 <= e2 && sorted (e2 :: r)
 | _             -> true

let rec insert e = function
 | []          -> [[e]]
 | h :: t as l -> (e :: l) :: List.map (fun t' -> h :: t') (insert e t)

let permute xs = List.fold_right (fun h z -> List.concat (List.map (insert h) z))
                                 xs [[]]

let permutation_sort l = List.find sorted (permute l)
