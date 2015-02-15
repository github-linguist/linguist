array = {1, 2, 3, 4, 5}
Sum = sum [x \\ x <-: array]
Prod = foldl (*) 1 [x \\ x <-: array]
