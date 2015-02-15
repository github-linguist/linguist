dotp a b | length a == length b = sum (zipWith (*) a b)
         | otherwise = error "Vector sizes must match"

main = print $ dotp [1, 3, -5] [4, -2, -1] -- prints 3
