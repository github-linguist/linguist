horner :: (Num a) => a -> [a] -> a
horner x = foldr (\a b -> a + b*x) 0

main = print $ horner 3 [-19, 7, -4, 6]
