cocktailSort :: Ord a => [a] -> [a]
cocktailSort l
  | not swapped1 = l
  | not swapped2 = reverse $ l1
  | otherwise    = cocktailSort l2
  where (swapped1, l1) = swappingPass (>) (False, []) l
        (swapped2, l2) = swappingPass (<) (False, []) l1

        swappingPass :: Ord a => (a -> a -> Bool) -> (Bool, [a]) -> [a] -> (Bool, [a])
        swappingPass op (swapped, l) (x1 : x2 : xs)
          | op x1 x2  = swappingPass op (True,    x2 : l) (x1 : xs)
          | otherwise = swappingPass op (swapped, x1 : l) (x2 : xs)
        swappingPass _  (swapped, l) [x] = (swapped, x : l)
        swappingPass _  pair         []  = pair
