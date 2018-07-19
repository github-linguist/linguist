import Data.List (insert)

insertionSort :: Ord a => [a] -> [a]
insertionSort = foldr insert []

-- Example use:
-- *Main> insertionSort [6,8,5,9,3,2,1,4,7]
-- [1,2,3,4,5,6,7,8,9]
