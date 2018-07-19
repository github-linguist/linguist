import Data.List (unfoldr)

selectionSort = unfoldr selectionSort' where
   selectionSort' [] = Nothing
   selectionSort' (first:lst) = Just $ foldl f (first, []) lst
   f (small, output) x | x < small = (x, small:output)
                       | otherwise = (small, x:output)
