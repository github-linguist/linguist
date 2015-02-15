import Data.List

shellSort xs = foldr (invColumnize (map (foldr insert []))) xs gaps
  where gaps = takeWhile (< length xs) $ iterate (succ.(3*)) 1
        invColumnize f k = concat. transpose. f. transpose
                           . takeWhile (not.null). unfoldr (Just. splitAt k)
