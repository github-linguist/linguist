import Prelude hiding (odd)

halve, double :: Integral a => a -> a
halve = (`div` 2)
double = (2 *)

odd :: Integral a => a -> Bool
odd = (== 1) . (`mod` 2)

ethiopicmult :: Integral a => a -> a -> a
ethiopicmult a b = sum $ map snd $ filter (odd . fst) $ zip
    (takeWhile (>= 1) $ iterate halve a)
    (iterate double b)

main = print $ ethiopicmult 17 34 == 17 * 34
