-- Three infinite lists, corresponding to the three definitions in the problem
-- statement.

cats1 = map (\n -> product [n+2..2*n] `div` product [1..n]) [0..]

cats2 = 1 : map (\n -> sum $ zipWith (*) (reverse (take n cats2)) cats2) [1..]

cats3 = scanl (\c n -> c*2*(2*n-1) `div` (n+1)) 1 [1..]

main = mapM_ (print . take 15) [cats1, cats2, cats3]
