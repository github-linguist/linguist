-- Return an approximation to the arithmetic-geometric mean of two numbers.
-- The result is considered accurate when two successive approximations are
-- sufficiently close, as determined by "eq".
agm :: (Floating a) => a -> a -> ((a, a) -> Bool) -> a
agm a g eq = snd . head . dropWhile (not . eq) $ iterate step (a, g)
  where step (a, g) = ((a + g) / 2, sqrt (a * g))

-- Return the relative difference of the pair.  We assume that at least one of
-- the values is far enough from 0 to not cause problems.
relDiff :: (Fractional a) => (a, a) -> a
relDiff (x, y) = let n = abs (x - y)
                     d = ((abs x) + (abs y)) / 2
                 in n / d

main = do
  let equal = (< 0.000000001) . relDiff
  print $ agm 1 (1 / sqrt 2) equal
