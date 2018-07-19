import Data.Ratio

-- simply prints all the perfect numbers
main = mapM_ print [candidate
                   | candidate <- [2 .. 2^19],
                     getSum candidate == 1]
  where getSum candidate = 1 % candidate +
                           sum [1 % factor + 1 % (candidate `div` factor)
                               | factor <- [2 .. floor(sqrt(fromIntegral(candidate)))],
                                 candidate `mod` factor == 0]
