import Control.Monad (forM)
main = forM [1..10] out
    where
      out x | x `mod` 5 == 0 = print x
            | otherwise = (putStr . (++", ") . show) x
