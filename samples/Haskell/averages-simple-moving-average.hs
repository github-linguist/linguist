import Data.List
import Control.Arrow
import Control.Monad

sMA p = map (head *** head ).tail.
      scanl (\(y,_) -> (id &&& return. av) . (: if length y == p then init y else y)) ([],[])
    where av = liftM2 (/) sum (fromIntegral.length)

printSMA n p = mapM_ (\(n,a) -> putStrLn $ "Next number: " ++ show n ++ "  Average: " ++ show a)
  . take n . sMA p $ [1..5]++[5,4..1]++[3..]
