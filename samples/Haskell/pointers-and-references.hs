import Data.STRef

example :: ST s ()
example = do
  p <- newSTRef 1
  k <- readSTRef p
  writeSTRef p (k+1)
