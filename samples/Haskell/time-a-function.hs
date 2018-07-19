import System.CPUTime

-- We assume the function we are timing is an IO monad computation
timeIt :: (Fractional c) => (a -> IO b) -> a -> IO c
timeIt action arg =
  do startTime <- getCPUTime
     action arg
     finishTime <- getCPUTime
     return $ fromIntegral (finishTime - startTime) / 1000000000000

-- Version for use with evaluating regular non-monadic functions
timeIt' :: (Fractional c) => (a -> b) -> a -> IO c
timeIt' f = timeIt (\x -> f x `seq` return ())
