import System.Random
import Data.Array.IO
import Control.Monad

isSorted :: (Ord a) => [a] -> Bool
isSorted (e1:e2:r) = e1 <= e2 && isSorted (e2:r)
isSorted _         = True

-- from http://www.haskell.org/haskellwiki/Random_shuffle
shuffle :: [a] -> IO [a]
shuffle xs = do
        ar <- newArray n xs
        forM [1..n] $ \i -> do
            j <- randomRIO (i,n)
            vi <- readArray ar i
            vj <- readArray ar j
            writeArray ar j vi
            return vj
  where
    n = length xs
    newArray :: Int -> [a] -> IO (IOArray Int a)
    newArray n xs =  newListArray (1,n) xs

bogosort :: (Ord a) => [a] -> IO [a]
bogosort li | isSorted li = return li
            | otherwise   = shuffle li >>= bogosort
