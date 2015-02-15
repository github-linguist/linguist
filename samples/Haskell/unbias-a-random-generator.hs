import Control.Monad
import Random
import Data.IORef
import Text.Printf

randN :: Integer -> IO Bool
randN n = randomRIO (1,n) >>= return . (== 1)

unbiased :: Integer -> IO Bool
unbiased n = do
  a <- randN n
  b <- randN n
  if a /= b then return a else unbiased n

main :: IO ()
main = forM_ [3..6] $ \n -> do
  cb <- newIORef 0
  cu <- newIORef 0
  replicateM_ trials $ do
    b <- randN n
    u <- unbiased n
    when b $ modifyIORef cb (+ 1)
    when u $ modifyIORef cu (+ 1)
  tb <- readIORef cb
  tu <- readIORef cu
  printf "%d: %5.2f%%  %5.2f%%\n" n
    (100 * fromIntegral tb / fromIntegral trials :: Double)
    (100 * fromIntegral tu / fromIntegral trials :: Double)
  where trials = 50000
