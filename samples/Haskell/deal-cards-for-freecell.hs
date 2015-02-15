import Data.Int
import Data.Bits
import Data.List
import Data.Array.ST
import Control.Monad
import Control.Monad.ST
import System.Environment

srnd :: Int32 -> [Int]
srnd = map (fromIntegral . flip shiftR 16) .
       tail . iterate (\x -> (x * 214013 + 2531011) .&. maxBound)

deal :: Int32 -> [String]
deal s = runST (do
    ar <- newListArray (0,51) $ sequence ["A23456789TJQK", "CDHS"]
          :: ST s (STArray s Int String)
    forM (zip [52,51..1] rnd) $ \(n, r) -> do
      let j = r `mod` n
      vj <- readArray ar j
      vn <- readArray ar (n - 1)
      writeArray ar j vn
      return vj)
  where rnd = srnd s

showCards :: [String] -> IO ()
showCards = mapM_ (putStrLn . unwords) .
            takeWhile (not . null) .
            unfoldr (Just . splitAt 8)

main :: IO ()
main = do
  args <- getArgs
  let s = read (head args) :: Int32
  putStrLn $ "Deal " ++ show s ++ ":"
  let cards = deal s
  showCards cards
