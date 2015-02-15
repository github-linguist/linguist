import Data.List
import Data.Ord
import Data.Array
import Text.Printf

hc :: Int -> Array Int Int
hc n = arr
  where arr = listArray (1, n) $ 1 : 1 : map (f (arr!)) [3 .. n]
        f a i = a (a $ i - 1) + a (i - a (i - 1))

printMaxima :: (Int, (Int, Double)) -> IO ()
printMaxima (n, (pos, m)) =
    printf "Max between 2^%-2d and 2^%-2d is %1.5f at n = %6d\n"
                             n    (n + 1)        m        pos

main = do
    mapM_ printMaxima maxima
    printf "Mallows's number is %d\n" mallows
  where
    hca = hc $ 2^20
    hc' n  = fromIntegral (hca!n) / fromIntegral n
    maxima = zip [0..] $ map max powers
    max seq = maximumBy (comparing snd) $ zip seq (map hc' seq)
    powers = map (\n -> [2^n .. 2^(n + 1) - 1]) [0 .. 19]
    mallows = last.takeWhile ((< 0.55) . hc') $ [2^20, 2^20 - 1 .. 1]
