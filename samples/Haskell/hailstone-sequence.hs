import Data.List (maximumBy)
import Data.Ord (comparing)

hailstone :: Int -> [Int]
hailstone 1             = [1]
hailstone n | even n    = n : hailstone (n `div` 2)
            | otherwise = n : hailstone (n * 3 + 1)

withResult :: (t -> t1) -> t -> (t1, t)
withResult f x = (f x, x)

main :: IO ()
main = do
 let h27 = hailstone 27
 print $ length h27
 let h4 = show $ take 4 h27
 let t4 = show $ drop (length h27 - 4) h27
 putStrLn ("hailstone 27: " ++ h4 ++ " ... " ++ t4)
 print $ maximumBy (comparing fst) $ map (withResult (length . hailstone)) [1..100000]
