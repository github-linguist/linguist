import Data.List
import Numeric
import Text.Printf

-- Use the built-in function showIntAtBase.
toBin n = showIntAtBase 2 ("01" !!) n ""

-- Implement our own version.
toBin' 0 = []
toBin' x =  (toBin' $ x `div` 2) ++ (show $ x `mod` 2)

printToBin n = putStrLn $ printf "%4d  %14s  %14s" n (toBin n) (toBin' n)

main = do
  putStrLn $ printf "%4s  %14s  %14s" "N" "toBin" "toBin'"
  mapM_ printToBin [5, 50, 9000]
