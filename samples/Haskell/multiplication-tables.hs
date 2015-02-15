import Control.Monad
import Text.Printf

main = do
    putStrLn $ "   x" ++ concatMap fmt [1..12]
    zipWithM_ f [1..12] $ iterate ("    " ++) ""
  where f n s = putStrLn $ fmt n ++ s ++ concatMap (fmt . (*n)) [n..12]
        fmt n = printf "%4d" (n :: Int)
