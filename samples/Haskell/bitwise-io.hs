import Data.List
import Data.Char
import Control.Monad
import Control.Arrow
import System.Environment

int2bin :: Int -> [Int]
int2bin = unfoldr(\x -> if x==0 then Nothing
                          else Just (uncurry(flip(,)) (divMod x 2)))

bin2int :: [Int] -> Int
bin2int = foldr ((.(2 *)).(+)) 0

bitReader = map (chr.bin2int). takeWhile(not.null). unfoldr(Just. splitAt 7)
  . (take =<< (7 *) . (`div` 7) . length)

bitWriter xs =  tt ++ z00 where
  tt = concatMap (take 7.(++repeat 0).int2bin.ord) xs
  z00 = replicate (length xs `mod` 8) 0

main = do
  (xs:_) <- getArgs
  let bits = bitWriter xs

  putStrLn "Text to compress:"
  putStrLn $ '\t' : xs
  putStrLn $ "Uncompressed text length is " ++ show (length xs)
  putStrLn $ "Compressed text has " ++ show (length bits `div` 8) ++ " bytes."
  putStrLn "Read and decompress:"
  putStrLn $ '\t' : bitReader bits
