import Data.Bits
import ADNS.Endian -- http://hackage.haskell.org/package/hsdns

main = do
  putStrLn $ "Word size: " ++ bitsize
  putStrLn $ "Endianness: " ++ show endian
      where
        bitsize = show $ bitSize (undefined :: Int)
