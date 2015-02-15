import Data.Array.IO

main = do arr <- newArray (1,10) 37 :: IO (IOArray Int Int)
          a <- readArray arr 1
          writeArray arr 1 64
          b <- readArray arr 1
          print (a,b)
