import Control.Concurrent
import Control.Monad
import Data.Maybe
import System.IO

main = do
    c <- newEmptyMVar
    hSetBuffering stdin NoBuffering
    forkIO $ do
      a <- getChar
      putMVar c a
      putStrLn $ "\nChar '" ++ [a] ++
                 "' read and stored in MVar"
    wait c
  where wait c = do
          a <- tryTakeMVar c
          if isJust a then return ()
          else putStrLn "Awaiting char.." >>
               threadDelay 500000 >> wait c
