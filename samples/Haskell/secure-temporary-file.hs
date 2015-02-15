import System.IO

main = do (pathOfTempFile, h) <- openTempFile "." "prefix.suffix" -- first argument is path to directory where you want to put it
          -- do stuff with it here; "h" is the Handle to the opened file
          return ()
