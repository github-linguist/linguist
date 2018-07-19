import Control.Concurrent

main = do seconds <- readLn
          putStrLn "Sleeping..."
          threadDelay $ round $ seconds * 1000000
          putStrLn "Awake!"
