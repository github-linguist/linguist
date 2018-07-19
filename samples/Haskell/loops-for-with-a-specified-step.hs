import Control.Monad (forM_)
main = do forM_ [2,4..8] (\x -> putStr (show x ++ ", "))
          putStrLn "who do we appreciate?"
