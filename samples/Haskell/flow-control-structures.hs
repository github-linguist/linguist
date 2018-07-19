import Control.Monad
import Control.Monad.Trans
import Control.Monad.Exit

main = do
    runExitTMaybe $ do
        forM_ [1..5] $ \x -> do
            forM_ [1..5] $ \y -> do
                lift $ print (x, y)
                when (x == 3 && y == 2) $
                    exitWith ()
    putStrLn "Done."
