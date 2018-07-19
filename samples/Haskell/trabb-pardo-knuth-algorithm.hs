import Control.Monad (replicateM, mapM_)

f x = (abs x) ** 0.5 + 5 * x ** 3

main = do
        putStrLn "Enter 11 numbers for evaluation"
        x <- replicateM 11 $ readLn
        mapM_ ((\x -> if x > 400
                      then putStrLn "OVERFLOW"
                      else print x). f) $ reverse x
