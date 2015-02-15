import System.IO (hFlush, stdout)
main = do
    putStr "Enter a string: "
    hFlush stdout
    str <- getLine
    putStr "Enter an integer: "
    hFlush stdout
    num <- readLn :: IO Int
    putStrLn $ str ++ (show num)
