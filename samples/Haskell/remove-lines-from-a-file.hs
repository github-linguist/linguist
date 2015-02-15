import System.Environment (getArgs)

main = getArgs >>= (\[a, b, c] ->
            do contents <- fmap lines $ readFile a
               let b1 = read b :: Int
                   c1 = read c :: Int
               putStr $ unlines $ concat [take (b1 - 1) contents, drop c1 $ drop b1 contents]
       )
