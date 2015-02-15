main = mapM_ putStrLn $ zipWith3 (\a b c -> [a,b,c]) "abc" "ABC" "123"
