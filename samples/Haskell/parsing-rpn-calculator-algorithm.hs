import Data.List (elemIndex)

-- Show results
main = mapM_ (\(x, y) -> putStrLn $ x ++ " ==> " ++ show y) $ reverse $ zip b (a:c)
        where (a, b, c) = solve "3 4 2 * 1 5 - 2 3 ^ ^ / +"

-- Solve and report RPN
solve = foldl reduce ([], [], []) . words
reduce (xs, ps, st) w =
    if i == Nothing
        then (read w:xs, ("Pushing " ++ w):ps, xs:st)
        else (([(*),(+),(-),(/),(**)]!!o) a b:zs, ("Performing " ++ w):ps, xs:st)
    where   i = elemIndex (head w) "*+-/^"
            Just o = i
            (b:a:zs) = xs
