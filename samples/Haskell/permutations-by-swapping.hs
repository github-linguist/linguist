insertEverywhere :: a -> [a] -> [[a]]
insertEverywhere x [] = [[x]]
insertEverywhere x l@(y:ys) = (x:l) : map (y:) (insertEverywhere x ys)

s_perm :: [a] -> [[a]]
s_perm = foldl aux [[]]
  where aux items x = do (f, item) <- zip (cycle [reverse, id]) items
                         f (insertEverywhere x item)

s_permutations :: [a] -> [([a], Int)]
s_permutations = flip zip (cycle [1, -1]) . s_perm

main :: IO ()
main = do
  putStrLn "3 items:"
  mapM_ print $ s_permutations [0..2]
  putStrLn "4 items:"
  mapM_ print $ s_permutations [0..3]
