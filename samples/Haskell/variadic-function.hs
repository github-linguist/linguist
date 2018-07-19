class PrintAllType t where
    process :: [String] -> t

instance PrintAllType (IO a) where
    process args = do mapM_ putStrLn args
                      return undefined

instance (Show a, PrintAllType r) => PrintAllType (a -> r) where
    process args = \a -> process (args ++ [show a])

printAll :: (PrintAllType t) => t
printAll = process []

main :: IO ()
main = do printAll 5 "Mary" "had" "a" "little" "lamb"
          printAll 4 3 5
          printAll "Rosetta" "Code" "Is" "Awesome!"
