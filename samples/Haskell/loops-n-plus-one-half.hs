loop :: IO ()
loop = mapM_ action [1 .. 10]
  where action n = do
            putStr $ show n
            putStr $ if n == 10 then "\n" else ", "
