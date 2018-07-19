main :: IO ()
main = do contents <- readFile filename
          case drop 6 $ lines contents of
            []  -> error "File has less than seven lines"
            l:_ -> putStrLn l
  where filename = "testfile"
