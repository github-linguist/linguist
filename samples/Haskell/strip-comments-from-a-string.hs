ms = ";#"

main = getContents >>=
    mapM_ (putStrLn . takeWhile (`notElem` ms)) . lines
