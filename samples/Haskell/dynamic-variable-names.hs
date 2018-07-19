data Var a = Var String a deriving Show
main = do
    putStrLn "please enter you variable name"
    vName <- getLine
    let var = Var vName 42
    putStrLn $ "this is your variable: " ++ show var
