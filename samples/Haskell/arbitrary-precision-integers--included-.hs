main = do
    let y = show ( 5^4^3^2 )
    let l = length y
    putStrLn ("5**4**3**2 = " ++ take 20 y ++ "..." ++ drop (l-20) y ++ " and has " ++ show l ++ " digits")
