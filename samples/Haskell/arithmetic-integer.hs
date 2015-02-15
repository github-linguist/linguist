main = do
  a <- readLn :: IO Integer
  b <- readLn :: IO Integer
  putStrLn $ "a + b = " ++ show (a + b)
  putStrLn $ "a - b = " ++ show (a - b)
  putStrLn $ "a * b = " ++ show (a * b)
  putStrLn $ "a to the power of b = " ++ show (a ** b)
  putStrLn $ "a to the power of b = " ++ show (a ^ b)
  putStrLn $ "a to the power of b = " ++ show (a ^^ b)
  putStrLn $ "a `div` b = "  ++ show (a `div` b)  -- truncates towards negative infinity
  putStrLn $ "a `mod` b = "  ++ show (a `mod` b)  -- same sign as second operand
  putStrLn $ "a `divMod` b = "  ++ show (a `divMod` b)
  putStrLn $ "a `quot` b = " ++ show (a `quot` b) -- truncates towards 0
  putStrLn $ "a `rem` b = "  ++ show (a `rem` b)  -- same sign as first operand
  putStrLn $ "a `quotRem` b = "  ++ show (a `quotRem` b)
