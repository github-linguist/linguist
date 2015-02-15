import Numeric

mid3 :: Integral a => a -> Either String String
mid3 n |   m < 100 = Left "is too small"
       |    even l = Left "has an even number of digits"
       | otherwise = Right . take 3 $ drop ((l-3) `div` 2) s
  where m = abs n
        s = showInt m ""
        l = length s

showMid3 :: Integer -> String
showMid3 n = show n ++ ": " ++ either id id (mid3 n)

main :: IO ()
main = mapM_ (putStrLn . showMid3) [
  123, 12345, 1234567, 987654321, 10001, -10001, -123, -100, 100, -12345,
  1, 2, -1, -10, 2002, -2002, 0]
