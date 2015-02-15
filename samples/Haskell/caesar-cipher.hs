import Data.Char (ord, chr)
import Data.Ix (inRange)

caesar :: Int -> String -> String
caesar k = map f
  where
    f c
      | inRange ('a','z') c = tr 'a' k c
      | inRange ('A','Z') c = tr 'A' k c
      | otherwise = c

unCaesar :: Int -> String -> String
unCaesar k = caesar (-k)

-- char addition
tr :: Char -> Int -> Char -> Char
tr base offset char = chr $ ord base + (ord char - ord base + offset) `mod` 26
