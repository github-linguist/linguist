import Data.Char

trimLeft :: String -> String
trimLeft = dropWhile isSpace

trimRight :: String -> String
trimRight str | all isSpace str = ""
trimRight (c : cs) = c : trimRight cs

trim :: String -> String
trim = trimLeft . trimRight
