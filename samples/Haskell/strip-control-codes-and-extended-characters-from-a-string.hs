import Data.Char

strip = filter (\x -> ord x > 32 && ord x < 126)
