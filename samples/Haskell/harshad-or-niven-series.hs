import Data.Char (ord)

main = do print $ take 20 harshads
          print $ head $ filter (> 1000) harshads
    where digsum = sum . map ((48 -) . ord) . show
          harshads = filter (\n -> mod n (digsum n) == 0) [1..]
