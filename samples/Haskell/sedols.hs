import Data.Char

char2value c | c `elem` "AEIOU"     = error "No vowels."
             | c >= '0' && c <= '9' = ord c - ord '0'
             | c >= 'A' && c <= 'Z' = ord c - ord 'A' + 10

sedolweight = [1,3,1,7,3,9]

checksum sedol = show ((10 - (tmp `mod` 10)) `mod` 10)
  where tmp = sum $ zipWith (*) sedolweight $ map char2value sedol

main = mapM_ (\sedol -> putStrLn $ sedol ++ checksum sedol)
        [ "710889",
          "B0YBKJ",
          "406566",
          "B0YBLH",
          "228276",
          "B0YBKL",
          "557910",
          "B0YBKR",
          "585284",
          "B0YBKT" ]
