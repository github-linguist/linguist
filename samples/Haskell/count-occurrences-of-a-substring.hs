import Data.Text hiding (length)

-- Return the number of non-overlapping occurrences of sub in str.
countSubStrs str sub = length $ breakOnAll (pack sub) (pack str)

main = do
  print $ countSubStrs "the three truths" "th"
  print $ countSubStrs "ababababab" "abab"
