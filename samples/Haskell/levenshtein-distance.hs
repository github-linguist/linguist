levenshtein s1 s2 = last $ foldl transform [0 .. length s1] s2
  where transform ns@(n:ns') c = scanl calc (n+1) $ zip3 s1 ns ns'
          where calc z (c', x, y) = minimum [y+1, z+1, x + fromEnum (c' /= c)]

main = print (levenshtein "kitten" "sitting")
