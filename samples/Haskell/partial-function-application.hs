fs = map
f1 = (* 2)
f2 = (^ 2)

fsf1 = fs f1
fsf2 = fs f2

main :: IO ()
main = do
  print $ fsf1 [0, 1, 2, 3] -- prints [0, 2, 4, 6]
  print $ fsf2 [0, 1, 2, 3] -- prints [0, 1, 4, 9]
  print $ fsf1 [2, 4, 6, 8] -- prints [4, 8, 12, 16]
  print $ fsf2 [2, 4, 6, 8] -- prints [4, 16, 36, 64]
