import Data.Ratio ((%))

real2cf :: (RealFrac a, Integral b) => a -> [b]
real2cf x =
  i : if f == 0 then [] else real2cf (1/f)
  where (i, f) = properFraction x

main :: IO ()
main = do
  print $ real2cf (13 % 11) -- => [1,5,2]
  print $ take 20 $ real2cf (sqrt 2) -- => [1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2]
