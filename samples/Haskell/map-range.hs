import Data.Ratio
import Text.Printf

-- Map a value from the range [a1,a2] to the range [b1,b2].  We don't check
-- for empty ranges.
mapRange :: (Fractional a) => (a, a) -> (a, a) -> a -> a
mapRange (a1,a2) (b1,b2) s = b1+(s-a1)*(b2-b1)/(a2-a1)

main = do
  -- Perform the mapping over floating point numbers.
  putStrLn "---------- Floating point ----------"
  mapM_ (\n -> prtD n . mapRange (0,10) (-1,0) $ fromIntegral n) [0..10]

  -- Perform the same mapping over exact rationals.
  putStrLn "---------- Rationals ----------"
  mapM_ (\n -> prtR n . mapRange (0,10) (-1,0) $ n%1) [0..10]

    where prtD :: PrintfType r => Integer -> Double -> r
          prtD n x = printf "%2d -> %6.3f\n" n x
          prtR :: PrintfType r => Integer -> Rational -> r
          prtR n x = printf "%2d -> %s\n" n (show x)
