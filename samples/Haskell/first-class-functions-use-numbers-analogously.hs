module Main
  where

import Text.Printf

-- Pseudo code happens to be valid Haskell
x  = 2.0
xi = 0.5
y  = 4.0
yi = 0.25
z  = x + y
zi = 1.0 / ( x + y )

-- Multiplier function
multiplier :: Double -> Double -> Double -> Double
multiplier a b = \m -> a * b * m

main :: IO ()
main = do
  let
    numbers = [x, y, z]
    inverses = [xi, yi, zi]
    pairs = zip numbers inverses
    print_pair (number, inverse) =
      let new_function = multiplier number inverse
      in printf "%f * %f * 0.5 = %f\n" number inverse (new_function 0.5)
  mapM_ print_pair pairs
