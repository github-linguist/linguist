import Data.List
import Data.Ratio
import System.Environment
import Text.Printf

-- A wrapper type for Rationals to make them look nicer when we print them.
newtype Rat = Rat Rational
instance Show Rat where
  show (Rat n) = show (numerator n) ++ "/" ++ show (denominator n)

-- Convert a list of base b digits to its corresponding number.  We assume the
-- digits are valid base b numbers and that their order is from least to most
-- significant.
digitsToNum :: Integer -> [Integer] -> Integer
digitsToNum b = foldr1 (\d acc -> b * acc + d)

-- Convert a number to the list of its base b digits.  The order will be from
-- least to most significant.
numToDigits :: Integer -> Integer -> [Integer]
numToDigits _ 0 = [0]
numToDigits b n = unfoldr step n
  where step 0 = Nothing
        step m = let (q,r) = m `quotRem` b in Just (r,q)

-- Return the n'th element in the base b van der Corput sequence.  The base
-- must be ≥ 2.
vdc :: Integer -> Integer -> Rat
vdc b n | b < 2 = error "vdc: base must be ≥ 2"
        | otherwise = let ds = reverse $ numToDigits b n
                      in Rat (digitsToNum b ds % b ^ length ds)

-- Print the base followed by a sequence of van der Corput numbers.
printVdc :: (Integer,[Rat]) -> IO ()
printVdc (b,ns) = putStrLn $ printf "Base %d:" b
                  ++ concatMap (printf " %5s" . show) ns

-- To print the n'th van der Corput numbers for n in [2,3,4,5] call the program
-- with no arguments.  Otherwise, passing the base b, first n, next n and
-- maximum n will print the base b numbers for n in [firstN, nextN, ..., maxN].
main :: IO ()
main = do
  args <- getArgs
  let (bases, nums) = case args of
        [b, f, s, m] -> ([read b], [read f, read s..read m])
        _ -> ([2,3,4,5], [0..9])
  mapM_ printVdc [(b,rs) | b <- bases, let rs = map (vdc b) nums]
