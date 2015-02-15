import Data.List (partition, intersect, nub)
import Control.Monad
import System.Random (StdGen, getStdRandom, randomR)
import Text.Printf

numberOfDigits = 4 :: Int

main = bullsAndCows

bullsAndCows :: IO ()
bullsAndCows = do
    digits <- getStdRandom $ pick numberOfDigits ['1' .. '9']
    putStrLn "Guess away!"
    loop digits

  where loop digits = do
            input <- getLine
            if okay input
              then
                  let (bulls, cows) = score digits input in
                  if bulls == numberOfDigits then
                      putStrLn "You win!"
                  else do
                      printf "%d bulls, %d cows.\n" bulls cows
                      loop digits
              else do
                  putStrLn "Malformed guess; try again."
                  loop digits

        okay :: String -> Bool
        okay input =
            length input == numberOfDigits &&
            input == nub input &&
            all legalchar input
          where legalchar c = '1' <= c && c <= '9'

        score :: String -> String -> (Int, Int)
        score secret guess = (length bulls, cows)
          where (bulls, nonbulls) = partition (uncurry (==)) $
                    zip secret guess
                cows = length $ uncurry intersect $ unzip nonbulls

pick :: Int -> [a] -> StdGen -> ([a], StdGen)
{- Randomly selects items from a list without replacement. -}
pick n l g = f n l g (length l - 1) []
  where  f 0 _ g _   ps = (ps, g)
         f n l g max ps =
             f (n - 1) (left ++ right) g' (max - 1) (picked : ps)
          where (i, g') = randomR (0, max) g
                (left, picked : right) = splitAt i l
