import Char
import Control.Monad.Error
import Data.List
import IO
import Maybe
import Random

main = do
    hSetBuffering stdout NoBuffering
    mapM_ putStrLn
        [ "THE 24 GAME\n"
        , "Given four digits in the range 1 to 9"
        , "Use the +, -, *, and / operators in reverse polish notation"
        , "To show how to make an answer of 24.\n"
        ]
    digits <- liftM (sort . take 4 . randomRs (1,9)) getStdGen :: IO [Int]
    putStrLn ("Your digits: " ++ intercalate " " (map show digits))
    guessLoop digits
    where guessLoop digits =
              putStr "Your expression: " >>
              liftM (processGuess digits . words) getLine >>=
              either (\m -> putStrLn m >> guessLoop digits) putStrLn

processGuess _      [] = Right ""
processGuess digits xs | not $ matches = Left "Wrong digits used"
    where matches = digits == (sort . map read $ filter (all isDigit) xs)
processGuess digits xs = calc xs >>= check
    where check 24 = Right "Correct"
          check x  = Left (show (fromRational (x :: Rational)) ++ " is wrong")

-- A Reverse Polish Notation calculator with full error handling
calc = result []
    where result [n] [] = Right n
          result _   [] = Left "Too few operators"
          result ns  (x:xs) = simplify ns x >>= flip result xs

simplify (a:b:ns) s | isOp s = Right ((fromJust $ lookup s ops) b a : ns)
simplify _        s | isOp s = Left ("Too few values before " ++ s)
simplify ns       s | all isDigit s = Right (fromIntegral (read s) : ns)
simplify _        s = Left ("Unrecognized symbol: " ++ s)

isOp v = elem v $ map fst ops

ops = [("+",(+)), ("-",(-)), ("*",(*)), ("/",(/))]
