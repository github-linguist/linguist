import Control.Monad
import System.Random

-- Repeat the action until the predicate is true.
until_ act pred = act >>= pred >>= flip unless (until_ act pred)

answerIs ans guess =
  case compare ans guess of
    LT -> putStrLn "Too high. Guess again." >> return False
    EQ -> putStrLn "You got it!" >> return True
    GT -> putStrLn "Too low. Guess again." >> return False

-- Repeatedly read until the input *starts* with a number.  (Since
-- we use "reads" we allow garbage to follow it, though.)
ask = do line <- getLine
         case reads line of
           ((num,_):_) -> return num
           otherwise -> putStrLn "Please enter a number." >> ask

main = do
  ans <- randomRIO (1,100) :: IO Int
  putStrLn "Try to guess my secret number between 1 and 100."
  ask `until_` answerIs ans
