import Control.Monad (liftM2)
import Data.List (group)

-- this function is composed out of many functions; data flows from the bottom up
lookAndSay :: Integer -> Integer
lookAndSay = read                                   -- convert digits to integer
           . concatMap                              -- concatenate for each run,
               (liftM2 (++) (show . length)         --    the length of it
                            (take 1))               --    and an example member
           . group                                  -- collect runs of the same digit
           . show                                   -- convert integer to digits

-- less comments
lookAndSay2 :: Integer -> Integer
lookAndSay2 = read . concatMap (liftM2 (++) (show . length)
                                            (take 1))
            . group . show


-- same thing with more variable names
lookAndSay3 :: Integer -> Integer
lookAndSay3 n = read (concatMap describe (group (show n)))
  where describe run = show (length run) ++ take 1 run

main = mapM_ print (iterate lookAndSay 1)           -- display sequence until interrupted
