import System.Environment (getArgs)
import System.Time (getClockTime)

main :: IO ()
main = do
  args <- getArgs
  if null args
    then catch (readFile "notes.txt" >>= putStr)
               (\_ -> return ())
    else
      do ct <- getClockTime
         appendFile "notes.txt" $ show ct ++ "\n\t" ++ unwords args ++ "\n"
