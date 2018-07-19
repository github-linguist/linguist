import System.Environment
import Data.List
import Data.Char
import Data.Maybe

main = do
        x <- mapM lookupEnv ["LANG", "LC_ALL", "LC_CTYPE"]
        if any (isInfixOf "UTF". map toUpper) $ catMaybes x
         then putStrLn "UTF supported: \x25b3"
         else putStrLn "UTF not supported"
