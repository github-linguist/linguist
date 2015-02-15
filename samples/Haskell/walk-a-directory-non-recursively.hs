import System.Directory
import Text.Regex
import Data.Maybe

walk :: FilePath -> String -> IO ()
walk dir pattern = do
    filenames <- getDirectoryContents dir
    mapM_ putStrLn $ filter (isJust.(matchRegex $ mkRegex pattern)) filenames

main = walk "." ".\\.hs$"
