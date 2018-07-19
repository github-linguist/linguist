import System.IO
import System.Directory

check :: (FilePath -> IO Bool) -> FilePath -> IO ()
check p s = do
  result <- p s
  putStrLn $ s ++ if result then " does exist" else " does not exist"

main = do
  check doesFileExist "input.txt"
  check doesDirectoryExist "docs"
  check doesFileExist "/input.txt"
  check doesDirectoryExist "/docs"
