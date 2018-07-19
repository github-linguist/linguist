import System.IO

readLines :: Handle -> IO [String]
readLines h = do
  s <- hGetContents h
  return $ lines s

readWords :: Handle -> IO [String]
readWords h = do
  s <- hGetContents h
  return $ words s
