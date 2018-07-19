import System.IO
import System.Directory

main = do
  removeFile "output.txt"
  removeDirectory "docs"
  removeFile "/output.txt"
  removeDirectory "/docs"
