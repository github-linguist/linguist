import System.IO
import System.Directory

main = do
  renameFile "input.txt" "output.txt"
  renameDirectory "docs" "mydocs"
  renameFile "/input.txt" "/output.txt"
  renameDirectory "/docs" "/mydocs"
