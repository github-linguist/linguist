import System.Directory

createFile name = writeFile name ""

main = do
  createFile "output.txt"
  createDirectory "docs"
  createFile "/output.txt"
  createDirectory "/docs"
