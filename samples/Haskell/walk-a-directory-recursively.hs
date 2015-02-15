import System.Environment
import System.Directory
import System.FilePath.Find

search pat dir =
  find always (fileName ~~? pat) dir

main = do [pat] <- getArgs
          dir   <- getCurrentDirectory
          files <- search pat dir
          mapM_ putStrLn files
