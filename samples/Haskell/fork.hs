import System.Posix.Process

main = do
  forkProcess (putStrLn "This is the new process")
  putStrLn "This is the original process"
