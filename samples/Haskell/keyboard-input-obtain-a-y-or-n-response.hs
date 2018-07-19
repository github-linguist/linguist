import System.IO

hFlushInput :: Handle -> IO ()
hFlushInput hdl = do
  r <- hReady hdl
  if r then do
    c <- hGetChar hdl
    hFlushInput hdl
  else
    return ()

yorn :: IO Char
yorn = do
  c <- getChar
  if c == 'Y' || c == 'N' then return c
  else if c == 'y' then return 'Y'
  else if c == 'n' then return 'N'
  else yorn

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStr "Press Y or N to continue: "

  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  hFlushInput stdin
  answer <- yorn
  putStrLn [answer]
