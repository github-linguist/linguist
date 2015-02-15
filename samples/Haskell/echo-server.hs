module Main where
import Network (withSocketsDo, accept, listenOn, sClose, PortID(PortNumber))
import Control.Monad (forever)
import System.IO (hGetLine, hPutStrLn, hFlush, hClose)
import System.IO.Error (isEOFError)
import Control.Concurrent (forkIO)
import Control.Exception (bracket)

-- For convenience in testing, ensure that the listen socket is closed if the main loop is aborted
withListenOn port body = bracket (listenOn port) sClose body

echo (handle, host, port) = catch (forever doOneLine) stop where
  doOneLine = do line <- hGetLine handle
                 print (host, port, init line)
                 hPutStrLn handle line
                 hFlush handle
  stop error = do putStrLn $ "Closed connection from " ++ show (host, port) ++ " due to " ++ show error
                  hClose handle

main = withSocketsDo $
  withListenOn (PortNumber 12321) $ \listener ->
    forever $ do
      acc@(_, host, port) <- accept listener
      putStrLn $ "Accepted connection from " ++ show (host, port)
      forkIO (echo acc)
