import System.Environment
import Control.Concurrent
import Control.Monad

sleepSort :: [Int] -> IO ()
sleepSort values = do
	chan <- newChan
	forM_ values (\time -> forkIO (threadDelay (50000 * time) >> writeChan chan time))
	forM_ values (const (readChan chan >>= print))

main :: IO ()
main = getArgs >>= sleepSort . map read
