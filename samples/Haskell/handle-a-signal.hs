import Prelude hiding (catch)
import Control.Exception (catch, throwIO, AsyncException(UserInterrupt))
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Control.Concurrent (threadDelay)

main = do t0 <- getCurrentTime
          catch (loop 0)
                (\e -> if e == UserInterrupt
                         then do t1 <- getCurrentTime
                                 putStrLn ("\nTime: " ++ show (diffUTCTime t1 t0))
                         else throwIO e)

loop i = do print i
            threadDelay 500000 {- µs -}
            loop (i + 1)
