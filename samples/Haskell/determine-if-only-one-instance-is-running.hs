import Control.Concurrent
import System.Directory (doesFileExist, getAppUserDataDirectory,
    removeFile)
import System.IO (withFile, Handle, IOMode(WriteMode), hPutStr)

oneInstance :: IO ()
oneInstance = do
    -- check if file "$HOME/.myapp.lock" exists
    user <- getAppUserDataDirectory "myapp.lock"
    locked <- doesFileExist user
    if locked
    then print "There is already one instance of this program running."
    else do
        t <- myThreadId
        -- this is the entry point to the main program:
        -- withFile creates a file, then calls a function,
        -- then closes the file
        withFile user WriteMode (do_program t)
        -- remove the lock when we're done
        removeFile user

do_program :: ThreadId -> Handle -> IO ()
do_program t h = do
    let s = "Locked by thread: " ++ show t
    -- print what thread has acquired the lock
    putStrLn s
    -- write the same message to the file, to show that the
    -- thread "owns" the file
    hPutStr h s
    -- wait for one second
    threadDelay 1000000

main :: IO ()
main = do
    -- launch the first thread, which will create the lock file
    forkIO oneInstance
    -- wait for half a second
    threadDelay 500000
    -- launch the second thread, which will find the lock file and
    -- thus will exit immediately
    forkIO oneInstance
    return ()
