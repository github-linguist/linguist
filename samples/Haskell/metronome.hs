import Control.Concurrent
import Control.Concurrent.MVar
import System.Process (runCommand)

-- This program works only on the GHC compiler because of the use of
-- threadDelay

data Beep = Stop | Hi | Low

type Pattern = [Beep]

type BeatsPerMinute = Int

minute = 60000000 -- 1 minute = 60,000,000 microseconds

-- give one of the following example patterns to the metronome function

pattern4_4 = [Hi, Low, Low, Low]
pattern2_4 = [Hi, Low]
pattern3_4 = [Hi, Low, Low]
pattern6_8 = [Hi, Low, Low, Low, Low, Low]

-- use this version if you can't play audio, use Windows or don't
-- have audio files to play
-- beep :: Beep -> IO ()
-- beep Stop = return ()
-- beep Hi = putChar 'H'
-- beep Low = putChar 'L'

-- use this version if you can and want to play audio on Linux using
-- Alsa. Change the name of the files to those of your choice

beep Stop = return ()
beep Hi = putChar 'H' >> runCommand "aplay hi.wav &> /dev/null" >> return ()
beep Low = putChar 'L' >> runCommand "aplay low.wav &> /dev/null" >> return ()

tick :: MVar Pattern -> BeatsPerMinute -> IO ()
tick b i = do
    t <- readMVar b
    case t of
        [Stop] -> return ()
        x -> do
            mapM_ (\v -> forkIO (beep v) >> threadDelay (minute `div` i)) t
            tick b i

metronome :: Pattern -> BeatsPerMinute -> IO ()
metronome p i = do
    putStrLn "Press any key to stop the metronome."
    b <- newMVar p
    _ <- forkIO $ tick b i
    _ <- getChar
    putMVar b [Stop]
