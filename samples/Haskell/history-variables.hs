import Data.IORef

newtype HVar a = HVar (IORef [a])

newHVar :: a -> IO (HVar a)
newHVar value = fmap HVar (newIORef [value])

readHVar :: HVar a -> IO a
readHVar (HVar ref) = fmap head (readIORef ref)

writeHVar :: a -> HVar a -> IO ()
writeHVar value (HVar ref) = modifyIORef ref (value:)

undoHVar :: HVar a -> IO ()
undoHVar (HVar ref) = do
    (_ : history) <- readIORef ref
    writeIORef ref history

getHistory :: HVar a -> IO [a]
getHistory (HVar ref) = readIORef ref

-- Testing
main :: IO ()
main = do
    var <- newHVar 0
    writeHVar 1 var
    writeHVar 2 var
    writeHVar 3 var
    getHistory var >>= print
    undoHVar var
    undoHVar var
    undoHVar var
