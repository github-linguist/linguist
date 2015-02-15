import qualified Control.Exception as C
check x y = C.catch (x `div` y `seq` return False)
                    (\_ -> return True)
