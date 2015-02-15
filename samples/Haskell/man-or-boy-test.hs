 import Control.Monad
 import Data.IORef

 a k x1 x2 x3 x4 x5 = do r <- newIORef k
                         let b = do k <- pred !r
                                    a k b x1 x2 x3 x4
                         if k <= 0 then liftM2 (+) x4 x5 else b
     where f !r = modifyIORef r f >> readIORef r

 main = a 10 #1 #(-1) #(-1) #1 #0 >>= print
     where (#) f = f . return
