import Data.List (genericLength)
import Data.STRef
import Control.Monad.ST

sd :: RealFloat a => [a] -> a
sd l = sqrt $ sum (map ((^2) . subtract mean) l) / n
  where n = genericLength l
        mean = sum l / n

sdAccum :: RealFloat a => ST s (a -> ST s a)
sdAccum = do
    accum <- newSTRef []
    return $ \x -> do
        modifySTRef accum (x:)
        list <- readSTRef accum
        return $ sd list

main = mapM_ print results
  where results = runST $ do
                    runningSD <- sdAccum
                    mapM runningSD [2, 4, 4, 4, 5, 5, 7, 9]
