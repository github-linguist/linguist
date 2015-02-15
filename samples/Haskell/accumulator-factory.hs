import Control.Monad.ST
import Data.STRef

accumulator :: (Num a) => a -> ST s (a -> ST s a)
accumulator sum0 = do
  sum <- newSTRef sum0
  return $ \n -> do
    modifySTRef sum (+ n)
    readSTRef sum

main :: IO ()
main = print foo
    where foo = runST $ do
                  x <- accumulator 1
                  x 5
                  accumulator 3
                  x 2.3
