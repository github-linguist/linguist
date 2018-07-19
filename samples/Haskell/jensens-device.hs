import Control.Monad
import Control.Monad.ST
import Data.STRef

sum' ref_i lo hi term =
  return sum `ap`
         mapM (\i -> writeSTRef ref_i i >> term) [lo..hi]

foo = runST $ do
        i <- newSTRef undefined -- initial value doesn't matter
        sum' i 1 100 $ return recip `ap` readSTRef i

main = print foo
