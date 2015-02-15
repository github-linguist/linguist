module Spiral where

import Data.List
import Control.Monad
import Control.Monad.Instances

grade xs = map snd. sort $ zip xs [0..]
values n = cycle [1,n,-1,-n]
counts n = (n:).concatMap (ap (:) return)  $ [n-1,n-2..1]
reshape n = unfoldr (\xs -> if null xs then Nothing else Just (splitAt n xs))

spiral n = reshape n . grade. scanl1 (+). concat $ zipWith replicate (counts n) (values n)
