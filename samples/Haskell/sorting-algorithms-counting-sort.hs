import Data.Array

countingSort :: (Ix n) => [n] -> n -> n -> [n]
countingSort l lo hi = concatMap (uncurry $ flip replicate) count
  where count = assocs . accumArray (+) 0 (lo, hi) . map (\i -> (i, 1)) $ l
