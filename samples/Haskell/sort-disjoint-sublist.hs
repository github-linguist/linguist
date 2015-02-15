import Control.Monad
import qualified Data.Array as A
import Data.Array.IArray
import Data.Array.ST
import Data.List
import Data.List.Utils

-- Partition 'xs' according to whether their element indices are in 'is'.  Sort
-- the sublist corresponding to 'is', merging the result with the remainder of
-- the list.
disSort1 :: (Ord a, Num a, Enum a, Ord b) => [b] -> [a] -> [b]
disSort1 xs is = let is' = sort is
                     (sub, rest) = partition ((`elem` is') . fst) $ zip [0..] xs
                 in map snd . merge rest . zip is' . sort $ map snd sub

-- Convert the list to an array.  Extract the sublist corresponding to the
-- indices 'is'.  Sort the sublist, replacing those elments in the array.
disSort2 :: (Ord a) => [a] -> [Int] -> [a]
disSort2 xs is = let as = A.listArray (0, length xs - 1) xs
                     sub = zip (sort is) . sort $ map (as !) is
                 in elems $ as // sub

-- Similar to disSort2, but using mutable arrays.  The sublist is updated
-- "in place", rather than creating a new array.  However, this is not visible
-- to a caller.
disSort3 :: [Int] -> [Int] -> [Int]
disSort3 xs is = elems . runSTUArray $ do
                   as <- newListArray (0, length xs - 1) xs
                   sub <- liftM (zip (sort is) . sort) $ mapM (readArray as) is
                   mapM_ (uncurry (writeArray as)) sub
                   return as

main = do
  let xs = [7, 6, 5, 4, 3, 2, 1, 0]
      is = [6, 1, 7]
  print $ disSort1 xs is
  print $ disSort2 xs is
  print $ disSort3 xs is
