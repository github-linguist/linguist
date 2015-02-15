import Data.Bits (Bits(testBit, bitSize))
import Data.List (partition)

lsdSort :: (Ord a, Bits a) => [a] -> [a]
lsdSort = fixSort positiveLsdSort

msdSort :: (Ord a, Bits a) => [a] -> [a]
msdSort = fixSort positiveMsdSort

-- Fix a sort that puts negative numbers at the end, like positiveLsdSort and positiveMsdSort
fixSort sorter list = uncurry (flip (++)) (break (< 0) (sorter list))

positiveLsdSort :: (Bits a) => [a] -> [a]
positiveLsdSort list = foldl step list [0..bitSize (head list)] where
	step list bit = uncurry (++) (partition (not . flip testBit bit) list)

positiveMsdSort :: (Bits a) => [a] -> [a]
positiveMsdSort list = aux (bitSize (head list) - 1) list where
	aux _ [] = []
	aux (-1) list = list
	aux bit list = aux (bit - 1) lower ++ aux (bit - 1) upper where
		(lower, upper) = partition (not . flip testBit bit) list
