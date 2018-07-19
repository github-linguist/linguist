-- Same merge as in Merge Sort
merge :: (Ord a) => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x : xs) (y : ys)
	| x <= y = x : merge xs (y : ys)
	| otherwise = y : merge (x : xs) ys

strandSort :: (Ord a) => [a] -> [a]
strandSort [] = []
strandSort (x : xs) = merge strand (strandSort rest) where
	(strand, rest) = extractStrand x xs
	extractStrand x [] = ([x], [])
	extractStrand x (x1 : xs)
		| x <= x1 = let (strand, rest) = extractStrand x1 xs in (x : strand, rest)
		| otherwise = let (strand, rest) = extractStrand x xs in (strand, x1 : rest)
