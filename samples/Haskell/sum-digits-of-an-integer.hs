digsum base = f 0 where
	f a 0 = a
	f a n = f (a+r) q where
		(q,r) = n `divMod` base

main = print $ digsum 16 255 -- "FF": 15 + 15 = 30
