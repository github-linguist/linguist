import Data.List (findIndices)

tf = mapM (\_ -> [1,0])

wrongness b = findIndices id . zipWith (/=) b . map (fromEnum . ($ b))

statements = [	(==12) . length,
		3 ⊂ [length statements-6..],
		2 ⊂ [1,3..],
		4 → [4..6],
		0 ⊂ [1..3],
		4 ⊂ [0,2..],
		1 ⊂ [1,2],
		6 → [4..6],
		3 ⊂ [0..5],
		2 ⊂ [10,11],
		1 ⊂ [6,7,8],
		4 ⊂ [0..10]
	] where
	(s ⊂ x) b = s == (sum . map (b!!) . takeWhile (< length b)) x
	(a → x) b = (b!!a == 0) || all ((==1).(b!!)) x

testall s n = [(b, w) | b <- tf s, w <- [wrongness b s], length w == n]

main = let t = testall statements in do
	putStrLn "Answer"
	mapM_ print $ t 0
	putStrLn "Near misses"
	mapM_ print $ t 1
