import qualified Data.Set as S

semordnilaps = snd . foldl f (S.empty,[]) where
	f (s,w) x | S.member (reverse x) s = (s, x:w)
	          | otherwise = (S.insert x s, w)

main=do	s <- readFile "unixdict.txt"
	let l = semordnilaps (lines s)
	print $ length l
	mapM_ print $ map (\x->(x, reverse x)) $ take 5 l
