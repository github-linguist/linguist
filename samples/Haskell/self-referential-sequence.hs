import Data.Set (Set, member, insert, empty)
import Data.List (group, sort)

step :: String -> String
step = concatMap (\list -> show (length list) ++ [head list]) . group . sort

findCycle :: (Ord a) => [a] -> [a]
findCycle = aux empty where
	aux set (x : xs)
		| x `member` set = []
		| otherwise = x : aux (insert x set) xs

select :: [[a]] -> [[a]]
select = snd . foldl (\(len, acc) xs -> case len `compare` length xs of
		LT -> (length xs, [xs])
		EQ -> (len, xs : acc)
		GT -> (len, acc)) (0, [])

main :: IO ()
main = mapM_ (mapM_ print) $ -- Print out all the numbers
	select $ -- find the longest ones
	map findCycle $ -- run the sequences until there is a repeat
	map (iterate step) $ -- produce the sequence
	map show -- turn the numbers into digits
	[1..1000000] -- The input seeds
