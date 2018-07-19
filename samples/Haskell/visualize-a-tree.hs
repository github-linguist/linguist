data Tree a = Empty | Node { value :: a, left :: Tree a, right :: Tree a }
	deriving (Show, Eq)

tree = Node 1 (Node 2 (Node 4 (Node 7 Empty Empty) Empty)
	(Node 5 Empty Empty)) (Node 3 (Node 6 (Node 8 Empty Empty)
	(Node 9 Empty Empty)) Empty)

treeIndent Empty = ["-- (nil)"]
treeIndent t = ["--" ++ show (value t)]
	++ map ("  |"++) ls ++ ("  `" ++ r):map ("   "++) rs
	where
	(r:rs) = treeIndent$right t
	ls     = treeIndent$left t

main = mapM_ putStrLn $ treeIndent tree
