map (>5) [1..10] -- [False,False,False,False,False,True,True,True,True,True]

map (++ "s") ["Apple", "Orange", "Mango", "Pear"] -- ["Apples","Oranges","Mangos","Pears"]

foldr (+) 0 [1..10] -- prints 55

traverse :: [a] -> [a]
traverse list = map func list
	where func a = -- ...do something with a
