import Data.Map

makeMap ks vs = fromList $ zip ks vs
mymap = makeMap ['a','b','c'] [1,2,3]
