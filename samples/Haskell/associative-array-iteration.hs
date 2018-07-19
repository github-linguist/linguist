import qualified Data.Map as M

myMap = M.fromList [("hello", 13), ("world", 31), ("!", 71)]

main = do -- pairs
          print $ M.toList myMap
          -- keys
          print $ M.keys myMap
          -- values
          print $ M.elems myMap
