import Data.List
import Control.Monad
import Control.Arrow

missingPerm :: Eq a => [[a]] -> [[a]]
missingPerm = (\\) =<< permutations . nub . join

deficientPermsList = ["ABCD","CABD","ACDB","DACB",
                      "BCDA","ACBD","ADCB","CDAB",
                      "DABC","BCAD","CADB","CDBA",
                      "CBAD","ABDC","ADBC","BDCA",
                      "DCBA","BACD","BADC","BDAC",
                      "CBDA","DBCA","DCAB"]

main = do
    print $ missingPerm deficientPermsList
