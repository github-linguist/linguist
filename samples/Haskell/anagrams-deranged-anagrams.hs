import Control.Arrow
import Data.List
import Data.Ord
import qualified Data.Map as M
import qualified Data.Set as S

-- Group lists of words based on their "signatures".  A signature is a sorted
-- list of characters.  Handle duplicate input words by storing them in sets.
groupBySig = map (sort &&& S.singleton)

-- Convert groups to lists of equivalent words.
equivs = map (S.toList . snd) . M.toList . M.fromListWith S.union

-- Indicate whether the pair of words differ in all character positions.
isDerangement (a, b) = and $ zipWith (/=) a b

-- Return all pairs of elements, ignoring order.
pairs = concat . unfoldr step
  where step (x:xs) = Just (map ((,) x) xs, xs)
        step []     = Nothing

-- Return all anagram pairs in the input string.
anagrams = concatMap pairs . equivs . groupBySig

-- Return the pair of words making the longest deranged anagram.
maxDerangedAnagram = maxByLen . filter isDerangement . anagrams
  where maxByLen [] = Nothing
        maxByLen xs = Just $ maximumBy (comparing (length . fst)) xs

main :: IO ()
main = do
  input <- getContents
  case maxDerangedAnagram $ words input of
    Nothing     -> putStrLn "No deranged anagrams were found."
    Just (a, b) -> putStrLn $ "Longest deranged anagrams: " ++ a ++ " and " ++ b
