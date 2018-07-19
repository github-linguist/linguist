import Data.List (maximumBy)
import Data.Ord (comparing)

(maxWgt, maxVol) = (25, 0.25)
items =
   [Bounty  "panacea"  3000  0.3  0.025,
    Bounty  "ichor"    1800  0.2  0.015,
    Bounty  "gold"     2500  2.0  0.002]

data Bounty = Bounty
   {itemName :: String,
    itemVal :: Int,
    itemWgt, itemVol :: Double}

names = map itemName items
vals = map itemVal items
wgts = map itemWgt items
vols = map itemVol items

dotProduct :: (Num a, Integral b) => [a] -> [b] -> a
dotProduct factors = sum . zipWith (*) factors . map fromIntegral

options :: [[Int]]
options = filter fits $ mapM f items
  where f (Bounty _ _ w v) = [0 .. m]
          where m = floor $ min (maxWgt / w) (maxVol / v)
        fits opt = dotProduct wgts opt <= maxWgt &&
                   dotProduct vols opt <= maxVol

showOpt :: [Int] -> String
showOpt opt = concat (zipWith showItem names opt) ++
    "total weight: " ++ show (dotProduct wgts opt) ++
    "\ntotal volume: " ++ show (dotProduct vols opt) ++
    "\ntotal value: " ++ show (dotProduct vals opt) ++ "\n"
  where showItem name num = name ++ ": " ++ show num ++ "\n"

main = putStr $ showOpt $ best options
  where best = maximumBy $ comparing $ dotProduct vals
