import Data.Char
import Data.Maybe
import Text.Printf

dirs = ["N", "NbE", "N-NE", "NEbN", "NE", "NEbE", "E-NE", "EbN",
        "E", "EbS", "E-SE", "SEbE", "SE", "SEbS", "S-SE", "SbE",
        "S", "SbW", "S-SW", "SWbS", "SW", "SWbW", "W-SW", "WbS",
        "W", "WbN", "W-NW", "NWbW", "NW", "NWbN", "N-NW", "NbW"]

-- Given an index between 0 and 31 return the corresponding compass point name.
pointName = capitalize . concatMap (fromMaybe "?" . fromChar) . (dirs !!)
  where fromChar c = lookup c [('N', "north"), ('S', "south"), ('E', "east"),
                               ('W', "west"),  ('b', " by "),  ('-', "-")]
        capitalize (c:cs) = toUpper c : cs

-- Convert degrees to a compass point index between 0 and 31.
pointIndex d = (round (d*1000) + 5625) `mod` 360000 `div` 11250

printPointName d = let deg = read d :: Double
                       idx = pointIndex deg
                   in printf "%2d  %-18s  %6.2f°\n" (idx+1) (pointName idx) deg

main = do
  input <- getContents
  mapM_ printPointName $ lines input
