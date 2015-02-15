import Data.List
import Control.Monad
import Control.Arrow
import Rosetta.Knuthshuffle

task = do
  let blanco = replicate (31*31) "  "
      cs = sequence [[-15,-14..15],[-15,-14..15]] :: [[Int]]
      constraint = uncurry(&&).((<= 15*15) &&& (10*10 <=)). sum. map (join (*))
-- select and randomize all circle points
  pts <- knuthShuffle $ filter constraint cs
-- 'paint' first 100 randomized circle points on canvas
  let canvas = foldl (\cs [x,y] -> replaceAt (31*(x+15)+y+15) "/ " cs ) blanco (take 100 pts)
-- show canvas
  mapM_ (putStrLn.concat). takeWhile(not.null). unfoldr (Just . splitAt 31) $ canvas
