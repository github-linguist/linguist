import System.Random
import Data.List
import Control.Monad
import Control.Arrow

labels = ["aleph", "beth", "gimel", "daleth", "he", "waw", "zayin", "heth" ]
piv n = take n . (++  repeat ' ')

main = do
  g <- newStdGen
  let rs,ps :: [Float]
      rs = take 1000000 $ randomRs(0,1) g
      ps = ap (++) (return. (1 -) .sum) $ map recip [5..11]
      sps = scanl1 (+) ps
      qs = (\xs -> map ((/1000000.0).fromIntegral.length. flip filter xs. (==))sps)
           $ map (head . flip dropWhile sps . (>)) rs
  putStrLn $ "       expected     actual"
  mapM_ putStrLn $ zipWith3
               (\l s c-> (piv 7 l) ++ (piv 13 $ show $ s)
                    ++(piv 12 $ show $ c)) labels ps qs
