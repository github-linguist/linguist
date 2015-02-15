import Graphics.HGL.Window
import Graphics.HGL.Run
import Control.Arrow
import Control.Monad
import Data.List

enumBase :: Int -> Int -> [[Int]]
enumBase n = mapM (enumFromTo 0). replicate n. pred

psPlus (a,b) (p,q) = (a+p, b+q)

toInt :: Double -> Int
toInt = fromIntegral.round

intPoint = toInt *** toInt

pts n =
  map (map (intPoint.psPlus (100,0)). ((0,300):). scanl1 psPlus. ((r,300):). zipWith (\h a -> (h*cos a, h*sin a)) rs) hs
  where
    [r,h,sr,sh] = [50, pi/5, 0.9, 0.75]
    rs   = take n $ map (r*) $ iterate(*sr) sr
    lhs  = map (map (((-1)**).fromIntegral)) $ enumBase n 2
    rhs  = take n $ map (h*) $ iterate(*sh) 1
    hs   = map (scanl1 (+). zipWith (*)rhs) lhs

fractalTree :: Int -> IO ()
fractalTree n =
   runWindow "Fractal Tree" (500,600)
    (\w -> setGraphic w (overGraphics ( map polyline $ pts (n-1))) >> getKey w)
