import Control.Monad
import Control.Monad.ST
import Data.STRef
import Data.Array.ST
import System.Random
import Bitmap
import Bitmap.BW
import Bitmap.Netpbm

main = do
    g <- getStdGen
    (t, _) <- stToIO $ drawTree (50, 50) (25, 25) 300 g
    writeNetpbm "/tmp/tree.pbm" t

drawTree :: (Int, Int) -> (Int, Int) -> Int -> StdGen -> ST s (Image s BW, StdGen)
drawTree (width, height) start steps stdgen = do
    img <- image width height off
    setPix img (Pixel start) on
    gen <- newSTRef stdgen
    let -- randomElem :: [a] -> ST s a
        randomElem l = do
            stdgen <- readSTRef gen
            let (i, stdgen') = randomR (0, length l - 1) stdgen
            writeSTRef gen stdgen'
            return $ l !! i
        -- newPoint :: ST s (Int, Int)
        newPoint = do
            p <- randomElem border
            c <- getPix img $ Pixel p
            if c == off then return p else newPoint
        -- wander :: (Int, Int) -> ST s ()
        wander p = do
            next <- randomElem $ filter (inRange pointRange) $ adjacent p
            c <- getPix img $ Pixel next
            if c == on then setPix img (Pixel p) on else wander next
    replicateM_ steps $ newPoint >>= wander
    stdgen <- readSTRef gen
    return (img, stdgen)
  where pointRange = ((0, 0), (width - 1, height - 1))
        adjacent (x, y) = [(x - 1, y - 1), (x, y - 1), (x + 1, y - 1),
                           (x - 1, y),                 (x + 1, y),
                           (x - 1, y + 1), (x, y + 1), (x + 1, y + 1)]
        border = liftM2 (,) [0, width - 1] [0 .. height - 1] ++
                 liftM2 (,) [1 .. width - 2] [0, height - 1]
        off = black
        on = white
