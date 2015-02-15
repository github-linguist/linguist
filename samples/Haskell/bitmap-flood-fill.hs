import Data.Array.ST
import Data.STRef
import Control.Monad
import Control.Monad.ST
import Bitmap

-- Implementation of a stack in the ST monad
pushST :: STStack s a -> a -> ST s ()
pushST s e = do
    s2 <- readSTRef s
    writeSTRef s (e : s2)

popST :: STStack s a -> ST s (Stack a)
popST s = do
    s2 <- readSTRef s
    writeSTRef s $ tail s2
    return $ take 1 s2

isNotEmptySTStack :: STStack s a -> ST s Bool
isNotEmptySTStack s = do
    s2 <- readSTRef s
    return $ not $ null s2

emptySTStack :: ST s (STStack s a)
emptySTStack = newSTRef []

consumeSTStack :: STStack s a -> (a -> ST s ()) -> ST s ()
consumeSTStack s f = do
    check <- isNotEmptySTStack s
    when check $ do
        e <- popST s
        f $ head e
        consumeSTStack s f

type Spanning s = STRef s (Bool, Bool)

setSpanLeft :: Spanning s -> Bool -> ST s ()
setSpanLeft s v = do
    (_, r) <- readSTRef s
    writeSTRef s (v, r)

setSpanRight :: Spanning s -> Bool -> ST s ()
setSpanRight s v = do
    (l, _) <- readSTRef s
    writeSTRef s (l, v)

setSpanNone :: Spanning s -> ST s ()
setSpanNone s = writeSTRef s (False, False)

floodFillScanlineStack :: Color c => Image s c -> Pixel -> c -> ST s (Image s c)
floodFillScanlineStack b coords newC = do
    stack <- emptySTStack -- new empty stack holding pixels to fill
    spans <- newSTRef (False, False) -- keep track of spans in scanWhileX
    fFSS b stack coords newC spans -- function loop
    return b
    where
        fFSS b st c newC p = do
            oldC <- getPix b c
            unless (oldC == newC) $ do
                pushST st c -- store the coordinates in the stack
                (w, h) <- dimensions b
                consumeSTStack st (scanWhileY b p oldC >=>
                    scanWhileX b st p oldC newC (w, h))

        -- take a buffer, the span record, the color of the Color the fill is
        -- started from, a coordinate from the stack, and returns the coord
        -- of the next point to be filled in the same column
        scanWhileY b p oldC coords@(Pixel (x, y)) =
            if y >= 0
            then do
                z <- getPix b coords
                if z == oldC
                then scanWhileY b p oldC (Pixel (x, y - 1))
                else do
                    setSpanNone p
                    return (Pixel (x, y + 1))
            else do
                setSpanNone p
                return (Pixel (x, y + 1))

        -- take a buffer, a stack, a span record, the old and new color, the
        -- height and width of the buffer, and a coordinate.
        -- paint the point with the new color, check if the fill must expand
        -- to the left or right or both, and store those coordinates in the
        -- stack for subsequent filling
        scanWhileX b st p oldC newC (w, h) coords@(Pixel (x, y)) =
            when (y < h) $ do
                z <- getPix b coords
                when (z == oldC) $ do
                    setPix b coords newC
                    (spanLeft, spanRight) <- readSTRef p
                    when (not spanLeft && x > 0) $ do
                        z2 <- getPix b (Pixel (x - 1, y))
                        when (z2 == oldC) $ do
                            pushST st (Pixel (x - 1, y))
                            setSpanLeft p True
                    when (spanLeft && x > 0) $ do
                        z3 <- getPix b (Pixel (x - 1, y))
                        when (z3 /= oldC) $
                            setSpanLeft p False
                    when (not spanRight && x < (w - 1)) $ do
                        z4 <- getPix b (Pixel (x + 1, y))
                        when (z4 == oldC) $ do
                            pushST st (Pixel (x + 1, y))
                            setSpanRight p True
                    when (spanRight && x < (w - 1)) $ do
                        z5 <- getPix b (Pixel (x + 1, y))
                        when (z5 /= oldC) $
                            setSpanRight p False
                    scanWhileX b st p oldC newC (w, h) (Pixel (x, y + 1))
