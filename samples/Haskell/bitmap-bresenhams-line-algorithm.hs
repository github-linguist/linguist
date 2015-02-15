module Bitmap.Line(line) where

import Bitmap
import Control.Monad
import Control.Monad.ST
import qualified Data.STRef

var = Data.STRef.newSTRef
get = Data.STRef.readSTRef
mutate = Data.STRef.modifySTRef

line :: Color c => Image s c -> Pixel -> Pixel -> c -> ST s ()
line i (Pixel (xa, ya)) (Pixel (xb, yb)) c = do
    yV <- var y1
    errorV <- var $ deltax `div` 2
    forM_ [x1 .. x2] (\x -> do
        y <- get yV
        setPix i (Pixel $ if steep then (y, x) else (x, y)) c
        mutate errorV $ subtract deltay
        error <- get errorV
        when (error < 0) (do
            mutate yV (+ ystep)
            mutate errorV (+ deltax)))
  where steep = abs (yb - ya) > abs (xb - xa)
        (xa', ya', xb', yb') = if steep
          then (ya, xa, yb, xb)
          else (xa, ya, xb, yb)
        (x1, y1, x2, y2) = if xa' > xb'
          then (xb', yb', xa', ya')
          else (xa', ya', xb', yb')
        deltax = x2 - x1
        deltay = abs $ y2 - y1
        ystep = if y1 < y2 then 1 else -1
