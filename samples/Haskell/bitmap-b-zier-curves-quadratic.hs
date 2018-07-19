{-# LANGUAGE
    FlexibleInstances, TypeSynonymInstances,
    ViewPatterns #-}

import Bitmap
import Bitmap.Line
import Control.Monad
import Control.Monad.ST

type Point = (Double, Double)
fromPixel (Pixel (x, y)) = (toEnum x, toEnum y)
toPixel (x, y) = Pixel (round x, round y)

pmap :: (Double -> Double) -> Point -> Point
pmap f (x, y) = (f x, f y)

onCoordinates :: (Double -> Double -> Double) -> Point -> Point -> Point
onCoordinates f (xa, ya) (xb, yb) = (f xa xb, f ya yb)

instance Num Point where
    (+) = onCoordinates (+)
    (-) = onCoordinates (-)
    (*) = onCoordinates (*)
    negate = pmap negate
    abs = pmap abs
    signum = pmap signum
    fromInteger i = (i', i')
      where i' = fromInteger i

bézier :: Color c =>
    Image s c -> Pixel -> Pixel -> Pixel -> c -> Int ->
    ST s ()
bézier
  i
  (fromPixel -> p1) (fromPixel -> p2) (fromPixel -> p3)
  c samples =
    zipWithM_ f ts (tail ts)
  where ts = map (/ top) [0 .. top]
          where top = toEnum $ samples - 1
        curvePoint t =
            pt (t' ^^ 2) p1 +
            pt (2 * t * t') p2 +
            pt (t ^^ 2) p3
          where t' = 1 - t
                pt n p = pmap (*n) p
        f (curvePoint -> p1) (curvePoint -> p2) =
            line i (toPixel p1) (toPixel p2) c
