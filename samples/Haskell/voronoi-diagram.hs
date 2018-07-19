-- Compile with: ghc -O2 -fllvm -fforce-recomp -threaded --make
{-# LANGUAGE BangPatterns #-}
module Main where

import System.Random

import Data.Word
import Data.Array.Repa as Repa
import Data.Array.Repa.IO.BMP

{-# INLINE sqDistance #-}
sqDistance :: Word32 -> Word32 -> Word32 -> Word32 -> Word32
sqDistance !x1 !y1 !x2 !y2 = ((x1-x2)^2) + ((y1-y2)^2)

centers :: Int -> Int -> Array U DIM2 Word32
centers nCenters nCells =
    fromListUnboxed (Z :. nCenters :. 2) $ take (2*nCenters) $ randomRs (0, fromIntegral nCells) (mkStdGen 1)

applyReduce2 arr f =
    traverse arr (\(i :. j) -> i) $ \lookup (Z:.i) ->
        f (lookup (Z:.i:.0)) (lookup (Z:.i:.1))

minimize1D arr = foldS f h t
  where
    indexed arr = traverse arr id (\src idx@(Z :. i) -> (src idx, (fromIntegral i)))
    (Z :. n) = extent arr
    iarr = indexed arr
    h = iarr ! (Z :. 0)
    t = extract (Z :. 1) (Z :. (n-1)) iarr

    f min@(!valMin, !iMin ) x@(!val, !i) | val < valMin = x
                                         | otherwise = min

voronoi :: Int -> Int -> Array D DIM2 Word32
voronoi nCenters nCells =
    let
      {-# INLINE cellReducer #-}
      cellReducer = applyReduce2 (centers nCenters nCells)
      {-# INLINE nearestCenterIndex #-}
      nearestCenterIndex = snd . (Repa.! Z) . minimize1D
    in
      Repa.fromFunction (Z :. nCells :. nCells :: DIM2) $ \ (Z:.i:.j) ->
          nearestCenterIndex $ cellReducer (sqDistance (fromIntegral i) (fromIntegral j))

genColorTable :: Int -> Array U DIM1 (Word8, Word8, Word8)
genColorTable n = fromListUnboxed (Z :. n) $ zip3 l1 l2 l3
    where
      randoms = randomRs (0,255) (mkStdGen 1)
      (l1, rest1) = splitAt n randoms
      (l2, rest2) = splitAt n rest1
      l3 = take n rest2

colorize :: Array U DIM1 (Word8, Word8, Word8) -> Array D DIM2 Word32 -> Array D DIM2 (Word8, Word8, Word8)
colorize ctable = Repa.map $ \x -> ctable Repa.! (Z:. fromIntegral x)

main = do
  let nsites = 150
  let ctable = genColorTable nsites
  voro <- computeP $ colorize ctable (voronoi nsites 512) :: IO (Array U DIM2 (Word8, Word8, Word8))
  writeImageToBMP "out.bmp" voro
