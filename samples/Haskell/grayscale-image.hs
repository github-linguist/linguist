module Bitmap.Gray(module Bitmap.Gray) where

import Bitmap
import Control.Monad.ST

newtype Gray = Gray Int deriving (Eq, Ord)

instance Color Gray where
    luminance (Gray x) = x
    black = Gray 0
    white = Gray 255
    toNetpbm = map $ toEnum . luminance
    fromNetpbm = map $ Gray . fromEnum
    netpbmMagicNumber _ = "P5"
    netpbmMaxval _ = "255"

toGrayImage :: Color c => Image s c -> ST s (Image s Gray)
toGrayImage = mapImage $ Gray . luminance
