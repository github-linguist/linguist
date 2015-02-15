{-# LANGUAGE ScopedTypeVariables #-}

module Bitmap.Netpbm(readNetpbm, writeNetpbm) where

import Bitmap
import Data.Char
import System.IO
import Control.Monad
import Control.Monad.ST
import Data.Array.ST

nil :: a
nil = undefined

readNetpbm :: forall c. Color c => FilePath -> IO (Image RealWorld c)
readNetpbm path = do
    let die = fail "readNetpbm: bad format"
    ppm <- readFile path
    let (s, rest) = splitAt 2 ppm
    unless (s == magicNumber) die
    let getNum :: String -> IO (Int, String)
        getNum ppm = do
            let (s, rest) = span isDigit $ skipBlanks ppm
            when (null s) die
            return (read s, rest)
    (width, rest) <- getNum rest
    (height, rest) <- getNum rest
    (_, c : rest) <-
        if getMaxval then getNum rest else return (nil, rest)
    unless (isSpace c) die
    i <- stToIO $ listImage width height $
        fromNetpbm $ map fromEnum rest
    return i
  where skipBlanks =
           dropWhile isSpace .
           until ((/= '#') . head) (tail . dropWhile (/= '\n')) .
           dropWhile isSpace
        magicNumber = netpbmMagicNumber (nil :: c)
        getMaxval = not $ null $ netpbmMaxval (nil :: c)

writeNetpbm :: forall c. Color c => FilePath -> Image RealWorld c -> IO ()
writeNetpbm path i = withFile path WriteMode $ \h -> do
    (width, height) <- stToIO $ dimensions i
    let w = hPutStrLn h
    w $ magicNumber
    w $ show width ++ " " ++ show height
    unless (null maxval) (w maxval)
    stToIO (getPixels i) >>= hPutStr h . toNetpbm
  where magicNumber = netpbmMagicNumber (nil :: c)
        maxval = netpbmMaxval (nil :: c)
