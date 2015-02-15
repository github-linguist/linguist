#!/usr/bin/runhaskell

import System.Entropy
import Data.Binary.Get
import qualified Data.ByteString.Lazy as B

main = do
  bytes <- getEntropy 4
  print (runGet getWord32be $ B.fromChunks [bytes])
