#!/usr/bin/runhaskell

import Network.HTTP.Conduit
import qualified Data.ByteString.Lazy as L
import Network (withSocketsDo)

main = withSocketsDo
     $ simpleHttp "https://sourceforge.net/" >>= L.putStr
