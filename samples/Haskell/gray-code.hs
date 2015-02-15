import Data.Bits
import Data.Char
import Numeric
import Text.Printf

-- Conversion to and from traditional binary and Gray code
grayToBin :: (Integral t, Bits t) => t -> t
grayToBin 0 = 0
grayToBin g = g `xor` (grayToBin $ g `shiftR` 1)

binToGray :: (Integral t, Bits t) => t -> t
binToGray b = b `xor` (b `shiftR` 1)

-- Print the first 32 Gray codes alongside their decimal and binary equivalences.
main = flip mapM_ (take 32 [0,1..] :: [Int]) (\num -> do
	let bin  = showIntAtBase 2 intToDigit num ""
	    gray = showIntAtBase 2 intToDigit (binToGray num) ""
	printf "int: %2d -> bin: %5s -> gray: %5s\n" num bin gray)
