import Control.Monad (replicateM)

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.Binary.Get
import Data.Binary.Put
import Data.Bits

import Data.Array (Array, listArray, (!))
import Data.List (foldl)
import Data.Word (Word32)

import Numeric (showHex)


-- functions
type Fun = Word32 -> Word32 -> Word32 -> Word32

funF, funG, funH, funI :: Fun
funF x y z = (x .&. y) .|. (complement x .&. z)
funG x y z = (x .&. z) .|. (complement z .&. y)
funH x y z = x `xor` y `xor` z
funI x y z = y `xor` (complement z .|. x)

idxF, idxG, idxH, idxI :: Int -> Int
idxF i = i
idxG i = (5 * i + 1) `mod` 16
idxH i = (3 * i + 5) `mod` 16
idxI i = 7 * i `mod` 16


-- arrays
funA :: Array Int Fun
funA = listArray (1,64) $ replicate 16 =<< [funF, funG, funH, funI]

idxA :: Array Int Int
idxA = listArray (1,64) $ zipWith ($) (replicate 16 =<< [idxF, idxG, idxH, idxI]) [0..63]

rotA :: Array Int Int
rotA = listArray (1,64) $ concat . replicate 4 =<<
       [[7, 12, 17, 22], [5, 9, 14, 20], [4, 11, 16, 23], [6, 10, 15, 21]]

sinA :: Array Int Word32
sinA = listArray (1,64) $ map (floor . (*mult) . abs . sin) [1..64]
    where mult = 2 ** 32 :: Double


-- to lazily calculate MD5 sum for standart input:
-- main = putStrLn . md5sum =<< BL.getContents

main :: IO ()
main = mapM_ (putStrLn . md5sum . BLC.pack)
        [ ""
        , "a"
        , "abc"
        , "message digest"
        , "abcdefghijklmnopqrstuvwxyz"
        , "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
        , "12345678901234567890123456789012345678901234567890123456789012345678901234567890"
        ]


md5sum :: BL.ByteString -> String
md5sum input =
    let MD5 a b c d = getMD5 initial `runGet` input
    in  foldr hex [] . BL.unpack . runPut $ mapM_ putWord32le [a,b,c,d]
    where
      initial = MD5 0x67452301 0xEFCDAB89 0x98BADCFE 0x10325476

      hex x s | x < 16    = '0' : showHex x s -- quick hack: like "%02x"
              | otherwise =       showHex x s


data MD5 = MD5
    { a :: {-# UNPACK #-} !Word32
    , b :: {-# UNPACK #-} !Word32
    , c :: {-# UNPACK #-} !Word32
    , d :: {-# UNPACK #-} !Word32
    }


getMD5 :: MD5 -> Get MD5
getMD5 md5 = do
  chunk <- getLazyByteString 64
  let len = BL.length chunk

  if len == 64
  then getMD5 $! md5 <+> chunk  -- apply and process next chunk

  else do                       -- input is totally eaten, finalize
    bytes <- bytesRead
    let fin   = runPut . putWord64le $ fromIntegral (bytes - 64 + len) * 8
        pad n = chunk `BL.append` (0x80 `BL.cons` BL.replicate (n - 1) 0x00)

    return $ if len >= 56
        then md5 <+> pad (64 - len) <+> BL.replicate 56 0x00 `BL.append` fin
        else md5 <+> pad (56 - len) `BL.append` fin


(<+>) :: MD5 -> BL.ByteString -> MD5
infixl 5  <+>
md5@(MD5 a b c d) <+> bs =
    let datA = listArray (0,15) $ replicateM 16 getWord32le `runGet` bs
        MD5 a' b' c' d' = foldl' (md5round datA) md5 [1..64]
    in MD5 (a + a') (b + b') (c + c') (d + d')


md5round :: Array Int Word32 -> MD5 -> Int -> MD5
md5round datA (MD5 a b c d) i =
    let f  =  funA ! i
        w  =  datA ! (idxA ! i)
        a' =  b + (a + f b c d + w + sinA ! i) `rotateL` rotA ! i
    in MD5 d a' b c
