import Data.Char
import Text.Printf

-- Perform encryption or decryption, depending on f.
crypt f key = map toLetter . zipWith f (cycle key)
  where toLetter = chr . (+) (ord 'A')

-- Encrypt or decrypt one letter.
enc k c = (ord k + ord c) `mod` 26
dec k c = (ord c - ord k) `mod` 26

-- Given a key, encrypt or decrypt an input string.
encrypt = crypt enc
decrypt = crypt dec

-- Convert a string to have only upper case letters.
convert = map toUpper . filter isLetter

main = do
  let key  = "VIGENERECIPHER"
      text = "Beware the Jabberwock, my son! The jaws that bite, "
             ++ "the claws that catch!"
      encr = encrypt key $ convert text
      decr = decrypt key encr
  printf "    Input: %s\n      Key: %s\nEncrypted: %s\nDecrypted: %s\n"
    text key encr decr
