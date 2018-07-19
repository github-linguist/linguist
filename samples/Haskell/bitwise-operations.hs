import Data.Bits

bitwise :: Int -> Int -> IO ()
bitwise a b = do
  print $ a .&. b
  print $ a .|. b
  print $ a `xor` b
  print $ complement a
  print $ shiftL a b -- left shift
  print $ shiftR a b -- arithmetic right shift
  print $ shift a b  -- You can also use the "unified" shift function; positive is for left shift, negative is for right shift
  print $ shift a (-b)
  print $ rotateL a b -- rotate left
  print $ rotateR a b -- rotate right
  print $ rotate a b  -- You can also use the "unified" rotate function; positive is for left rotate, negative is for right rotate
  print $ rotate a (-b)

main = bitwise 255 170
