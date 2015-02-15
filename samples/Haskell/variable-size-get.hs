import Foreign

sizeOf (undefined :: Int) -- size of Int in bytes (4 on mine)
sizeOf (undefined :: Double) -- size of Double in bytes (8 on mine)
sizeOf (undefined :: Bool) -- size of Bool in bytes (4 on mine)
sizeOf (undefined :: Ptr a) -- size of Ptr in bytes (4 on mine)
