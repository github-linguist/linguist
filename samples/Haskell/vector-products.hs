type Vector a = [a]
type Scalar a = a

a,b,c,d :: Vector Int
a = [ 3,  4,  5 ]
b = [ 4,  3,  5 ]
c = [-5,-12,-13 ]
d = [ 3,  4,  5,  6 ]

dot :: (Num t) => Vector t -> Vector t -> Scalar t
dot u v | length u == length v = sum $ zipWith (*) u v
        | otherwise = error "Dotted Vectors must be of equal dimension."

cross :: (Num t) => Vector t -> Vector t -> Vector t
cross u v | length u == 3 && length v == 3 =
             [u !! 1 * v !! 2 - u !! 2 * v !! 1,
              u !! 2 * v !! 0 - u !! 0 * v !! 2,
              u !! 0 * v !! 1 - u !! 1 * v !! 0]
          | otherwise = error "Crossed Vectors must both be three dimensional."

scalarTriple :: (Num t) => Vector t -> Vector t -> Vector t -> Scalar t
scalarTriple q r s = dot q $ cross r s

vectorTriple :: (Num t) => Vector t -> Vector t -> Vector t -> Vector t
vectorTriple q r s = cross q $ cross r s

main = do
   mapM_ putStrLn [ "a . b     = " ++ (show $ dot a b)
                  , "a x b     = " ++ (show $ cross a b)
                  , "a . b x c = " ++ (show $ scalarTriple a b c)
                  , "a x b x c = " ++ (show $ vectorTriple a b c)
                  , "a . d     = " ++ (show $ dot a d) ]
