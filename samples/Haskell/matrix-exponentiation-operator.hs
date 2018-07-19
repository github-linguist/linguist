import Data.List

xs <+> ys = zipWith (+) xs ys
xs <*> ys = sum $ zipWith (*) xs ys

newtype Mat a = Mat [[a]] deriving (Eq, Show)

instance Num a => Num (Mat a) where
  negate (Mat x) = Mat $ map (map negate) x
  Mat x + Mat y   = Mat $ zipWith (<+>) x y
  Mat x * Mat y   = Mat [[xs <*> ys | ys <- transpose y] | xs <- x]
  fromInteger _ = undefined -- don't know dimension of the desired matrix
