newtype Series a = S { coeffs :: [a] } deriving (Eq, Show)
-- Invariant: coeffs must be an infinite list

instance Num a => Num (Series a) where
  fromInteger n = S $ fromInteger n : repeat 0
  negate (S fs) = S $ map negate fs
  S fs + S gs   = S $ zipWith (+) fs gs
  S (f:ft) * S gs@(g:gt) = S $ f*g : coeffs (S ft * S gs + S (map (f*) gt))

instance Fractional a => Fractional (Series a) where
  fromRational n = S $ fromRational n : repeat 0
  S (f:ft) / S (g:gt) = S qs where qs = f/g : map (/g) (coeffs (S ft - S qs * S gt))

-- utility function to convert from a finite polynomial
fromFiniteList xs = S (xs ++ repeat 0)

int (S fs) = S $ 0 : zipWith (/) fs [1..]

diff (S (_:ft)) = S $ zipWith (*) ft [1..]

sinx,cosx :: Series Rational
sinx = int cosx
cosx = 1 - int sinx

fiboS = 1 / fromFiniteList [1,-1,-1]
