gcd :: (Integral a) => a -> a -> a
gcd 0 0 =  error "Prelude.gcd: gcd 0 0 is undefined"
gcd x y =  gcd' (abs x) (abs y) where
  gcd' a 0  =  a
  gcd' a b  =  gcd' b (a `rem` b)
