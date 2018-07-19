lcm :: (Integral a) => a -> a -> a
lcm _ 0 =  0
lcm 0 _ =  0
lcm x y =  abs ((x `quot` (gcd x y)) * y)
