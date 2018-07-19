nonsqr :: Integral a => a -> a
nonsqr n = n + round (sqrt (fromIntegral n))
