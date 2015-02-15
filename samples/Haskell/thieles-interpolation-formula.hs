thiele xs ys = f rho1 (tail xs) where
	f _ [] _ = 1
	f r@(r0:r1:r2:rs) (x:xs) v = r2 - r0 + (v-x) / f (tail r) xs v

	rho1 = (map ((!!1).(++[0])) rho)

	rho = [0,0..] : [0,0..] : ys : rnext (tail rho) xs (tail xs) where
		rnext _ _ [] = []
		rnext r@(r0:r1:rs) x xn = let z_ = zipWith in
			(z_ (+)	(tail r0)
				(z_ (/)	(z_ (-) x xn)
				    	(z_ (-) r1 (tail r1))))
			: rnext (tail r) x (tail xn)

-- inverted interpolation function of f
inv_interp f xs = thiele (map f xs) xs

main = do	print $ 3.21 * inv_sin (sin (pi / 3.21))
		print $ pi/1.2345 * inv_cos (cos (1.2345))
		print $ 7 * inv_tan (tan (pi / 7))
	where
		inv_sin = inv_interp sin $ div_pi 2 31
		inv_cos = inv_interp cos $ div_pi 2 100
		inv_tan = inv_interp tan $ div_pi 4 1000 -- because we can
		-- uniformly take n points from 0 to Pi/d
		div_pi d n = map (* (pi / (d * n))) [0..n]
