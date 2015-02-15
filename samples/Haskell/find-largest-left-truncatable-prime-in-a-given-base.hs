primesTo100 = [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97]

-- (eq. to) find2km (2^k * n) = (k,n)
find2km :: Integral a => a -> (Int,a)
find2km n = f 0 n
	where f k m
            | r == 1 = (k,m)
            | otherwise = f (k+1) q
            where (q,r) = quotRem m 2

-- n is the number to test; a is the (presumably randomly chosen) witness
millerRabinPrimality :: Integer -> Integer -> Bool
millerRabinPrimality n a
    | a >= n_ = True
    | b0 == 1 || b0 == n_ = True
    | otherwise = iter (tail b)
    where
        n_ = n-1
        (k,m) = find2km n_
        b0 = powMod n a m
        b = take k $ iterate (squareMod n) b0
        iter [] = False
        iter (x:xs)
            | x == 1 = False
            | x == n_ = True
            | otherwise = iter xs

-- (eq. to) pow_ (*) (^2) n k = n^k
pow_ :: (Num a, Integral b) => (a->a->a) -> (a->a) -> a -> b -> a
pow_ _ _ _ 0 = 1
pow_ mul sq x_ n_ = f x_ n_ 1
    where
        f x n y
            | n == 1 = x `mul` y
            | r == 0 = f x2 q y
            | otherwise = f x2 q (x `mul` y)
            where
                (q,r) = quotRem n 2
                x2 = sq x

mulMod :: Integral a => a -> a -> a -> a
mulMod a b c = (b * c) `mod` a
squareMod :: Integral a => a -> a -> a
squareMod a b = (b * b) `rem` a

-- (eq. to) powMod m n k = n^k `mod` m
powMod :: Integral a => a -> a -> a -> a
powMod m = pow_ (mulMod m) (squareMod m)

-- Caller supplies a witness list w, which may be used for MR test.
-- Use faster trial division against a small primes list first, to
-- weed out more obvious composites.
is_prime w n
	| n < 100 = n `elem` primesTo100
	| any ((==0).(n`mod`)) primesTo100 = False
	| otherwise = all (millerRabinPrimality n) w

-- final result gets a more thorough Miller-Rabin
left_trunc base = head $ filter (is_prime primesTo100) (reverse hopeful) where
	hopeful = extend base $ takeWhile (<base) primesTo100 where
	extend b x = if null d then x else extend (b*base) d where
		d = concatMap addDigit [1..base-1]
		-- we do *one* prime test, which seems good enough in practice
		addDigit a = filter (is_prime [3]) $ map (a*b+) x

main = mapM_ print $ map (\x->(x, left_trunc x)) [3..21]
