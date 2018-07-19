data BalancedTernary = Bt [Int]

zeroTrim a = if null s then [0] else s where
	s = f [] [] a
	f x _ [] = x
	f x y (0:zs) = f x (y++[0]) zs
	f x y (z:zs) = f (x++y++[z]) [] zs

btList (Bt a) = a

instance Eq BalancedTernary where
	(==) a b = btList a == btList b

btNormalize = listBt . _carry 0 where
	_carry c [] = if c == 0 then [] else [c]
	_carry c (a:as) = r:_carry cc as where
		(cc, r) = f $ (a+c) `quotRem` 3 where
			f (x,  2) = (x + 1, -1)
			f (x, -2) = (x - 1,  1)
			f x = x

listBt = Bt . zeroTrim

instance Show BalancedTernary where
	show = reverse . map (\d->case d of -1->'-'; 0->'0'; 1->'+') . btList

strBt = Bt . zeroTrim.reverse.map (\c -> case c of '-' -> -1; '0' -> 0; '+' -> 1)

intBt :: Integral a => a -> BalancedTernary
intBt = fromIntegral . toInteger

btInt = foldr (\a z -> a + 3 * z) 0 . btList

listAdd a b = take (max (length a) (length b)) $ zipWith (+) (a++[0,0..]) (b++[0,0..])

-- mostly for operators, also small stuff to make GHC happy
instance Num BalancedTernary where
	negate = Bt . map negate . btList
	(+) x y = btNormalize $ listAdd (btList x) (btList y)
	(*) x y = btNormalize $ mul_ (btList x) (btList y) where
		mul_ _ [] = []
                mul_ as b = foldr (\a z -> listAdd (map (a*) b) (0:z)) [] as

	-- we don't need to define binary "-" by hand

	signum (Bt a) = if a == [0] then 0 else Bt [last a]
	abs x = if signum x == Bt [-1] then negate x else x

	fromInteger = btNormalize . f where
		f 0 = []
		f x = fromInteger (rem x 3) : f (quot x 3)


main = let	(a,b,c) = (strBt "+-0++0+", intBt (-436), strBt "+-++-")
		r = a * (b - c)
	in do
		print $ map btInt [a,b,c]
		print $ r
		print $ btInt r
