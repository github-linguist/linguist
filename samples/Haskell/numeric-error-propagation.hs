data Error a = Error {value :: a, uncertainty :: a} deriving (Eq, Show)

instance (Floating a) => Num (Error a) where
	Error a ua + Error b ub = Error (a + b) (sqrt (ua ^ 2 + ub ^ 2))
	negate (Error a ua) = Error (negate a) ua
	Error a ua * Error b ub = Error (a * b) (abs (a * b * sqrt ((ua / a) ^ 2 + (ub / b) ^ 2))) -- I've factored out the f^2 from the square root
	fromInteger a = Error (fromInteger a) 0

instance (Floating a) => Fractional (Error a) where
	fromRational a = Error (fromRational a) 0
	Error a ua / Error b ub = Error (a / b) (abs (a / b * sqrt ((ua / a) ^ 2 + (ub / b) ^ 2))) -- I've factored out the f^2 from the square root

instance (Floating a) => Floating (Error a) where
	Error a ua ** Error c 0 = Error (a ** c) (abs (ua * c * a**c / a))

main = print (sqrt ((x1 - x2) ** 2 + (y1 - y2) ** 2)) where -- using (^) for exponentiation would calculate a*a, which the problem specifically said was calculated wrong
	x1 = Error 100 1.1
	y1 = Error 50 1.2
	x2 = Error 200 2.2
	y2 = Error 100 2.3
