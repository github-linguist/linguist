import Text.Printf

euler :: (Num a, Ord a) => (a -> a -> a) -> a -> a -> a -> a -> [(a,a)]
euler f y0 a b h =
	(a, y0) :
	if a < b
		then euler f (y0 + (f a y0) * h) (a + h) b h
		else []

newtonCooling :: Double -> Double -> Double
newtonCooling _ t = -0.07 * (t - 20)

main = do
  mapM_ (uncurry $ printf "%6.3f %6.3f\n") $ euler newtonCooling 100 0 100 10
  putStrLn "DONE"
