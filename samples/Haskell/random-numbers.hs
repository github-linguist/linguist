import System.Random

pairs :: [a] -> [(a,a)]
pairs (x:y:zs) = (x,y):pairs zs
pairs _        = []

gauss mu sigma (r1,r2) =
  mu + sigma * sqrt (-2 * log r1) * cos (2 * pi * r2)

gaussians :: (RandomGen g, Random a, Floating a) => Int -> g -> [a]
gaussians n g = take n $ map (gauss 1.0 0.5) $ pairs $ randoms g

result :: IO [Double]
result = getStdGen >>= \g -> return $ gaussians 1000 g
