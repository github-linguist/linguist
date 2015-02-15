import qualified Data.Map as M
import System.Random
import Data.List
import Control.Monad
import System.Environment

testFile = [1..10]

selItem g xs = foldl' f (head xs, 1, 2, g) $ tail xs
    where f :: RandomGen a => (b, Int, Int, a) -> b -> (b, Int, Int, a)
          f (c, cn, n, gen) l | v == 1    = (l, n, n+1, ngen)
                              | otherwise = (c, cn, n+1, ngen)
            where (v, ngen) = randomR (1, n) gen

oneOfN a = do
            g <- newStdGen
            let (r, _, _, _) = selItem g a
            return r

test = do
        x <- replicateM 1000000 (oneOfN testFile)
        let f m l = M.insertWith (+) l 1 m
        let results = foldl' f M.empty x
        forM_ (M.toList results) $ \(x, y) -> putStrLn $ "Line number " ++ show x ++
								 " had count :" ++ show y

main = do
        a <- getArgs
        g <- newStdGen
        if null a then test
                  else putStrLn.(\(l, n, _, _) -> "Line " ++
                               show n ++ ": " ++ l)
                       .selItem g.lines =<< (readFile $ head a)
