import Control.Monad
import Data.Char (isAlpha, toLower)
import qualified Data.Map as M
import qualified Data.IntSet as S
import System.Environment (getArgs)

main =  do
    (files, _ : q) <- liftM (break (== "--")) getArgs
    buildII files >>= mapM_ putStrLn . queryII q

data IIndex = IIndex
    [FilePath]              -- Files in the index
    (M.Map String S.IntSet) -- Maps word to indices of the list
  deriving Show

buildII :: [FilePath] -> IO IIndex
buildII files =
    liftM (IIndex files . foldl f M.empty . zip [0..]) $
    mapM readFile files
  where f m (i, s) =
            foldl g m $ map (lowercase . filter isAlpha) $ words s
          where g m word = M.insertWith S.union word (S.singleton i) m

queryII :: [String] -> IIndex -> [FilePath]
queryII q (IIndex files m) =
    map (files !!) $ S.toList $ intersections $
    map (\word -> M.findWithDefault S.empty (lowercase word) m) q

intersections [] = S.empty
intersections xs = foldl1 S.intersection xs

lowercase = map toLower
