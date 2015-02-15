import qualified Data.Set as S

consolidate :: Ord a => [S.Set a] -> [S.Set a]
consolidate = foldl comb []
  where comb [] s' = [s']
        comb (s:ss) s'
          | S.null (s `S.intersection` s') = s : comb ss s'
          | otherwise = comb ss (s `S.union` s')
