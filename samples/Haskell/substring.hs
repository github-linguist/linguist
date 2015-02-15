t45 n c s | null sub = []
          | otherwise = take n. head $ sub
  where sub = filter(isPrefixOf c) $ tails s
