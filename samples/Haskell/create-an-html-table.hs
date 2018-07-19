#!/usr/bin/runhaskell

import Control.Monad (forM_)
import System.Random
import Data.List as L

import Text.Blaze.Html5
import Text.Blaze.Html.Renderer.Pretty

makeTable :: RandomGen g => [String] -> Int -> g -> Html
makeTable headings nRows gen =
  table $ do
    thead $ tr $ forM_ (L.map toHtml headings) th
    tbody $ forM_ (zip [1 .. nRows] $ unfoldr (Just . split) gen)
      (\(x,g) -> tr $ forM_ (take (length headings)
                                  (x:randomRs (1000,9999) g)) (td . toHtml))

main = do
  g <- getStdGen
  putStrLn $ renderHtml $ makeTable ["", "X", "Y", "Z"] 3 g
