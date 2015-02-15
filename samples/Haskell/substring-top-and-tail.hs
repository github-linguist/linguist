-- We define the functions to return an empty string if the argument is too
-- short for the particular operation.

remFirst, remLast, remBoth :: String -> String

remFirst "" = ""
remFirst cs = tail cs

remLast "" = ""
remLast cs = init cs

remBoth (c:cs) = remLast cs
remBoth  _     = ""

main :: IO ()
main = do
  let s = "Some string."
  mapM_ (\f -> putStrLn . f $ s) [remFirst, remLast, remBoth]
