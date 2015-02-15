import Data.List.Ordered

powers m = map (^ m) [0..]

squares = powers 2
cubes = powers 3
foo = filter (not . has cubes) squares

main :: IO ()
main = print $ take 10 $ drop 20 $ foo
