import Prelude hiding (Bool(..), not, (&&), (||), (==))

main = mapM_ (putStrLn . unlines . map unwords)
    [ table "not"     $ unary not
    , table "and"     $ binary (&&)
    , table "or"      $ binary (||)
    , table "implies" $ binary (=->)
    , table "equals"  $ binary (==)
    ]

data Trit = False | Maybe | True deriving (Show)

False `nand` _     = True
_     `nand` False = True
True  `nand` True  = False
_     `nand` _     = Maybe

not a = nand a a

a && b = not $ a `nand` b

a || b = not a `nand` not b

a =-> b = a `nand` not b

a == b = (a && b) || (not a && not b)

inputs1 = [True, Maybe, False]
inputs2 = [(a,b) | a <- inputs1, b <- inputs1]

unary f = map (\a -> [a, f a]) inputs1
binary f = map (\(a,b) -> [a, b, f a b]) inputs2

table name xs = map (map pad) . (header :) $ map (map show) xs
    where header = map (:[]) (take ((length $ head xs) - 1) ['A'..]) ++ [name]

pad s = s ++ replicate (5 - length s) ' '
