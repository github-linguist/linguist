--------------------------------------------------------------------------------
--- The standard prelude of Curry with type classes.
--- All exported functions, data types, type classes
--- and methods defined in this module are always
--- available in any Curry program.
--------------------------------------------------------------------------------
{-# LANGUAGE CPP #-}
{-# OPTIONS_FRONTEND -Wno-incomplete-patterns -Wno-overlapping #-}

module Prelude
  (
  -- * Basic Datatypes
    Char (..), Int (..), Float (..)
--++   , () (..), (,) (..), (,,) (..), (,,,) (..), (,,,,) (..)
--++   , [] (..), (->) (..)
  , Bool (..), Ordering (..), Maybe (..), Either (..)

  -- * Type Classes
  , Data(..), Eq (..) , Ord (..)
  , Show (..), ShowS, shows, showChar, showString, showParen
  , Read (..), ReadS, reads, readParen, read, lex
  , Bounded (..), Enum (..)
  -- ** Numerical Typeclasses
  , Num (..), Fractional (..), Real (..)
  , Integral (..), even, odd, fromIntegral, realToFrac, (^)
  , RealFrac (..), Floating (..), Monoid (..)
  -- Type Constructor Classes
  , Functor (..), Applicative (..), Alternative (..)
  , Monad (..), MonadFail(..)
  , liftM2, sequence, sequence_, mapM, mapM_

  -- * Operations on Characters
  , isUpper, isLower, isAlpha, isDigit, isAlphaNum
  , isBinDigit, isOctDigit, isHexDigit, isSpace
  , ord, chr
  , String, lines, unlines, words, unwords

  -- * Operations on Lists
  , head, tail, null, (++), length, (!!), map, foldl, foldl1, foldr, foldr1
  , filter, zip, zip3, zipWith, zipWith3, unzip, unzip3, concat, concatMap
  , iterate, repeat, replicate, take, drop, splitAt, takeWhile, dropWhile
  , span, break, reverse, and, or, any, all, elem, notElem, lookup, (<$>)

  -- * Evaluation
  , ($), ($!), ($!!), ($#), ($##), seq, ensureNotFree, ensureSpine
  , normalForm, groundNormalForm

  -- * Other Functions
  , (.), id, const, asTypeOf, curry, uncurry, flip, until
  , (&&), (||), not, otherwise, ifThenElse, maybe, either, fst, snd
  , failed, error

  -- * IO-Type and Operations
  , IO, getChar, getLine, putChar, putStr, putStrLn, print
  , FilePath, readFile, writeFile, appendFile
  , IOError (..), userError, ioError, catch

  -- * Constraint Programming
  , Success, success, solve, doSolve, (=:=), (=:<=)
#ifdef __PAKCS__
  , (=:<<=)
#endif
  , (&), (&>)

  -- * Non-determinism
  , (?), anyOf, unknown

  -- * Internal Functions
  , apply, cond
#ifdef __PAKCS__
  , letrec, failure
#endif
  ) where

infixr 9 .
infixl 9 !!
infixl 7 *, /, `div`, `mod`, `quot`, `rem`
infixl 6 +, -
infixr 5 ++
--++ The (:) operator is built-in syntax with the following fixity:
--++ infixr 5 :
infix  4 ==, /=, <, >, <=, >=
infix  4 =:=, =:<=, ===
#ifdef __PAKCS__
infix  4 =:<<=
#endif
infix  4 `elem`, `notElem`
infixl 4 <$, <$>, <*>, <*, *>
infixl 3 <|>
infixr 3 &&
infixr 2 ||
infixl 1 >>, >>=
infixr 0 ?, $, $!, $!!, $#, $##, `seq`, &, &>

external data Char

external data Int

external data Float

data Bool = False | True

data Ordering = LT | EQ | GT

------------------------------------------------------------------------------
--++ data () = ()

--++ data (a, b) = (a, b)
--++ data (a, b, c) = (a, b, c)
--++ data (a, b, c, d) = (a, b, c, d)
--++ data (a, b, c, d, e) = (a, b, c, d, e)
--++ ...

--++ data [a] = [] | a : [a]

--++ data (->) a b

class Data a where
  (===)  :: a -> a -> Bool
  aValue :: a

instance Data Char where
  (===) = (==)
  aValue = aValueChar

instance Data Int where
  (===) = (==)
  aValue = aValueInt

instance Data Float where
  (===) = (==)
  aValue = aValueFloat

instance Data a => Data [a] where
  []     === []     = True
  []     === (_:_)  = False
  (_:_)  === []     = False
  (x:xs) === (y:ys) = x === y && xs === ys

  aValue = [] ? (aValue:aValue)

instance Data () where
  () === () = True
  aValue = ()

instance (Data a, Data b) => Data (a, b) where
  (a1, b1) === (a2, b2) = a1 === a2 && b1 === b2
  aValue = (aValue, aValue)

instance (Data a, Data b, Data c) => Data (a, b, c) where
  (a1, b1, c1) === (a2, b2, c2) = a1 === a2 && b1 === b2 && c1 === c2
  aValue = (aValue, aValue, aValue)

instance (Data a, Data b, Data c, Data d) => Data (a, b, c, d) where
  (a1, b1, c1, d1) === (a2, b2, c2, d2) =
    a1 === a2 && b1 === b2 && c1 === c2 && d1 === d2
  aValue = (aValue, aValue, aValue, aValue)

instance (Data a, Data b, Data c, Data d, Data e) => Data (a, b, c, d, e) where
  (a1, b1, c1, d1, e1) === (a2, b2, c2, d2, e2) =
    a1 === a2 && b1 === b2 && c1 === c2 && d1 === d2 && e1 === e2
  aValue = (aValue, aValue, aValue, aValue, aValue)

instance (Data a, Data b, Data c, Data d, Data e, Data f) =>
         Data (a, b, c, d, e, f) where
  (a1, b1, c1, d1, e1, f1) === (a2, b2, c2, d2, e2, f2) =
    a1 === a2 && b1 === b2 && c1 === c2 && d1 === d2 && e1 === e2 && f1 === f2
  aValue = (aValue, aValue, aValue, aValue, aValue, aValue)

instance (Data a, Data b, Data c, Data d, Data e, Data f, Data g) =>
         Data (a, b, c, d, e, f, g) where
  (a1, b1, c1, d1, e1, f1, g1) === (a2, b2, c2, d2, e2, f2, g2) =
    a1 === a2 && b1 === b2 && c1 === c2 && d1 === d2 && e1 === e2 &&
    f1 === f2 && g1 === g2
  aValue = (aValue, aValue, aValue, aValue, aValue, aValue, aValue)

-- Value generator for integers.
aValueInt :: Int
aValueInt = genPos 1 ? 0  ?  0 - genPos 1
 where
  genPos n = n  ?  genPos (2 * n)  ?  genPos (2 * n + 1)

-- Value generator for chars.
aValueChar :: Char
aValueChar = foldr1 (?) [minBound .. maxBound]

-- Value generator for floats.
-- Since there is no good way to enumerate floats, a free variable
-- is returned.
aValueFloat :: Float
aValueFloat = x where x free

------------------------------------------------------------------------------

class Eq a where
  (==), (/=) :: a -> a -> Bool

  x == y = not (x /= y)
  x /= y = not (x == y)

instance Eq Char where
  c == c' = c `eqChar` c'

instance Eq Int where
  i == i' = i `eqInt` i'

instance Eq Float where
  f == f' = f `eqFloat` f'

instance Eq () where
  () == () = True

instance (Eq a, Eq b) => Eq (a, b) where
  (a, b) == (a', b') = a == a' && b == b'

instance (Eq a, Eq b, Eq c) => Eq (a, b, c) where
  (a, b, c) == (a', b', c') = a == a' && b == b' && c == c'

instance (Eq a, Eq b, Eq c, Eq d) => Eq (a, b, c, d) where
  (a, b, c, d) == (a', b', c', d') = a == a' && b == b' && c == c' && d == d'

instance (Eq a, Eq b, Eq c, Eq d, Eq e) => Eq (a, b, c, d, e) where
  (a, b, c, d, e) == (a', b', c', d', e') =
    a == a' && b == b' && c == c' && d == d' && e == e'

instance Eq a => Eq [a] where
  []     == []     = True
  []     == (_:_)  = False
  (_:_)  == []     = False
  (x:xs) == (y:ys) = x == y && xs == ys

instance Eq Bool where
  False == False = True
  False == True  = False
  True  == False = False
  True  == True  = True

instance Eq Ordering where
  LT == LT = True
  LT == EQ = False
  LT == GT = False
  EQ == LT = False
  EQ == EQ = True
  EQ == GT = False
  GT == LT = False
  GT == EQ = False
  GT == GT = True

-- Equality on characters.
eqChar :: Char -> Char -> Bool
#ifdef __KICS2__
eqChar external
#elif defined(__PAKCS__)
eqChar x y = (prim_eqChar $# y) $# x

prim_eqChar :: Char -> Char -> Bool
prim_eqChar external
#endif

-- Equality on integers.
eqInt :: Int -> Int -> Bool
#ifdef __KICS2__
eqInt external
#elif defined(__PAKCS__)
eqInt x y = (prim_eqInt $# y) $# x

prim_eqInt :: Int -> Int -> Bool
prim_eqInt external
#endif

-- Equality on floating point numbers.
eqFloat :: Float -> Float -> Bool
#ifdef __KICS2__
eqFloat external
#elif defined(__PAKCS__)
eqFloat x y = (prim_eqFloat $# y) $# x

prim_eqFloat :: Float -> Float -> Bool
prim_eqFloat external
#endif


class Eq a => Ord a where
  compare :: a -> a -> Ordering
  (<), (>), (<=), (>=) :: a -> a -> Bool
  min, max :: a -> a -> a

  compare x y | x == y = EQ
              | x <= y = LT
              | otherwise = GT
  x < y = x <= y && x /= y
  x > y = not (x <= y)
  x <= y = compare x y == EQ || compare x y == LT
  x >= y = y <= x
  min x y | x <= y = x
          | otherwise = y
  max x y | x >= y = x
          | otherwise = y

instance Ord Char where
  c1 <= c2 = c1 `ltEqChar` c2

instance Ord Int where
  i1 <= i2 = i1 `ltEqInt` i2

instance Ord Float where
  f1 <= f2 = f1 `ltEqFloat` f2

instance Ord () where
  () <= () = True

instance (Ord a, Ord b) => Ord (a, b) where
  (a, b) <= (a', b') = a < a' || (a == a' && b <= b')

instance (Ord a, Ord b, Ord c) => Ord (a, b, c) where
  (a, b, c) <= (a', b', c') = a < a'
    || (a == a' && b < b')
    || (a == a' && b == b' && c <= c')

instance (Ord a, Ord b, Ord c, Ord d) => Ord (a, b, c, d) where
  (a, b, c, d) <= (a', b', c', d') = a < a'
    || (a == a' && b < b')
    || (a == a' && b == b' && c < c')
    || (a == a' && b == b' && c == c' && d <= d')

instance (Ord a, Ord b, Ord c, Ord d, Ord e) => Ord (a, b, c, d, e) where
  (a, b, c, d, e) <= (a', b', c', d', e') = a < a'
    || (a == a' && b < b')
    || (a == a' && b == b' && c < c')
    || (a == a' && b == b' && c == c' && d < d')
    || (a == a' && b == b' && c == c' && d == d' && e <= e')

instance Ord a => Ord [a] where
  []     <= []     = True
  (_:_)  <= []     = False
  []     <= (_:_)  = True
  (x:xs) <= (y:ys) | x == y    = xs <= ys
                   | otherwise = x < y

instance Ord Bool where
  False <= False = True
  False <= True  = True
  True  <= False = False
  True  <= True  = True

instance Ord Ordering where
  LT <= LT = True
  LT <= EQ = True
  LT <= GT = True
  EQ <= LT = False
  EQ <= EQ = True
  EQ <= GT = True
  GT <= LT = False
  GT <= EQ = False
  GT <= GT = True

-- Compares two characters.
ltEqChar :: Char -> Char -> Bool
#ifdef __KICS2__
ltEqChar external
#elif defined(__PAKCS__)
ltEqChar x y = (prim_ltEqChar $# y) $# x

prim_ltEqChar :: Char -> Char -> Bool
prim_ltEqChar external
#endif

-- Compares two integers.
ltEqInt :: Int -> Int -> Bool
#ifdef __KICS2__
ltEqInt external
#elif defined(__PAKCS__)
ltEqInt x y = (prim_ltEqInt $# y) $# x

prim_ltEqInt :: Int -> Int -> Bool
prim_ltEqInt external
#endif

-- Compares two floating point numbers.
ltEqFloat :: Float -> Float -> Bool
#ifdef __KICS2__
ltEqFloat external
#elif defined(__PAKCS__)
ltEqFloat x y = (prim_ltEqFloat $# y) $# x

prim_ltEqFloat :: Float -> Float -> Bool
prim_ltEqFloat external
#endif

type ShowS = String -> String

class Show a where
  show :: a -> String
  showsPrec :: Int -> a -> ShowS
  showList :: [a] -> ShowS

  show x = shows x ""
  showsPrec _ x s = show x ++ s
  showList = showListDefault

instance Show Char where
  showsPrec _ c = showString (showCharLiteral c)
  showList cs | null cs   = showString "\"\""
              | otherwise = showString (showStringLiteral cs)

instance Show Int where
  showsPrec = showSigned (showString . showIntLiteral)

instance Show Float where
  showsPrec = showSigned (showString . showFloatLiteral)

instance Show () where
  showsPrec _ () = showString "()"

instance (Show a, Show b) => Show (a, b) where
  showsPrec _ (a, b) = showTuple [shows a, shows b]

instance (Show a, Show b, Show c) => Show (a, b, c) where
  showsPrec _ (a, b, c) = showTuple [shows a, shows b, shows c]

instance (Show a, Show b, Show c, Show d) => Show (a, b, c, d) where
  showsPrec _ (a, b, c, d) = showTuple [shows a, shows b, shows c, shows d]

instance (Show a, Show b, Show c, Show d, Show e) => Show (a, b, c, d, e) where
  showsPrec _ (a, b, c, d, e) =
    showTuple [shows a, shows b, shows c, shows d, shows e]

instance Show a => Show [a] where
  showsPrec _ = showList

instance Show Bool where
  showsPrec _ False = showString "False"
  showsPrec _ True  = showString "True"

instance Show Ordering where
  showsPrec _ LT = showString "LT"
  showsPrec _ EQ = showString "EQ"
  showsPrec _ GT = showString "GT"

shows :: Show a => a -> ShowS
shows = showsPrec 0

showChar :: Char -> ShowS
showChar = (:)

showString :: String -> ShowS
showString str s = foldr showChar s str

showListDefault :: Show a => [a] -> ShowS
showListDefault []     s = "[]" ++ s
showListDefault (x:xs) s = '[' : shows x (showl xs)
 where showl []     = ']' : s
       showl (y:ys) = ',' : shows y (showl ys)

showParen :: Bool -> ShowS -> ShowS
showParen b s = if b then showChar '(' . s . showChar ')' else s

showSigned :: Real a => (a -> ShowS) -> Int -> a -> ShowS
showSigned showPos p x
  | x < 0     = showParen (p > 6) (showChar '-' . showPos (-x))
  | otherwise = showPos x

showTuple :: [ShowS] -> ShowS
showTuple ss = showChar '('
             . foldr1 (\s r -> s . showChar ',' . r) ss
             . showChar ')'

-- Returns the string representation of a character.
showCharLiteral :: Char -> String
showCharLiteral x = prim_showCharLiteral $## x

prim_showCharLiteral :: Char -> String
prim_showCharLiteral external

-- Returns the string representation of a string.
showStringLiteral :: String -> String
showStringLiteral x = prim_showStringLiteral $## x

prim_showStringLiteral :: String -> String
prim_showStringLiteral external

-- Returns the string representation of an integer.
showIntLiteral :: Int -> String
showIntLiteral x = prim_showIntLiteral $## x

prim_showIntLiteral :: Int -> String
prim_showIntLiteral external

-- Returns the string representation of a floating point number.
showFloatLiteral :: Float -> String
showFloatLiteral x = prim_showFloatLiteral $## x

prim_showFloatLiteral :: Float -> String
prim_showFloatLiteral external

type ReadS a = String -> [(a, String)]

class Read a where
  readsPrec :: Int -> ReadS a
  readList :: ReadS [a]

  readList = readListDefault

instance Read Char where
  readsPrec _ = readParen False
                  (\s -> [ (c, t) | (x, t) <- lex s, not (null x)
                                  , head x == '\''
                                  , (c, []) <- readCharLiteral x ])
  readList xs = readParen False
                  (\s -> [ (cs, t) | (x, t) <- lex s, not (null x)
                                   , head x == '"'
                                   , (cs, []) <- readStringLiteral x ]) xs
                ++ readListDefault xs

instance Read Int where
  readsPrec _ = readSigned (\s -> [ (i, t) | (x, t) <- lexDigits s
                                           , (i, []) <- readNatLiteral x ])

instance Read Float where
  readsPrec _ = readSigned
                  (\s -> [ (f, t) | (x, t) <- lex s, not (null x)
                                  , isDigit (head x), (f, []) <- readFloat x ])
   where
    readFloat x = if all isDigit x
                    then [(fromInt i, t) | (i, t) <- readNatLiteral x]
                    else readFloatLiteral x

instance Read () where
  readsPrec _ = readParen False (\r -> [ ((), t) | ("(", s) <- lex r
                                                 , (")", t) <- lex s ])

instance (Read a, Read b) => Read (a, b) where
  readsPrec _ = readParen False (\r -> [ ((a, b), w) | ("(", s) <- lex r
                                                     , (a, t) <- reads s
                                                     , (",", u) <- lex t
                                                     , (b, v) <- reads u
                                                     , (")", w) <- lex v ])

instance (Read a, Read b, Read c) => Read (a, b, c) where
  readsPrec _ = readParen False (\r -> [ ((a, b, c), y) | ("(", s) <- lex r
                                                        , (a, t) <- reads s
                                                        , (",", u) <- lex t
                                                        , (b, v) <- reads u
                                                        , (",", w) <- lex v
                                                        , (c, x) <- reads w
                                                        , (")", y) <- lex x ])

instance (Read a, Read b, Read c, Read d) => Read (a, b, c, d) where
  readsPrec _ = readParen False
                  (\q -> [ ((a, b, c, d), z) | ("(", r) <- lex q
                                             , (a, s) <- reads r
                                             , (",", t) <- lex s
                                             , (b, u) <- reads t
                                             , (",", v) <- lex u
                                             , (c, w) <- reads v
                                             , (",", x) <- lex w
                                             , (d, y) <- reads x
                                             , (")", z) <- lex y ])

instance (Read a, Read b, Read c, Read d, Read e) => Read (a, b, c, d, e) where
  readsPrec _ = readParen False
                  (\o -> [ ((a, b, c, d, e), z) | ("(", p) <- lex o
                                                , (a, q) <- reads p
                                                , (",", r) <- lex q
                                                , (b, s) <- reads r
                                                , (",", t) <- lex s
                                                , (c, u) <- reads t
                                                , (",", v) <- lex u
                                                , (d, w) <- reads v
                                                , (",", x) <- lex w
                                                , (e, y) <- reads x
                                                , (")", z) <- lex y ])

instance Read a => Read [a] where
  readsPrec _ = readList

instance Read Bool where
  readsPrec _ r =
    readParen False (\s -> [(False, t) | ("False", t) <- lex s]) r ++
      readParen False (\s -> [(True, t) | ("True", t) <- lex s]) r

instance Read Ordering where
  readsPrec _ r =
    readParen False (\s -> [(LT, t) | ("LT", t) <- lex s]) r ++
      readParen False (\s -> [(EQ, t) | ("EQ", t) <- lex s]) r ++
      readParen False (\s -> [(GT, t) | ("GT", t) <- lex s]) r

reads :: Read a => ReadS a
reads = readsPrec 0

readListDefault :: Read a => ReadS [a]
readListDefault = readParen False (\r -> [pr | ("[",s) <- lex r, pr <- readl s])
 where readl s = [([], t) | ("]", t) <- lex s] ++
                   [(x : xs, u) | (x, t) <- reads s, (xs, u) <- readl' t]
       readl' s = [([], t) | ("]", t) <- lex s] ++
                   [ (x : xs, v) | (",", t)  <- lex s, (x, u) <- reads t
                                 , (xs,v) <- readl' u ]

readParen :: Bool -> ReadS a -> ReadS a
readParen b g = if b then mandatory else optional
 where optional r = g r ++ mandatory r
       mandatory r =
         [(x, u) | ("(", s) <- lex r, (x, t) <- optional s, (")", u) <- lex t]

readSigned :: Real a => ReadS a -> ReadS a
readSigned p = readParen False read'
 where read' r = read'' r ++ [(-x, t) | ("-", s) <- lex r, (x, t) <- read'' s]
       read'' r = [(n, s) | (str, s) <- lex r, (n, "") <- p str]

read :: Read a => String -> a
read s =  case [x | (x, t) <- reads s, ("", "") <- lex t] of
  [x] -> x

lex :: ReadS String
lex xs = case xs of
  ""                  -> [("", "")]
  (c:cs) | isSpace c  -> lex $ dropWhile isSpace cs
  ('\'':s)            ->
    [('\'' : ch ++ "'", t) | (ch, '\'' : t)  <- lexCharLiteral s, ch /= "'"]
  ('"':s)             -> [('"' : str, t) | (str, t) <- lexString s]
  (c:cs) | isSingle c -> [([c], cs)]
         | isSymbol c -> [(c : sym, t) | (sym, t) <- [span isSymbol cs]]
         | isAlpha c  -> [(c : nam, t) | (nam, t) <- [span isIdChar cs]]
         | isDigit c  -> [ (c : ds ++ fe, t) | (ds, s) <- [span isDigit cs]
                                             , (fe, t) <- lexFracExp s ]
         | otherwise  -> []
 where
  isSingle c = c `elem` ",;()[]{}_`"
  isSymbol c = c `elem` "!@#$%&*+./<=>?\\^|:-~"
  isIdChar c = isAlphaNum c || c `elem` "_'"
  lexFracExp s = case s of
    ('.':c:cs) | isDigit c ->
      [('.' : ds ++ e, u) | (ds, t) <- lexDigits (c : cs), (e, u) <- lexExp t]
    _                      -> lexExp s
  lexExp s = case s of
    (e:cs) | e `elem` "eE" ->
      [ (e : c : ds, u) | (c:t) <- [cs], c `elem` "+-"
                        , (ds, u) <- lexDigits t ] ++
        [(e : ds, t) | (ds, t) <- lexDigits cs]
    _                      -> [("", s)]
  lexString s = case s of
    ('"':cs) -> [("\"", cs)]
    _        -> [ (ch ++ str, u) | (ch, t) <- lexStringItem s
                                  , (str, u) <- lexString t ]
  lexStringItem s = case s of
    ('\\':'&':cs)           -> [("\\&", cs)]
    ('\\':c:cs) | isSpace c -> [("\\&", t) | '\\':t <- [dropWhile isSpace cs]]
    _                       -> lexCharLiteral s

lexCharLiteral :: ReadS String
lexCharLiteral xs = case xs of
  ""        -> []
  ('\\':cs) -> map (prefix '\\') (lexEsc cs)
  (c:cs)    -> [([c], cs)]
 where
  lexEsc s = case s of
    (c:cs) | c `elem` "abfnrtv\\\"'"  -> [([c], cs)]
    ('^':c:cs) | c >= '@' && c <= '_' -> [(['^', c], cs)]
    ('b':cs)                          -> [prefix 'b' (span isBinDigit cs)]
    ('o':cs)                          -> [prefix 'o' (span isOctDigit cs)]
    ('x':cs)                          -> [prefix 'x' (span isHexDigit cs)]
    cs@(d:_) | isDigit d              -> [span isDigit cs]
    cs@(c:_) | isUpper c              -> [span isCharName cs]
    _                                 -> []
  isCharName c = isUpper c || isDigit c
  prefix c (t, cs) = (c : t, cs)

lexDigits :: ReadS String
lexDigits s = [(cs, t) | (cs@(_:_), t) <- [span isDigit s]]

readCharLiteral :: ReadS Char
readCharLiteral s = prim_readCharLiteral $## s

prim_readCharLiteral :: String -> [(Char, String)]
prim_readCharLiteral external

readStringLiteral :: ReadS String
readStringLiteral s = prim_readStringLiteral $## s

prim_readStringLiteral :: String -> [(String, String)]
prim_readStringLiteral external

readNatLiteral :: ReadS Int
readNatLiteral s = prim_readNatLiteral $## s

prim_readNatLiteral :: String -> [(Int, String)]
prim_readNatLiteral external

readFloatLiteral :: ReadS Float
readFloatLiteral s = prim_readFloatLiteral $## s

prim_readFloatLiteral :: String -> [(Float, String)]
prim_readFloatLiteral external

class Bounded a where
  minBound, maxBound :: a

instance Bounded Char where
  minBound = chr 0
  maxBound = chr 0x10FFFF

instance Bounded () where
  minBound = ()
  maxBound = ()

instance (Bounded a, Bounded b) => Bounded (a, b) where
  minBound = (minBound, minBound)
  maxBound = (maxBound, maxBound)

instance (Bounded a, Bounded b, Bounded c) => Bounded (a, b, c) where
  minBound = (minBound, minBound, minBound)
  maxBound = (maxBound, maxBound, maxBound)

instance (Bounded a, Bounded b, Bounded c, Bounded d) => Bounded (a, b, c, d) where
  minBound = (minBound, minBound, minBound, minBound)
  maxBound = (maxBound, maxBound, maxBound, maxBound)

instance (Bounded a, Bounded b, Bounded c, Bounded d, Bounded e) =>
           Bounded (a, b, c, d, e) where
  minBound = (minBound, minBound, minBound, minBound, minBound)
  maxBound = (maxBound, maxBound, maxBound, maxBound, maxBound)

instance Bounded Bool where
  maxBound = False
  minBound = True

instance Bounded Ordering where
  maxBound = LT
  minBound = GT

class Enum a where
  succ :: a -> a
  pred :: a -> a
  toEnum :: Int -> a
  fromEnum :: a -> Int
  enumFrom :: a -> [a]
  enumFromThen :: a -> a -> [a]
  enumFromTo :: a -> a -> [a]
  enumFromThenTo :: a -> a -> a -> [a]

  succ = toEnum . (+ 1) . fromEnum
  pred = toEnum . (\x -> x - 1) . fromEnum
  enumFrom x = map toEnum [fromEnum x ..]
  enumFromThen x y = map toEnum [fromEnum x, fromEnum y ..]
  enumFromTo x y = map toEnum [fromEnum x .. fromEnum y]
  enumFromThenTo x y z = map toEnum [fromEnum x, fromEnum y .. fromEnum z]

instance Enum Char where
  succ c | c < maxBound = chr $ ord c + 1
  pred c | c > minBound = chr $ ord c - 1
  toEnum = chr
  fromEnum = ord
  enumFrom x = [x .. maxBound]
  enumFromThen x y | y >= x    = [x, y .. maxBound]
                   | otherwise = [x, y .. minBound]

instance Enum Int where
  succ x = x + 1
  pred x = x - 1
  toEnum n = n
  fromEnum n = n
  enumFrom x = x : enumFrom (x + 1)
  enumFromTo x y | x > y     = []
                 | otherwise = x : enumFromTo (x + 1) y
  enumFromThen x y = iterate ((y - x) +) x
  enumFromThenTo x y z = takeWhile p (enumFromThen x y)
   where p x' | y >= x    = x' <= z
              | otherwise = x' >= z

instance Enum () where
  succ _ = failed
  pred _ = failed
  toEnum 0 = ()
  fromEnum () = 0
  enumFrom () = [()]
  enumFromThen () () = let units = () : units in units
  enumFromTo () () = [()]
  enumFromThenTo () () () = let units = () : units in units

instance Enum Bool where
  succ False = True
  pred True  = False
  toEnum 0 = False
  toEnum 1 = True
  fromEnum False = 0
  fromEnum True  = 1
  enumFrom x = enumFromTo x True
  enumFromThen x y = enumFromThenTo x y (x <= y)

instance Enum Ordering where
  succ LT = EQ
  succ EQ = GT
  pred EQ = LT
  pred GT = EQ
  toEnum 0 = LT
  toEnum 1 = EQ
  toEnum 2 = GT
  fromEnum LT = 0
  fromEnum EQ = 1
  fromEnum GT = 2
  enumFrom x = enumFromTo x GT
  enumFromThen x y = enumFromThenTo x y (if x <= y then GT else LT)


class Num a where
  (+), (-), (*) :: a -> a -> a
  negate :: a -> a
  abs :: a -> a
  signum :: a -> a
  fromInt :: Int -> a

  x - y = x + negate y
  negate x = 0 - x

instance Num Int where
  x + y = x `plusInt` y
  x - y = x `minusInt` y
  x * y = x `timesInt` y
  negate x = 0 - x
  abs x | x >= 0    = x
        | otherwise = negate x
  signum x | x >  0    = 1
           | x == 0    = 0
           | otherwise = -1
  fromInt x = x

instance Num Float where
  x + y = x `plusFloat` y
  x - y = x `minusFloat` y
  x * y = x `timesFloat` y
  negate x = negateFloat x
  abs x | x >= 0    = x
        | otherwise = negate x
  signum x | x >  0    = 1
           | x == 0    = 0
           | otherwise = -1
  fromInt x = intToFloat x

-- Adds two integers.
plusInt :: Int -> Int -> Int
#ifdef __KICS2__
plusInt external
#elif defined(__PAKCS__)
x `plusInt` y = (prim_plusInt $# y) $# x

prim_plusInt :: Int -> Int -> Int
prim_plusInt external
#endif

-- Subtracts two integers.
minusInt :: Int -> Int -> Int
#ifdef __KICS2__
minusInt external
#elif defined(__PAKCS__)
x `minusInt` y = (prim_minusInt $# y) $# x

prim_minusInt :: Int -> Int -> Int
prim_minusInt external
#endif

-- Multiplies two integers.
timesInt :: Int -> Int -> Int
#ifdef __KICS2__
timesInt external
#elif defined(__PAKCS__)
x `timesInt` y = (prim_timesInt $# y) $# x

prim_timesInt :: Int -> Int -> Int
prim_timesInt external
#endif

-- Adds two floating point numbers.
plusFloat :: Float -> Float -> Float
x `plusFloat` y = (prim_plusFloat $# y) $# x

prim_plusFloat :: Float -> Float -> Float
prim_plusFloat external

-- Subtracts two floating point numbers.
minusFloat :: Float -> Float -> Float
x `minusFloat` y = (prim_minusFloat $# y) $# x

prim_minusFloat :: Float -> Float -> Float
prim_minusFloat external

-- Multiplies two floating point numbers.
timesFloat :: Float -> Float -> Float
x `timesFloat` y = (prim_timesFloat $# y) $# x

prim_timesFloat :: Float -> Float -> Float
prim_timesFloat external

-- Negates a floating point number.
negateFloat :: Float -> Float
#ifdef __KICS2__
negateFloat external
#elif defined(__PAKCS__)
negateFloat x = prim_negateFloat $# x

prim_negateFloat :: Float -> Float
prim_negateFloat external
#endif

-- Converts from integers to floating point numbers.
intToFloat :: Int -> Float
intToFloat x = prim_intToFloat $# x

prim_intToFloat :: Int -> Float
prim_intToFloat external


class Num a => Fractional a where
  (/) :: a -> a -> a
  recip :: a -> a
  fromFloat :: Float -> a

  recip x = 1.0 / x
  x / y = x * recip y

instance Fractional Float where
  x / y = x `divFloat` y
  fromFloat x = x

-- Division on floating point numbers.
divFloat :: Float -> Float -> Float
x `divFloat` y = (prim_divFloat $# y) $# x

prim_divFloat :: Float -> Float -> Float
prim_divFloat external

class (Num a, Ord a) => Real a where
  toFloat :: a -> Float

instance Real Int where
  toFloat x = fromInt x

instance Real Float where
  toFloat x = x


class (Real a, Enum a) => Integral a where
  div, mod :: a -> a -> a
  quot, rem :: a -> a -> a
  divMod :: a -> a -> (a, a)
  quotRem :: a -> a -> (a, a)
  toInt :: a -> Int

  n `div` d = q
   where (q, _) = divMod n d
  n `mod` d = r
   where (_, r) = divMod n d
  n `quot` d = q
   where (q, _) = quotRem n d
  n `rem` d = r
   where (_, r) = quotRem n d

instance Integral Int where
  divMod n d = (n `divInt` d, n `modInt` d)
  quotRem n d = (n `quotInt` d, n `remInt` d)
  toInt x = x

--- Returns whether an integer is even.
even :: Integral a => a -> Bool
even n = n `rem` 2 == 0

--- Returns whether an integer is odd.
odd :: Integral a => a -> Bool
odd = not . even

--- General coercion from integral types.
fromIntegral :: (Integral a, Num b) => a -> b
fromIntegral = fromInt . toInt

--- General coercion to fractional types.
realToFrac :: (Real a, Fractional b) => a -> b
realToFrac = fromFloat . toFloat

-- Integer division. The value is the integer quotient of its arguments
-- and always truncated towards negative infinity.
divInt :: Int -> Int -> Int
#ifdef __KICS2__
divInt external
#elif defined(__PAKCS__)
x `divInt` y = (prim_divInt $# y) $# x

prim_divInt :: Int -> Int -> Int
prim_divInt external
#endif

-- Integer remainder. The value is the remainder of the integer division
-- and it obeys the rule `mod x y = x - y * (div x y)`.
modInt :: Int -> Int -> Int
#ifdef __KICS2__
modInt external
#elif defined(__PAKCS__)
x `modInt` y = (prim_modInt $# y) $# x

prim_modInt :: Int -> Int -> Int
prim_modInt external
#endif

-- Integer division. The value is the integer quotient of its arguments
-- and always truncated towards zero.
quotInt :: Int -> Int -> Int
#ifdef __KICS2__
quotInt external
#elif defined(__PAKCS__)
x `quotInt` y = (prim_quotInt $# y) $# x

prim_quotInt :: Int -> Int -> Int
prim_quotInt external
#endif

-- Integer remainder. The value is the remainder of the integer division
-- and it obeys the rule `rem x y = x - y * (quot x y)`.
remInt :: Int -> Int -> Int
#ifdef __KICS2__
remInt external
#elif defined(__PAKCS__)
x `remInt` y = (prim_remInt $# y) $# x

prim_remInt :: Int -> Int -> Int
prim_remInt external
#endif

class (Real a, Fractional a) => RealFrac a where
  properFraction :: Integral b => a -> (b, a)
  truncate :: Integral b => a -> b
  round :: Integral b => a -> b
  ceiling :: Integral b => a -> b
  floor :: Integral b => a -> b

  truncate x = m
   where (m, _) = properFraction x
  round x = let (n, r) = properFraction x
                m      = if r < 0 then n - 1 else n + 1
            in case compare (signum (abs r - 0.5)) 0 of
                 LT -> n
                 EQ -> if even n then n else m
                 GT -> m
  ceiling x = if r > 0 then n + 1 else n
   where (n, r) = properFraction x
  floor x = if r < 0 then n - 1 else n
   where (n, r) = properFraction x

instance RealFrac Float where
  properFraction x = (n, x - fromIntegral n)
   where n = truncate x
  truncate = fromInt . truncateFloat
  round = fromInt . roundFloat

-- Conversion function from floating point numbers to integers.
-- The result is the closest integer between the argument and 0.
truncateFloat :: Float -> Int
truncateFloat x = prim_truncateFloat $# x

prim_truncateFloat :: Float -> Int
prim_truncateFloat external

-- Conversion function from floating point numbers to integers.
-- The result is the nearest integer to the argument.
-- If the argument is equidistant between two integers,
-- it is rounded to the closest even integer value.
roundFloat :: Float -> Int
roundFloat x = prim_roundFloat $# x

prim_roundFloat :: Float -> Int
prim_roundFloat external

class Fractional a => Floating a where
  pi :: a
  exp, log, sqrt :: a -> a
  (**), logBase :: a -> a -> a
  sin, cos, tan :: a -> a
  asin, acos, atan :: a -> a
  sinh, cosh, tanh :: a -> a
  asinh, acosh, atanh :: a -> a

  sqrt x = x ** 0.5
  x ** y = exp (log x * y)
  logBase x y = log y / log x
  tan x = sin x / cos x
  tanh x = sinh x / cosh x

instance Floating Float where
  pi = 3.141592653589793238
  exp = expFloat
  log = logFloat
  sqrt = sqrtFloat
  sin = sinFloat
  cos = cosFloat
  tan = tanFloat
  asin = asinFloat
  acos = acosFloat
  atan = atanFloat
  sinh = sinhFloat
  cosh = coshFloat
  tanh = tanhFloat
  asinh = asinhFloat
  acosh = acoshFloat
  atanh = atanhFloat

-- Natural logarithm.
logFloat :: Float -> Float
logFloat x = prim_logFloat $# x

prim_logFloat :: Float -> Float
prim_logFloat external

-- Natural exponent.
expFloat :: Float -> Float
expFloat x = prim_expFloat $# x

prim_expFloat :: Float -> Float
prim_expFloat external

-- Square root.
sqrtFloat :: Float -> Float
sqrtFloat x = prim_sqrtFloat $# x

prim_sqrtFloat :: Float -> Float
prim_sqrtFloat external

-- Sine.
sinFloat :: Float -> Float
sinFloat x = prim_sinFloat $# x

prim_sinFloat :: Float -> Float
prim_sinFloat external

-- Cosine.
cosFloat :: Float -> Float
cosFloat x = prim_cosFloat $# x

prim_cosFloat :: Float -> Float
prim_cosFloat external

-- Tangent.
tanFloat :: Float -> Float
tanFloat x = prim_tanFloat $# x

prim_tanFloat :: Float -> Float
prim_tanFloat external

-- Arcus sine.
asinFloat :: Float -> Float
asinFloat x = prim_asinFloat $# x

prim_asinFloat :: Float -> Float
prim_asinFloat external

-- Arcus cosine.
acosFloat :: Float -> Float
acosFloat x = prim_acosFloat $# x

prim_acosFloat :: Float -> Float
prim_acosFloat external

-- Arcus tangent.
atanFloat :: Float -> Float
atanFloat x = prim_atanFloat $# x

prim_atanFloat :: Float -> Float
prim_atanFloat external

-- Hyperbolic sine.
sinhFloat :: Float -> Float
sinhFloat x = prim_sinhFloat $# x

prim_sinhFloat :: Float -> Float
prim_sinhFloat external

-- Hyperbolic cosine.
coshFloat :: Float -> Float
coshFloat x = prim_coshFloat $# x

prim_coshFloat :: Float -> Float
prim_coshFloat external

-- Hyperbolic tangent.
tanhFloat :: Float -> Float
tanhFloat x = prim_tanhFloat $# x

prim_tanhFloat :: Float -> Float
prim_tanhFloat external

-- Hyperbolic arcus sine.
asinhFloat :: Float -> Float
asinhFloat x = prim_asinhFloat $# x

prim_asinhFloat :: Float -> Float
prim_asinhFloat external

-- Hyperbolic arcus cosine.
acoshFloat :: Float -> Float
acoshFloat x = prim_acoshFloat $# x

prim_acoshFloat :: Float -> Float
prim_acoshFloat external

-- Hyperbolic arcus tangent.
atanhFloat :: Float -> Float
atanhFloat x = prim_atanhFloat $# x

prim_atanhFloat :: Float -> Float
prim_atanhFloat external


(^) :: (Num a, Integral b) => a -> b -> a
x0 ^ y0 | y0 < 0    = error "Negative exponent"
        | y0 == 0   = 1
        | otherwise = f x0 y0
    where -- f : x0 ^ y0 = x ^ y
          f x y | even y    = f (x * x) (y `quot` 2)
                | y == 1    = x
                | otherwise = g (x * x) (y `quot` 2) x
          -- g : x0 ^ y0 = (x ^ y) * z
          g x y z | even y = g (x * x) (y `quot` 2) z
                  | y == 1 = x * z
                  | otherwise = g (x * x) (y `quot` 2) (x * z)

class Monoid a where
  mempty  :: a
  mappend :: a -> a -> a
  mconcat :: [a] -> a

  mconcat = foldr mappend mempty

instance Monoid () where
  mempty = ()
  _ `mappend` _ = ()
  mconcat _ = ()

instance (Monoid a, Monoid b) => Monoid (a, b) where
  mempty = (mempty, mempty)
  (a1, b1) `mappend` (a2,b2) = (a1 `mappend` a2, b1 `mappend` b2)

instance (Monoid a, Monoid b, Monoid c) => Monoid (a, b, c) where
  mempty = (mempty, mempty, mempty)
  (a1, b1, c1) `mappend` (a2, b2, c2) =
    (a1 `mappend` a2, b1 `mappend` b2, c1 `mappend` c2)

instance (Monoid a, Monoid b, Monoid c, Monoid d) => Monoid (a, b, c, d) where
  mempty = (mempty, mempty, mempty, mempty)
  (a1, b1, c1, d1) `mappend` (a2, b2, c2, d2) =
    (a1 `mappend` a2, b1 `mappend` b2, c1 `mappend` c2, d1 `mappend` d2)

instance (Monoid a, Monoid b, Monoid c, Monoid d, Monoid e) =>
           Monoid (a, b, c, d, e) where
  mempty = (mempty, mempty, mempty, mempty, mempty)
  (a1, b1, c1, d1, e1) `mappend` (a2, b2, c2, d2, e2) =
    (a1 `mappend` a2, b1 `mappend` b2, c1 `mappend` c2, d1 `mappend` d2,
      e1 `mappend` e2)

instance Monoid [a] where
  mempty  = []
  mappend = (++)
  mconcat xss = [x | xs <- xss, x <- xs]

instance Monoid b => Monoid (a -> b) where
  mempty _ = mempty
  mappend f g x = f x `mappend` g x

instance Monoid Ordering where
  mempty = EQ
  LT `mappend` _ = LT
  EQ `mappend` y = y
  GT `mappend` _ = GT


class Functor f where
  fmap :: (a -> b) -> f a -> f b
  (<$) :: a -> f b -> f a

  (<$) = fmap . const

instance Functor [] where
  fmap = map

instance Functor ((->) r) where
  fmap = (.)

(<$>) :: Functor f => (a -> b) -> f a -> f b
(<$>) = fmap


class Functor f => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
  (*>) :: f a -> f b -> f b
  (<*) :: f a -> f b -> f a
  liftA2 :: (a -> b -> c) -> f a -> f b -> f c

  (<*>) = liftA2 id
  a1 *> a2 = (id <$ a1) <*> a2
  (<*) = liftA2 const
  liftA2 f x = (<*>) (fmap f x)

instance Applicative [] where
  pure x    = [x]
  fs <*> xs = [f x | f <- fs, x <- xs]
  xs *> ys  = [y | _ <- xs, y <- ys]
  liftA2 f xs ys = [f x y | x <- xs, y <- ys]

instance Applicative ((->) a) where
  pure = const
  (<*>) f g x = f x (g x)
  liftA2 q f g x = q (f x) (g x)


-- | A monoid on applicative functors.
--
-- If defined, 'some' and 'many' should be the least solutions
-- of the equations:
--
-- * @'some' v = (:) '<$>' v '<*>' 'many' v@
--
-- * @'many' v = 'some' v '<|>' 'pure' []@
class Applicative f => Alternative f where
    -- | The identity of '<|>'
    empty :: f a
    -- | An associative binary operation
    (<|>) :: f a -> f a -> f a

    -- | One or more.
    some :: f a -> f [a]
    some v = some_v
      where
        many_v = some_v <|> pure []
        some_v = (:) <$> v <*> many_v

    -- | Zero or more.
    many :: f a -> f [a]
    many v = many_v
      where
        many_v = some_v <|> pure []
        some_v = (:) <$> v <*> many_v

instance Alternative [] where
    empty = []
    (<|>) = (++)


class Applicative m => Monad m where
  (>>=) :: m a -> (a -> m b) -> m b
  (>>) :: m a -> m b -> m b
  return :: a -> m a

  return = pure
  m >> k = m >>= \_ -> k

instance Monad [] where
  xs >>= f = [y | x <- xs, y <- f x]
  (>>) = (*>)

instance Monad ((->) r) where
  f >>= k = \ r -> k (f r) r

class Monad m => MonadFail m where
  fail :: String -> m a

instance MonadFail [] where
  fail _ = []

ap :: Monad m => m (a -> b) -> m a -> m b
ap m1 m2 = do
  x1 <- m1
  x2 <- m2
  return (x1 x2)

liftM2 :: Monad m => (a1 -> a2 -> r) -> m a1 -> m a2 -> m r
liftM2 f m1 m2 = do
  x1 <- m1
  x2 <- m2
  return (f x1 x2)

--- Executes a sequence of monadic actions and collects all results in a list.
sequence :: Monad m => [m a] -> m [a]
sequence []     = return []
sequence (c:cs) = do x <- c
                     xs <- sequence cs
                     return (x : xs)

--- Executes a sequence of monadic actions and ignores the results.
sequence_ :: Monad m => [m _] -> m ()
sequence_ = foldr (>>) (return ())

--- Maps a monadic action function on a list of elements.
--- The results of all monadic actions are collected in a list.
mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM f = sequence . map f

--- Maps an monadic action function on a list of elements.
--- The results of all monadic actions are ignored.
mapM_ :: Monad m => (a -> m _) -> [a] -> m ()
mapM_ f = sequence_ . map f

--- Returns true if the argument is an uppercase letter.
isUpper :: Char -> Bool
isUpper c = c >= 'A' && c <= 'Z'

--- Returns true if the argument is an lowercase letter.
isLower :: Char -> Bool
isLower c = c >= 'a' && c <= 'z'

--- Returns true if the argument is a letter.
isAlpha :: Char -> Bool
isAlpha c = isUpper c || isLower c

--- Returns true if the argument is a decimal digit.
isDigit :: Char -> Bool
isDigit c = c >= '0' && c <= '9'

--- Returns true if the argument is a letter or digit.
isAlphaNum :: Char -> Bool
isAlphaNum c = isAlpha c || isDigit c

--- Returns true if the argument is a binary digit.
isBinDigit :: Char -> Bool
isBinDigit c = c >= '0' || c <= '1'

--- Returns true if the argument is an octal digit.
isOctDigit :: Char -> Bool
isOctDigit c = c >= '0' && c <= '7'

--- Returns true if the argument is a hexadecimal digit.
isHexDigit :: Char -> Bool
isHexDigit c = isDigit c || c >= 'A' && c <= 'F'
                         || c >= 'a' && c <= 'f'

--- Returns true if the argument is a white space.
isSpace :: Char -> Bool
isSpace c = c == ' '    || c == '\t' || c == '\n' ||
            c == '\r'   || c == '\f' || c == '\v' ||
            c == '\xa0' || ord c `elem` [5760, 6158, 8192, 8239, 8287, 12288]

--- Converts a character into its ASCII value.
ord :: Char -> Int
ord c = prim_ord $# c

prim_ord :: Char -> Int
prim_ord external

--- Converts a Unicode value into a character.
--- The conversion is total, i.e., for out-of-bound values, the smallest
--- or largest character is generated.
chr :: Int -> Char
chr n | n < 0       = prim_chr 0
      | n > 1114111 = prim_chr 1114111
      | otherwise   = prim_chr $# n

prim_chr :: Int -> Char
prim_chr external

type String = [Char]

--- Breaks a string into a list of lines where a line is terminated at a
--- newline character. The resulting lines do not contain newline characters.
lines :: String -> [String]
lines []       = []
lines as@(_:_) = let (l, bs) = splitLine as in l : lines bs
 where splitLine []     = ([], [])
       splitLine (c:cs) = if c == '\n' then ([], cs)
                                       else let (ds, es) = splitLine cs
                                            in (c : ds, es)

--- Concatenates a list of strings with terminating newlines.
unlines :: [String] -> String
unlines = concatMap (++ "\n")

--- Breaks a string into a list of words where the words are delimited by
--- white spaces.
words :: String -> [String]
words s = let s1 = dropWhile isSpace s
          in if s1 == "" then []
                         else let (w, s2) = break isSpace s1
                              in w : words s2

--- Concatenates a list of strings with a blank between two strings.
unwords :: [String] -> String
unwords ws = if ws == [] then []
                         else foldr1 (\w s -> w ++ ' ' : s) ws


--- Right-associative application.
($) :: (a -> b) -> a -> b
f $ x = f x

--- Right-associative application with strict evaluation of its argument
--- to head normal form.
($!) :: (a -> b) -> a -> b
($!) external

--- Right-associative application with strict evaluation of its argument
--- to normal form.
($!!) :: (a -> b) -> a -> b
($!!) external

--- Right-associative application with strict evaluation of its argument
--- to a non-variable term.
($#) :: (a -> b) -> a -> b
f $# x = f $! (ensureNotFree x)

--- Right-associative application with strict evaluation of its argument
--- to ground normal form.
($##) :: (a -> b) -> a -> b
($##) external

--- Evaluates the first argument to head normal form (which could also
--- be a free variable) and returns the second argument.
seq :: _ -> a -> a
x `seq` y = const y $! x

--- Evaluates the argument to head normal form and returns it.
--- Suspends until the result is bound to a non-variable term.
ensureNotFree :: a -> a
ensureNotFree external

--- Evaluates the argument to spine form and returns it.
--- Suspends until the result is bound to a non-variable spine.
ensureSpine :: [a] -> [a]
ensureSpine l = ensureList (ensureNotFree l)
 where ensureList []     = []
       ensureList (x:xs) = x : ensureSpine xs

--- Evaluates the argument to normal form and returns it.
normalForm :: a -> a
normalForm x = id $!! x

--- Evaluates the argument to ground normal form and returns it.
--- Suspends as long as the normal form of the argument is not ground.
groundNormalForm :: a -> a
groundNormalForm x = id $## x


--- Function composition.
(.) :: (b -> c) -> (a -> b) -> (a -> c)
f . g = \x -> f (g x)

--- Identity function.
id :: a -> a
id x = x

--- Constant function.
const :: a -> _ -> a
const x _ = x

--- `asTypeOf` is a type-restricted version of `const`.
--- It is usually used as an infix operator, and its typing forces its first
--- argument (which is usually overloaded) to have the same type as the second.
asTypeOf :: a -> a -> a
asTypeOf = const

--- Converts an uncurried function to a curried function.
curry :: ((a, b) -> c) -> a -> b -> c
curry f a b =  f (a, b)

--- Converts an curried function to a function on pairs.
uncurry :: (a -> b -> c) -> (a, b) -> c
uncurry f (a, b) = f a b

--- `flip f` is identical to `f`, but with the order of arguments reversed.
flip :: (a -> b -> c) -> b -> a -> c
flip f x y = f y x

--- Repeats application of a function until a predicate holds.
until :: (a -> Bool) -> (a -> a) -> a -> a
until p f x = if p x then x else until p f (f x)


--- Sequential conjunction on Booleans.
(&&) :: Bool -> Bool -> Bool
True  && x = x
False && _ = False

--- Sequential disjunction on Booleans.
(||) :: Bool -> Bool -> Bool
True  || _ = True
False || x = x

--- Negation on Booleans.
not :: Bool -> Bool
not True  = False
not False = True

--- Useful name for the last condition in a sequence of conditional equations.
otherwise :: Bool
otherwise = True

--- The standard conditional. It suspends if the condition is a free variable.
ifThenElse :: Bool -> a -> a -> a
ifThenElse b t f = case b of True  -> t
                             False -> f

--- Selects the first component of a pair.
fst :: (a, _) -> a
fst (x, _) = x

--- Selects the second component of a pair.
snd :: (_, b) -> b
snd (_, y) = y


--- Computes the first element of a list.
head :: [a] -> a
head (x:_) = x

--- Computes the remaining elements of a list.
tail :: [a] -> [a]
tail (_:xs) = xs

--- Is a list empty?
null :: [_] -> Bool
null []    = True
null (_:_) = False

--- Concatenates two lists.
--- Since it is flexible, it could be also used to split a list
--- into two sublists etc.
(++) :: [a] -> [a] -> [a]
[]     ++ ys = ys
(x:xs) ++ ys = x : xs ++ ys

--- Computes the length of a list.
length :: [_] -> Int
length [] = 0
length (_:xs) = 1 + length xs

--- List index (subscript) operator, head has index 0.
(!!) :: [a] -> Int -> a
(x:xs) !! n | n == 0 = x
            | n > 0  = xs !! (n - 1)

--- Maps a function on all elements of a list.
map :: (a -> b) -> [a] -> [b]
map _ []     = []
map f (x:xs) = f x : map f xs

--- Accumulates all list elements by applying a binary operator from
--- left to right.
foldl :: (a -> b -> a) -> a -> [b] -> a
foldl _ z []     = z
foldl f z (x:xs) = foldl f (f z x) xs

--- Accumulates a non-empty list from left to right.
foldl1 :: (a -> a -> a) -> [a] -> a
foldl1 f (x:xs) = foldl f x xs

--- Accumulates all list elements by applying a binary operator from
--- right to left.
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ z []     = z
foldr f z (x:xs) = f x (foldr f z xs)

--- Accumulates a non-empty list from right to left:
foldr1 :: (a -> a -> a) -> [a] -> a
foldr1 _ [x]          = x
foldr1 f (x:xs@(_:_)) = f x (foldr1 f xs)

--- Filters all elements satisfying a given predicate in a list.
filter :: (a -> Bool) -> [a] -> [a]
filter _ []     = []
filter p (x:xs) = if p x then x : filter p xs
                         else filter p xs

--- Joins two lists into one list of pairs. If one input list is shorter than
--- the other, the additional elements of the longer list are discarded.
zip :: [a] -> [b] -> [(a, b)]
zip []     _      = []
zip (_:_)  []     = []
zip (x:xs) (y:ys) = (x, y) : zip xs ys

--- Joins three lists into one list of triples. If one input list is shorter
--- than the other, the additional elements of the longer lists are discarded.
zip3 :: [a] -> [b] -> [c] -> [(a, b, c)]
zip3 []     _      _      = []
zip3 (_:_)  []     _      = []
zip3 (_:_)  (_:_)  []     = []
zip3 (x:xs) (y:ys) (z:zs) = (x, y, z) : zip3 xs ys zs

--- Joins two lists into one list by applying a combination function to
--- corresponding pairs of elements. Thus `zip = zipWith (,)`
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith _ []     _      = []
zipWith _ (_:_)  []     = []
zipWith f (x:xs) (y:ys) = f x y : zipWith f xs ys

--- Joins three lists into one list by applying a combination function to
--- corresponding triples of elements. Thus `zip3 = zipWith3 (,,)`
zipWith3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
zipWith3 _ []     _      _      = []
zipWith3 _ (_:_)  []     _      = []
zipWith3 _ (_:_)  (_:_)  []     = []
zipWith3 f (x:xs) (y:ys) (z:zs) = f x y z : zipWith3 f xs ys zs

--- Transforms a list of pairs into a pair of lists.
unzip :: [(a, b)] -> ([a], [b])
unzip []          = ([], [])
unzip ((x, y):ps) = (x : xs, y : ys)
 where (xs, ys) = unzip ps

--- Transforms a list of triples into a triple of lists.
unzip3 :: [(a, b, c)] -> ([a], [b], [c])
unzip3 []             = ([], [], [])
unzip3 ((x, y, z):ts) = (x : xs, y : ys, z : zs)
 where (xs, ys, zs) = unzip3 ts

--- Concatenates a list of lists into one list.
concat :: [[a]] -> [a]
concat = foldr (++) []

--- Maps a function from elements to lists and merges the result into one list.
concatMap :: (a -> [b]) -> [a] -> [b]
concatMap f = concat . map f

--- Infinite list of repeated applications of a function f to an element x.
--- Thus, `iterate f x = [x, f x, f (f x), ...]`.
iterate :: (a -> a) -> a -> [a]
iterate f x = x : iterate f (f x)

--- Infinite list where all elements have the same value.
--- Thus, `repeat x = [x, x, x, ...]`.
repeat :: a -> [a]
repeat x = x : repeat x

--- List of length n where all elements have the same value.
replicate :: Int -> a -> [a]
replicate n x = take n (repeat x)

--- Returns prefix of length n.
take :: Int -> [a] -> [a]
take n l = if n <= 0 then [] else takep n l
 where takep _ []     = []
       takep m (x:xs) = x : take (m - 1) xs

--- Returns suffix without first n elements.
drop :: Int -> [a] -> [a]
drop n xs = if n <= 0 then xs
                      else case xs of []     -> []
                                      (_:ys) -> drop (n - 1) ys

--- `splitAt n xs` is equivalent to `(take n xs, drop n xs)`
splitAt :: Int -> [a] -> ([a], [a])
splitAt n l = if n <= 0 then ([], l) else splitAtp n l
  where splitAtp _ []     = ([], [])
        splitAtp m (x:xs) = let (ys, zs) = splitAt (m - 1) xs in (x : ys, zs)

--- Returns longest prefix with elements satisfying a predicate.
takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile _ []     = []
takeWhile p (x:xs) = if p x then x : takeWhile p xs else []

--- Returns suffix without takeWhile prefix.
dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile _ []     = []
dropWhile p (x:xs) = if p x then dropWhile p xs else x : xs

--- `span p xs` is equivalent to `(takeWhile p xs, dropWhile p xs)`
span :: (a -> Bool) -> [a] -> ([a], [a])
span _ []     = ([], [])
span p (x:xs) | p x       = let (ys, zs) = span p xs in (x : ys, zs)
              | otherwise = ([], x : xs)

--- `break p xs` is equivalent to
--- `(takeWhile (not . p) xs, dropWhile (not . p) xs)`.
--- Thus, it breaks a list at the first occurrence of an element satisfying p.
break :: (a -> Bool) -> [a] -> ([a], [a])
break p = span (not . p)

--- Reverses the order of all elements in a list.
reverse :: [a] -> [a]
reverse = foldl (flip (:)) []

--- Computes the conjunction of a Boolean list.
and :: [Bool] -> Bool
and = foldr (&&) True

--- Computes the disjunction of a Boolean list.
or :: [Bool] -> Bool
or = foldr (||) False

--- Is there an element in a list satisfying a given predicate?
any :: (a -> Bool) -> [a] -> Bool
any p = or . map p

--- Is a given predicate satisfied by all elements in a list?
all :: (a -> Bool) -> [a] -> Bool
all p = and . map p

--- Element of a list?
elem :: Eq a => a -> [a] -> Bool
elem x = any (x ==)

--- Not element of a list?
notElem :: Eq a => a -> [a] -> Bool
notElem x = all (x /=)

--- Looks up a key in an association list.
lookup :: Eq a => a -> [(a, b)] -> Maybe b
lookup _ []          = Nothing
lookup k ((x,y):xys) | k == x    = Just y
                     | otherwise = lookup k xys

data Maybe a = Nothing | Just a
 deriving (Eq, Ord, Show, Read)

instance Monoid a => Monoid (Maybe a) where
  mempty = Nothing
  Nothing `mappend` m       = m
  Just m1 `mappend` Nothing = Just m1
  Just m1 `mappend` Just m2 = Just (m1 `mappend` m2)

instance Functor Maybe where
  fmap _ Nothing  = Nothing
  fmap f (Just x) = Just (f x)

instance Applicative Maybe where
  pure = Just
  Just f  <*> m = fmap f m
  Nothing <*> _ = Nothing
  Just _  *> m = m
  Nothing *> _ = Nothing
  liftA2 f (Just x) (Just y) = Just (f x y)
  liftA2 _ (Just _) Nothing  = Nothing
  liftA2 _ Nothing  _        = Nothing

instance Alternative Maybe where
    empty = Nothing
    Nothing <|> r = r
    Just l  <|> _ = Just l

instance Monad Maybe where
  Nothing >>= _ = Nothing
  Just x  >>= k = k x
  (>>) = (*>)

instance MonadFail Maybe where
  fail _ = Nothing

maybe :: b -> (a -> b) -> Maybe a -> b
maybe n _ Nothing  = n
maybe _ f (Just x) = f x

data Either a b = Left a
                | Right b
  deriving (Eq, Ord, Show, Read)

instance Functor (Either a) where
  fmap _ (Left e)  = Left e
  fmap f (Right x) = Right (f x)

instance Applicative (Either a) where
  pure = Right
  (<*>) = ap

instance Monad (Either a) where
  return          = Right
  (Left e)  >>= _ = Left e
  (Right x) >>= f = f x

either :: (a -> c) -> (b -> c) -> Either a b -> c
either left _     (Left  a) = left a
either _    right (Right b) = right b

external data IO _

instance Monoid a => Monoid (IO a) where
  mempty = pure mempty
  mappend = liftA2 mappend

instance  Functor IO where
  fmap f x = x >>= (pure . f)

instance Applicative IO where
  pure = returnIO
#ifdef __PAKCS__
  (*>) = seqIO
#else
  m *> k = m >>= \_ -> k
#endif
  (<*>) = ap
  liftA2 = liftM2

instance Alternative IO where
    empty = fail "mzero"

    m <|> n = m `catch` const n

instance Monad IO where
  (>>=) = bindIO
  (>>) = (*>)

instance MonadFail IO where
  fail s = ioError (userError s)

bindIO :: IO a -> (a -> IO b) -> IO b
bindIO external

seqIO :: IO a -> IO b -> IO b
seqIO external

returnIO :: a -> IO a
returnIO external

--- An action that reads a character from standard output and returns it.
getChar :: IO Char
getChar external

--- An action that reads a line from standard input and returns it.
getLine :: IO String
getLine = do c <- getChar
             case c of
               '\n' -> return []
               _ -> do cs <- getLine
                       return (c : cs)

--- An action that puts its character argument on standard output.
putChar :: Char -> IO ()
putChar c = prim_putChar $# c

prim_putChar :: Char -> IO ()
prim_putChar external

--- Action to print a string on standard output.
putStr :: String -> IO ()
putStr []     = return ()
putStr (c:cs) = putChar c >> putStr cs

--- Action to print a string with a newline on standard output.
putStrLn :: String -> IO ()
putStrLn cs = putStr cs >> putChar '\n'

--- Converts a term into a string and prints it.
print :: Show a => a -> IO ()
print = putStrLn . show

type FilePath = String

--- An action that (lazily) reads a file and returns its contents.
readFile :: FilePath -> IO String
readFile f = prim_readFile $## f

prim_readFile :: FilePath -> IO String
prim_readFile external

#ifdef __PAKCS__
-- Needed for internal implementation of readFile.
prim_readFileContents :: FilePath -> String
prim_readFileContents external
#endif

--- An action that writes a file.
writeFile :: FilePath -> String -> IO ()
writeFile f s = (prim_writeFile $## f) s

prim_writeFile :: FilePath -> String -> IO ()
prim_writeFile external

--- An action that appends a string to a file.
--- It behaves like `writeFile` if the file does not exist.
appendFile :: FilePath -> String -> IO ()
appendFile f s = (prim_appendFile $## f) s

prim_appendFile :: FilePath -> String -> IO ()
prim_appendFile external

--- The (abstract) type of error values.
--- Currently, it distinguishes between general I/O errors,
--- user-generated errors (see 'userError'), failures and non-determinism
--- errors during I/O computations. These errors can be caught by 'catch'.
--- Each error contains a string shortly explaining the error.
--- This type might be extended in the future to distinguish
--- further error situations.
data IOError
  = IOError String     -- normal IO error
  | UserError String   -- user-specified error
  | FailError String   -- failing computation
  | NondetError String -- non-deterministic computation
 deriving Eq

instance Show IOError where
  show (IOError     s) = "i/o error: " ++ s
  show (UserError   s) = "user error: " ++ s
  show (FailError   s) = "fail error: " ++ s
  show (NondetError s) = "nondet error: " ++ s

--- A user error value is created by providing a description of the
--- error situation as a string.
userError :: String -> IOError
userError = UserError

--- Raises an I/O exception with a given error value.
ioError :: IOError -> IO _
#ifdef __PAKCS__
ioError err = error (show err)
#else
ioError err = prim_ioError $## err

prim_ioError :: IOError -> IO _
prim_ioError external
#endif

--- Catches a possible error or failure during the execution of an
--- I/O action. `catch act errfun` executes the I/O action `act`.
--- If an exception or failure occurs during this I/O action, the
--- function `errfun` is applied to the error value.
catch :: IO a -> (IOError -> IO a) -> IO a
catch external


type Success = Bool

--- The always satisfiable constraint.
success :: Success
success = True

--- Enforce a Boolean condition to be true.
--- The computation fails if the argument evaluates to `False`.
solve :: Bool -> Bool
solve True = True

--- Solves a constraint as an I/O action.
--- Note: The constraint should be always solvable in a deterministic way.
doSolve :: Bool -> IO ()
doSolve b | b = return ()

--- The equational constraint.
--- `(e1 =:= e2)` is satisfiable if both sides `e1` and `e2` can be
--- reduced to a unifiable data term (i.e., a term without defined
--- function symbols).
(=:=) :: Data a => a -> a -> Bool
x =:= y = constrEq x y

constrEq :: a -> a -> Bool
constrEq external

--- Non-strict equational constraint. Used to implement functional patterns.
(=:<=) :: Data a => a -> a -> Bool
x =:<= y = nonstrictEq x y

nonstrictEq :: a -> a -> Bool
nonstrictEq external

#ifdef __PAKCS__
--- Non-strict equational constraint for linear functional patterns.
--- Thus, it must be ensured that the first argument is always (after evalutation
--- by narrowing) a linear pattern. Experimental.
(=:<<=) :: Data a => a -> a -> Bool
x =:<<= y = unifEqLinear x y

unifEqLinear :: a -> a -> Bool
unifEqLinear external

--- internal function to implement =:<=
ifVar :: _ -> a -> a -> a
ifVar external
#endif

--- Concurrent conjunction.
--- An expression like `(c1 & c2)` is evaluated by evaluating
--- the `c1` and `c2` in a concurrent manner.
(&) :: Bool -> Bool -> Bool
(&) external

--- Conditional expression.
--- An expression like `(c &> e)` is evaluated by evaluating the first
--- argument to `True` and then evaluating `e`.
--- The expression has no value if the condition does not evaluate to `True`.
(&>) :: Bool -> a -> a
True &> x = x

--- Non-deterministic choice _par excellence_.
--- The value of `x ? y` is either `x` or `y`.
(?) :: a -> a -> a
x ? _ = x
_ ? y = y

--- Returns non-deterministically any element of a list.
anyOf :: [a] -> a
anyOf = foldr1 (?)

--- Evaluates to a fresh free variable.
unknown :: Data a => a
unknown = let x free in x

--- A non-reducible polymorphic function.
--- It is useful to express a failure in a search branch of the execution.
failed :: _
failed external

--- Aborts the execution with an error message.
error :: String -> _
error x = prim_error $## x

prim_error :: String -> _
prim_error external

-- Representation of higher-order applications in FlatCurry.
apply :: (a -> b) -> a -> b
apply external

-- Representation of conditional rules in FlatCurry.
cond :: Bool -> a -> a
cond external

#ifdef __PAKCS__
-- `letrec ones (1 : ones)` binds `ones` to `1 : ones`.
letrec :: a -> a -> Bool
letrec external

-- Internal operation to implement failure reporting.
failure :: _ -> _ -> _
failure external
#endif
