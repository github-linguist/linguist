import Data.List (intercalate, unfoldr)

spellInteger :: Integer -> String
spellInteger n
 | n <    0  = "negative " ++ spellInteger (-n)
 | n <   20  = small n
 | n <  100  = let (a, b) = n `divMod` 10
               in  tens a ++ nonzero '-' b
 | n < 1000  = let (a, b) = n `divMod` 100
               in  small a ++ " hundred" ++ nonzero ' ' b
 | otherwise = intercalate ", " $ map big $ reverse $
               filter ((/= 0) . snd) $ zip [0..] $ unfoldr uff n

 where nonzero :: Char -> Integer -> String
       nonzero _ 0 = ""
       nonzero c n = c : spellInteger n

       uff :: Integer -> Maybe (Integer, Integer)
       uff 0 = Nothing
       uff n = Just $ uncurry (flip (,)) $ n `divMod` 1000

       small, tens :: Integer -> String
       small = (["zero", "one", "two", "three", "four", "five",
            "six", "seven", "eight", "nine", "ten", "eleven",
            "twelve", "thirteen", "fourteen", "fifteen", "sixteen",
            "seventeen", "eighteen", "nineteen"] !!) . fromEnum
       tens = ([undefined, undefined, "twenty", "thirty", "forty",
           "fifty", "sixty", "seventy", "eighty", "ninety"] !!) .
           fromEnum

       big :: (Int, Integer) -> String
       big (0, n) = spellInteger n
       big (1, n) = spellInteger n ++ " thousand"
       big (e, n) = spellInteger n ++ ' ' : (l !! e) ++ "illion"
         where l = [undefined, undefined, "m", "b", "tr", "quadr",
                   "quint", "sext", "sept", "oct", "non", "dec"]
