import Data.Complex

type CD = Complex Double

quadraticRoots :: (CD, CD, CD) -> (CD, CD)
quadraticRoots (a, b, c) =
    if   realPart b > 0
    then ((2*c) / (-b - d), (-b - d) / (2*a))
    else ((-b + d) / (2*a), (2*c) / (-b + d))
  where d = sqrt $ b^2 - 4*a*c
