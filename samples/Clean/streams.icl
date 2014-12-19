implementation module streams

import StdEnv

instance zero [Real]
where
        zero = [] //Infinite row of zeroes represented as empty list to ease computation

instance one [Real]
where
        one = [1.0:zero]

instance + [Real]
where
        (+) [s:s`] [t:t`] = [s+t:s`+t`]
        (+) [s:s`] [] = [s:s`]
        (+) [] [t:t`] = [t:t`]
        (+) [] [] = []
        
instance - [Real]
where
        (-) [s:s`] [t:t`] = [s-t:s`-t`]
        (-) [s:s`] [] = [s:s`]
        (-) [] [t:t`] = [-1.0] * [t:t`]
        (-) [] [] = []

instance * [Real]
where
        (*) [s:s`] [t:t`] = [s*t:s`*[t:t`]+[s]*t`]
        (*) _ _ = []

instance / [Real]
where
        (/) s t = s * (invert t)

X :: [Real]
X = [0.0:one]

invert :: [Real] -> [Real]
invert [s:s`] = [1.0/s:(invert [s:s`]) * s` * [-1.0/s]]

pow :: [Real] Int -> [Real]
pow s 0 = one
pow s n = s * pow s (n-1)

(shuffle) infixl 7 :: [Real] [Real] -> [Real]
(shuffle) [s:s`] [t:t`] = [s*t:s` shuffle [t:t`] + [s:s`] shuffle t`]
(shuffle) _ _ = []

