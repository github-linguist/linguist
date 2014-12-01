definition module streams

import StdEnv

instance zero [Real]
instance one [Real]
instance + [Real]        
instance - [Real]
instance * [Real]
instance / [Real]

X :: [Real]
invert :: [Real] -> [Real]
pow :: [Real] Int -> [Real]
(shuffle) infixl 7 :: [Real] [Real] -> [Real]

