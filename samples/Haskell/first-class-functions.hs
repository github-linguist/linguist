Prelude> let cube x = x ^ 3
Prelude> let croot x = x ** (1/3)
Prelude> let compose f g = \x -> f (g x) -- this is already implemented in Haskell as the "." operator
Prelude>                                 -- we could have written "let compose f g x = f (g x)" but we show this for clarity
Prelude> let funclist = [sin, cos, cube]
Prelude> let funclisti = [asin, acos, croot]
Prelude> zipWith (\f inversef -> (compose inversef f) 0.5) funclist funclisti
[0.5,0.4999999999999999,0.5]
