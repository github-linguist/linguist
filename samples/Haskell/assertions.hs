import Control.Exception

main = let a = someValue in
         assert (a == 42) -- throws AssertionFailed when a is not 42
                somethingElse -- what to return when a is 42
