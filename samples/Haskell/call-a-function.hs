-- Calling a function with a fixed number of arguments
multiply x y = x * y
multiply 10 20 -- returns 200

-- Calling a function that requires no arguments
-- Normally, you use constant instead of function without arguments:
twopi = 6.28
-- But you can also pass special value as the first argument indicating function call:
twopi () = 6.28 -- definition
twopi :: Num a => () -> a -- its type
twopi () -- returns 6.28

-- Partial application and auto-currying is built-in.
multiply_by_10 = (10 * )
map multiply_by_10 [1, 2, 3] -- [10, 20, 30]
multiply_all_by_10 = map multiply_by_10
multiply_all_by_10 [1, 2, 3] -- [10, 20, 30]

-- TODO:
-- Calling a function with optional arguments
-- Calling a function with a variable number of arguments
-- Calling a function with named arguments
-- Using a function in statement context
-- Using a function in first-class context within an expression
-- Obtaining the return value of a function
-- Distinguishing built-in functions and user-defined functions
-- Distinguishing subroutines and functions
-- Stating whether arguments are passed by value or by reference
