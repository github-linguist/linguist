def no_args():
    pass
# call
no_args()

def fixed_args(x, y):
    print('x=%r, y=%r' % (x, y))
# call
fixed_args(1, 2)        # x=1, y=2

def opt_args(x=1):
    print(x)
# calls
opt_args()              # 1
opt_args(3.141)         # 3.141

def var_args(*v):
    print(v)
# calls	
var_args(1, 2, 3)       # (1, 2, 3)
var_args(1, (2,3))      # (1, (2, 3))
var_args()              # ()

## Named arguments
fixed_args(y=2, x=1)    # x=1, y=2

## As a statement
if 1:
    no_args()

## First-class within an expression
assert no_args() is None

def return_something():
    return 1
x = return_something()

def is_builtin(x):
	print(x.__name__ in dir(__builtins__))
# calls
is_builtin(pow)         # True
is_builtin(is_builtin)  # False

## A subroutine is merely a function that has no explicit
## return statement and will return None.

## Python uses "Call by Object Reference".
## See, for example, http://www.python-course.eu/passing_arguments.php

## For partial function application see:
##   http://rosettacode.org/wiki/Partial_function_application#Python
