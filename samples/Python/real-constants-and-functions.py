import math

math.e          # e
math.pi         # pi
math.sqrt(x)    # square root  (Also commonly seen as x ** 0.5 to obviate importing the math module)
math.log(x)     # natural logarithm
math.log10(x)   # base 10 logarithm
math.exp(x)     # e raised to the power of x
abs(x)          # absolute value
math.floor(x)   # floor
math.ceil(x)    # ceiling
x ** y          # exponentiation
pow(x, y[, n])  # exponentiation [, modulo n (useful in certain encryption/decryption algorithms)]

# The math module constants and functions can, of course, be imported directly by:
#   from math import e, pi, sqrt, log, log10, exp, floor, ceil
