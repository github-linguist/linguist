 Inf                    #positive infinity
 -Inf                   #negative infinity
 .Machine$double.xmax   # largest finite floating-point number
 is.finite              # function to test to see if a number is finite

# function that returns the input if it is finite, otherwise returns (plus or minus) the largest finite floating-point number
 forcefinite <- function(x) ifelse(is.finite(x), x, sign(x)*.Machine$double.xmax)

 forcefinite(c(1, -1, 0, .Machine$double.xmax, -.Machine$double.xmax, Inf, -Inf))
# [1]   1.000000e+00  -1.000000e+00   0.000000e+00  1.797693e+308
# [5] -1.797693e+308  1.797693e+308 -1.797693e+308
