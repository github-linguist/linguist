f <- function(f0) f0(pi) # calc. the function in pi
tf <- function(x) x^pi   # a func. just to test

print(f(sin))
print(f(cos))
print(f(tf))
