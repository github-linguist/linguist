"%gcd%" <- function(u, v) {ifelse(u %% v != 0, v %gcd% (u%%v), v)}

"%lcm%" <- function(u, v) { abs(u*v)/(u %gcd% v)}

print (50 %lcm% 75)
