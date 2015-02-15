is.even <- function(x) !is.odd(x)

is.odd <- function(x) intToBits(x)[1] == 1
#or
is.odd <- function(x) x %% 2 == 1
