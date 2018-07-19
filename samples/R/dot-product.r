x <- c(1, 3, -5)
y <- c(4, -2, -1)

sum(x*y)  # compute products, then do the sum
x %*% y   # inner product

# loop implementation
dotp <- function(x, y) {
	n <- length(x)
	if(length(y) != n) stop("invalid argument")
	s <- 0
	for(i in 1:n) s <- s + x[i]*y[i]
	s
}

dotp(x, y)
