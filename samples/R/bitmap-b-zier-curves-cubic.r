# x, y: the x and y coordinates of the hull points
# n: the number of points in the curve.
bezierCurve <- function(x, y, n=10)
	{
	outx <- NULL
	outy <- NULL

	i <- 1
	for (t in seq(0, 1, length.out=n))
		{
		b <- bez(x, y, t)
		outx[i] <- b$x
		outy[i] <- b$y

		i <- i+1
		}

	return (list(x=outx, y=outy))
	}

bez <- function(x, y, t)
	{
	outx <- 0
	outy <- 0
	n <- length(x)-1
	for (i in 0:n)
		{
		outx <- outx + choose(n, i)*((1-t)^(n-i))*t^i*x[i+1]
		outy <- outy + choose(n, i)*((1-t)^(n-i))*t^i*y[i+1]
		}

	return (list(x=outx, y=outy))
	}

# Example usage
x <- c(4,6,4,5,6,7)
y <- 1:6
plot(x, y, "o", pch=20)
points(bezierCurve(x,y,20), type="l", col="red")
