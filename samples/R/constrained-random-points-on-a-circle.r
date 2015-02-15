RMin <- 10
RMax <- 15
NPts <- 100

# instead of a for loop, we generate what should be enough points
# also take care to have enough range to avoid rounding inaccuracies
nBlock <- NPts * ((RMax/RMin) ^ 2)
nValid <- 0
while (nValid < NPts) {
	X <- round(runif(nBlock, -RMax - 1, RMax + 1))
	Y <- round(runif(nBlock, -RMax - 1, RMax + 1))
	R <-  sqrt(X^2 + Y^2)
	Valid <- ( (R >= RMin) & (R <= RMax) )
	nValid <- sum(Valid)
	nBlock <- 2 * nBlock
}
plot(X[Valid][1:NPts],Y[Valid][1:NPts], pch=19, cex=0.25, col="blue",
	xlab="x",ylab="y",main="Fuzzy circle", xlim=c(-RMax,RMax), ylim=c(-RMax,RMax) )
