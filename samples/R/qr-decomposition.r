# R has QR decomposition built-in (using LAPACK or LINPACK)

a <- matrix(c(12, -51, 4, 6, 167, -68, -4, 24, -41), nrow=3, ncol=3, byrow=T)
d <- qr(a)
qr.Q(d)
qr.R(d)

# now fitting a polynomial
x <- 0:10
y <- 3*x^2 + 2*x + 1

# using QR decomposition directly
a <- cbind(1, x, x^2)
qr.coef(qr(a), y)

# using least squares
a <- cbind(x, x^2)
lsfit(a, y)$coefficients

# using a linear model
xx <- x*x
m <- lm(y ~ x + xx)
coef(m)
