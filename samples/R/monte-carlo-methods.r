# nice but not suitable for big samples!
monteCarloPi <- function(samples) {
  x <- runif(samples, -1, 1) # for big samples, you need a lot of memory!
  y <- runif(samples, -1, 1)
  l <- sqrt(x*x + y*y)
  return(4*sum(l<=1)/samples)
}

# this second function changes the samples number to be
# multiple of group parameter (default 100).
monteCarlo2Pi <- function(samples, group=100) {
  lim <- ceiling(samples/group)
  olim <- lim
  c <- 0
  while(lim > 0) {
    x <- runif(group, -1, 1)
    y <- runif(group, -1, 1)
    l <- sqrt(x*x + y*y)
    c <- c + sum(l <= 1)
    lim <- lim - 1
  }
  return(4*c/(olim*group))
}

print(monteCarloPi(1e4))
print(monteCarloPi(1e5))
print(monteCarlo2Pi(1e7))
