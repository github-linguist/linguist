polylongdiv <- function(n,d) {
  gd <- length(d)
  pv <- vector("numeric", length(n))
  pv[1:gd] <- d
  if ( length(n) >= gd ) {
    q <- c()
    while ( length(n) >= gd ) {
      q <- c(q, n[1]/pv[1])
      n <- n - pv * (n[1]/pv[1])
      n <- n[2:length(n)]
      pv <- pv[1:(length(pv)-1)]
    }
    list(q=q, r=n)
  } else {
    list(q=c(0), r=n)
  }
}

# an utility function to print polynomial
print.polynomial <- function(p) {
  i <- length(p)-1
  for(a in p) {
    if ( i == 0 ) {
      cat(a, "\n")
    } else {
      cat(a, "x^", i, " + ", sep="")
    }
    i <- i - 1
  }
}

r <- polylongdiv(c(1,-12,0,-42), c(1,-3))
print.polynomial(r$q)
print.polynomial(r$r)
