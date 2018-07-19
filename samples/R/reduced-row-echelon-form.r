rref <- function(m) {
  pivot <- 1
  norow <- nrow(m)
  nocolumn <- ncol(m)
  for(r in 1:norow) {
    if ( nocolumn <= pivot ) break;
    i <- r
    while( m[i,pivot] == 0 ) {
      i <- i + 1
      if ( norow == i ) {
        i <- r
        pivot <- pivot + 1
        if ( nocolumn == pivot ) return(m)
      }
    }
    trow <- m[i, ]
    m[i, ] <- m[r, ]
    m[r, ] <- trow
    m[r, ] <- m[r, ] / m[r, pivot]
    for(i in 1:norow) {
      if ( i != r )
        m[i, ] <- m[i, ] - m[r, ] * m[i, pivot]
    }
    pivot <- pivot + 1
  }
  return(m)
}

m <- matrix(c(1, 2, -1, -4,
              2, 3, -1, -11,
              -2, 0, -3, 22), 3, 4, byrow=TRUE)
print(m)
print(rref(m))
