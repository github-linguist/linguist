# recursive
recfibo <- function(n) {
  if ( n < 2 ) n
  else Recall(n-1) + Recall(n-2)
}

# print the first 21 elements
print.table(lapply(0:20, recfibo))

# iterative
iterfibo <- function(n) {
  if ( n < 2 )
    n
  else {
    f <- c(0, 1)
    for (i in 2:n) {
      t <- f[2]
      f[2] <- sum(f)
      f[1] <- t
    }
    f[2]
  }
}

print.table(lapply(0:20, iterfibo))

# iterative but looping replaced by map-reduce'ing
funcfibo <- function(n) {
  if (n < 2)
    n
  else {
    generator <- function(f, ...) {
      c(f[2], sum(f))
    }
    Reduce(generator, 2:n, c(0,1))[2]
  }
}

print.table(lapply(0:20, funcfibo))
