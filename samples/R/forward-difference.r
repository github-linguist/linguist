forwarddif <- function(a, n) {
  if ( n == 1 )
    a[2:length(a)] - a[1:length(a)-1]
  else {
    r <- forwarddif(a, 1)
    forwarddif(r, n-1)
  }
}

fdiff <- function(a, n) {
  r <- a
  for(i in 1:n) {
    r <- r[2:length(r)] - r[1:length(r)-1]
  }
  r
}

v <- c(90, 47, 58, 29, 22, 32, 55, 5, 55, 73)

print(forwarddif(v, 9))
print(fdiff(v, 9))
