horner <- function(a, x) {
  iv <- 0
  for(i in length(a):1) {
    iv <- iv * x + a[i]
  }
  iv
}

cat(horner(c(-19, 7, -4, 6), 3), "\n")
