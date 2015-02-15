fib2 <- function(n) {
  (n >= 0) || stop("bad argument")
  ( function(n) if (n <= 1) 1 else Recall(n-1)+Recall(n-2) )(n)
}
