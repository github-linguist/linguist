partially.apply <- function(f, ...) {
  capture <- list(...)
  function(...) {
    do.call(f, c(capture, list(...)))
  }
}

fs <- function(f, ...) sapply(list(...), f)
f1 <- function(x) 2*x
f2 <- function(x) x^2

fsf1 <- partially.apply(fs, f1)
fsf2 <- partially.apply(fs, f2)

fsf1(0:3)
fsf2(0:3)
fsf1(seq(2,8,2))
fsf2(seq(2,8,2))
