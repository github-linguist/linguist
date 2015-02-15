f <- function(x) x^3 -3*x^2 + 2*x

findroots <- function(f, begin, end, tol = 1e-20, step = 0.001) {
  se <- ifelse(sign(f(begin))==0, 1, sign(f(begin)))
  x <- begin
  while ( x <= end ) {
    v <- f(x)
    if ( abs(v) < tol ) {
      print(sprintf("root at %f", x))
    } else if ( ifelse(sign(v)==0, 1, sign(v)) != se )  {
      print(sprintf("root near %f", x))
    }
    se <- ifelse( sign(v) == 0 , 1, sign(v))
    x <- x + step
  }
}

findroots(f, -1, 3)
