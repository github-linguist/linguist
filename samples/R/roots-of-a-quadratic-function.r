quaddiscrroots <- function(a,b,c, tol=1e-5) {
  d <- b*b - 4*a*c + 0i
  root1 <- (-b + sqrt(d))/(2*a)
  root2 <- (-b - sqrt(d))/(2*a)
  if ( abs(Re(d)) < tol ) {
    list("real and equal", abs(root1), abs(root1))
  } else if ( Re(d) > 0 ) {
    list("real", Re(root1), Re(root2))
  } else {
    list("complex", root1, root2)
  }
}

for(coeffs in list(c(3,4,4/3), c(3,2,-1), c(3,2,1), c(1, -1e6, 1)) ) {
  cat(sprintf("roots of %gx^2 %+gx^1 %+g are\n", coeffs[1], coeffs[2], coeffs[3]))
  r <- quaddiscrroots(coeffs[1], coeffs[2], coeffs[3])
  cat(sprintf("  %s: %s, %s\n", r[[1]], r[[2]], r[[3]]))
}
