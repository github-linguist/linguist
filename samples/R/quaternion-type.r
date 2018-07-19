library(quaternions)

q <- Q(1, 2, 3, 4)
q1 <- Q(2, 3, 4, 5)
q2 <- Q(3, 4, 5, 6)
r <- 7.0

display <- function(x){
  e <- deparse(substitute(x))
  res <- if(class(x) == "Q") paste(x$r, "+", x$i, "i+", x$j, "j+", x$k, "k", sep = "") else x
  cat(noquote(paste(c(e, " = ", res, "\n"), collapse="")))
  invisible(res)
}

display(norm(q))
display(-q)
display(Conj(q))
display(r + q)
display(q1 + q2)
display(r*q)
display(q*r)
if(display(q1*q2) == display(q2*q1)) cat("q1*q2 == q2*q1\n") else cat("q1*q2 != q2*q1\n")

## norm(q) = 5.47722557505166
## -q = -1+-2i+-3j+-4k
## Conj(q) = 1+-2i+-3j+-4k
## r + q = 8+2i+3j+4k
## q1 + q2 = 5+7i+9j+11k
## r * q = 7+14i+21j+28k
## q * r = 7+14i+21j+28k
## q1 * q2 = -56+16i+24j+26k
## q2 * q1 = -56+18i+20j+28k
## q1*q2 != q2*q1
