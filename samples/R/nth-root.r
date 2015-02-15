nthroot <- function(A, n, tol=sqrt(.Machine$double.eps))
{
   ifelse(A < 1, x0 <- A * n, x0 <- A / n)
   repeat
   {
      x1 <- ((n-1)*x0 + A / x0^(n-1))/n
      if(abs(x1 - x0) > tol) x0 <- x1 else break
   }
   x1
}
nthroot(7131.5^10, 10)   # 7131.5
nthroot(7, 0.5)          # 49
