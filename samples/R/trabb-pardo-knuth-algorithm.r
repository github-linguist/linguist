S <- scan(n=11)

f <- function(x) sqrt(abs(x)) + 5*x^3

for (i in rev(S)) {
  res <- f(i)
  if (res > 400)
    print("Too large!")
  else
    print(res)
}
