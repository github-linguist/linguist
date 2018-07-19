collatz = method(n,
  n println
  unless(n <= 1,
    if(n even?, collatz(n / 2), collatz(n * 3 + 1)))
)
