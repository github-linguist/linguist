halve = (n) -> Math.floor n / 2
double = (n) -> n * 2
is_even = (n) -> n % 2 == 0

multiply = (a, b) ->
  prod = 0
  while a > 0
    prod += b if !is_even a
    a = halve a
    b = double b
  prod

# tests
do ->
  for i in [0..100]
    for j in [0..100]
      throw Error("broken for #{i} * #{j}") if multiply(i,j) != i * j
