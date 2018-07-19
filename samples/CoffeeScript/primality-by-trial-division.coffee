is_prime = (n) ->
  # simple prime detection using trial division, works
  # for all integers
  return false if n <= 1 # by definition
  p = 2
  while p * p <= n
    return false if n % p == 0
    p += 1
  true

for i in [-1..100]
  console.log i if is_prime i
