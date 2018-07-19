gcd = (x, y) ->
  return x if y == 0
  gcd(y, x % y)

# m,n generate primitive Pythag triples
#
# preconditions:
#   m, n are integers of different parity
#   m > n
#   gcd(m,n) == 1 (coprime)
#
# m, n generate: [m*m - n*n, 2*m*n, m*m + n*n]
# perimeter is 2*m*m + 2*m*n = 2 * m * (m+n)
count_triples = (max_perim) ->
  num_primitives = 0
  num_triples = 0
  m = 2
  upper_limit = Math.sqrt max_perim / 2
  while m <= upper_limit
    n = m % 2 + 1
    p = 2*m*m + 2*m*n
    delta = 4*m
    while n < m and p <= max_perim
      if gcd(m, n) == 1
        num_primitives += 1
        num_triples += Math.floor max_perim / p
      n += 2
      p += delta
    m += 1
  console.log num_primitives, num_triples

max_perim = Math.pow 10, 9 # takes under a minute
count_triples(max_perim)
