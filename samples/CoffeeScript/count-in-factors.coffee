count_primes = (max) ->
  # Count through the natural numbers and give their prime
  # factorization.  This algorithm uses no division.
  # Instead, each prime number starts a rolling odometer
  # to help subsequent factorizations.  The algorithm works similar
  # to the Sieve of Eratosthenes, as we note when each prime number's
  # odometer rolls a digit.  (As it turns out, as long as your computer
  # is not horribly slow at division, you're better off just doing simple
  # prime factorizations on each new n vs. using this algorithm.)
  console.log "1 = 1"
  primes = []
  n = 2
  while n <= max
    factors = []
    for prime_odometer in primes
      # digits are an array w/least significant digit in
      # position 0;  for example, [3, [0]] will roll as
      # follows:
      #    [0] -> [1] -> [2] -> [0, 1]
      [base, digits] = prime_odometer
      i = 0
      while true
        digits[i] += 1
        break if digits[i] < base
        digits[i] = 0
        factors.push base
        i += 1
        if i >= digits.length
          digits.push 0

    if factors.length == 0
      primes.push [n, [0, 1]]
      factors.push n
    console.log "#{n} = #{factors.join('*')}"
    n += 1

  primes.length

num_primes = count_primes 10000
console.log num_primes
