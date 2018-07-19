is_perfect_number = (n) ->
  do_factors_add_up_to n, 2*n

do_factors_add_up_to = (n, desired_sum) ->
  # We mildly optimize here, by taking advantage of
  # the fact that the sum_of_factors( (p^m) * x)
  # is (1 + ... + p^m-1 + p^m) * sum_factors(x) when
  # x is not itself a multiple of p.

  p = smallest_prime_factor(n)
  if p == n
    return desired_sum == p + 1

  # ok, now sum up all powers of p that
  # divide n
  sum_powers = 1
  curr_power = 1
  while n % p == 0
    curr_power *= p
    sum_powers += curr_power
    n /= p

  # if desired_sum does not divide sum_powers, we
  # can short circuit quickly
  return false unless desired_sum % sum_powers == 0

  # otherwise, recurse
  do_factors_add_up_to n, desired_sum / sum_powers

smallest_prime_factor = (n) ->
  for i in [2..n]
    return n if i*i > n
    return i if n % i == 0

# tests
do ->
  # This is pretty fast...
  for n in [2..100000]
    console.log n if is_perfect_number n

  # For big numbers, let's just sanity check the known ones.
  known_perfects = [
    33550336
    8589869056
    137438691328
  ]
  for n in known_perfects
    throw Error("fail") unless is_perfect_number(n)
    throw Error("fail") if is_perfect_number(n+1)
