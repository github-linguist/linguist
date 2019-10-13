# Return an array of primes. This is a trivial and extremely naive algorithm.

(defn primes
  "Returns a list of prime numbers less than n."
  [n]
  (def list @[])
  (for i 2 n
    (var isprime? true)
    (def len (length list))
    (for j 0 len
      (def trial (get list j))
      (if (zero? (% i trial)) (set isprime? false)))
    (if isprime? (array/push list i)))
  list)

(pp (primes 100))
