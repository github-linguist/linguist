def is_prime ( p )
  return true  if p == 2
  return false if p <= 1 || p.even?
  (3 .. Math.sqrt(p)).step(2) do |i|
    return false  if p % i == 0
  end
  true
end

def is_mersenne_prime ( p )
  return true  if p == 2
  m_p = ( 1 << p ) - 1
  s = 4
  (p-2).times { s = (s ** 2 - 2) % m_p }
  s == 0
end

precision = 20000   # maximum requested number of decimal places of 2 ** MP-1 #
long_bits_width = precision / Math.log(2) * Math.log(10)
upb_prime = (long_bits_width - 1).to_i / 2    # no unsigned #
upb_count = 45      # find 45 mprimes if int was given enough bits #

puts " Finding Mersenne primes in M[2..%d]:" % upb_prime

count = 0
for p in 2..upb_prime
  if is_prime(p) && is_mersenne_prime(p)
    print "M%d " % p
    count += 1
  end
  break  if count >= upb_count
end
puts
