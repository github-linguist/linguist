# Generate Charmichael Numbers
#
#  Nigel_Galloway
#  November 30th., 2012.
#
require 'prime'

Integer.each_prime(61) {|p|
  (2...p).each {|h3|
    g = h3 + p
    (1...g).each {|d|
      next if (g*(p-1)) % d != 0 or (-1*p*p) % h3 != d % h3
      q = 1 + ((p - 1) * g / d)
      next if not q.prime?
      r = 1 + (p * q / h3)
      next if not r.prime? or not (q * r) % (p - 1) == 1
      puts "#{p} X #{q} X #{r}"
    }
  }
  puts ""
}
