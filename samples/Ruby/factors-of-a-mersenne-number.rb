require 'prime'

def mersenne_factor(p)
  limit = Math.sqrt(2**p - 1)
  k = 1
  while (2*k*p - 1) < limit
    q = 2*k*p + 1
    if q.prime? and (q % 8 == 1 or q % 8 == 7) and trial_factor(2,p,q)
      # q is a factor of 2**p-1
      return q
    end
    k += 1
  end
  nil
end

def trial_factor(base, exp, mod)
  square = 1
  ("%b" % exp).each_char {|bit| square = square**2 * (bit == "1" ? base : 1) % mod}
  (square == 1)
end

def check_mersenne(p)
  print "M#{p} = 2**#{p}-1 is "
  f = mersenne_factor(p)
  if f.nil?
    puts "prime"
  else
    puts "composite with factor #{f}"
  end
end

Prime.each(53) { |p| check_mersenne p }
check_mersenne 929
