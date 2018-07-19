require 'rational' # for lcm
require 'mathn'    # for prime_division

def powerMod(b, p, m)
  result = 1
  bits = p.to_s(2)
  for bit in bits.split('')
    result = (result * result) % m
    if bit == '1'
      result = (result * b) % m
    end
  end
  result
end

def multOrder_(a, p, k)
  pk = p ** k
  t = (p - 1) * p ** (k - 1)
  r = 1
  for q, e in t.prime_division
    x = powerMod(a, t / q ** e, pk)
    while x != 1
      r *= q
      x = powerMod(x, q, pk)
    end
  end
  r
end

def multOrder(a, m)
  m.prime_division.inject(1) {|result, f|
    result.lcm(multOrder_(a, *f))
  }
end

puts multOrder(37, 1000)        # 100
b = 10**20-1
puts multOrder(2, b) # 3748806900
puts multOrder(17,b) # 1499522760
b = 100001
puts multOrder(54,b)
puts powerMod(54, multOrder(54,b), b)
if (1...multOrder(54,b)).any? {|r| powerMod(54, r, b) == 1}
  puts 'Exists a power r < 9090 where powerMod(54,r,b)==1'
else
  puts 'Everything checks.'
end
