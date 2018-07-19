#based on pseudo code from http://en.wikipedia.org/wiki/Extended_Euclidean_algorithm#Iterative_method_2 and from translating the python implementation.
def extended_gcd(a, b)
  last_remainder, remainder = a.abs, b.abs
  x, last_x, y, last_y = 0, 1, 1, 0
  while remainder != 0
    last_remainder, (quotient, remainder) = remainder, last_remainder.divmod(remainder)
    x, last_x = last_x - quotient*x, x
    y, last_y = last_y - quotient*y, y
  end

  return last_remainder, last_x * (a < 0 ? -1 : 1)
end

def invmod(e, et)
  g, x = extended_gcd(e, et)
  if g != 1
    raise 'Teh maths are broken!'
  end
  x % et
end
