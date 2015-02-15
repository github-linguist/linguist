require 'bigdecimal'

# square root of 2
sqrt2 = Object.new
def sqrt2.a(n); n == 1 ? 1 : 2; end
def sqrt2.b(n); 1; end

# Napier's constant
napier = Object.new
def napier.a(n); n == 1 ? 2 : n - 1; end
def napier.b(n); n == 1 ? 1 : n - 1; end

pi = Object.new
def pi.a(n); n == 1 ? 3 : 6; end
def pi.b(n); (2*n - 1)**2; end

# Estimates the value of a continued fraction _cfrac_, to _prec_
# decimal digits of precision. Returns a BigDecimal. _cfrac_ must
# respond to _cfrac.a(n)_ and _cfrac.b(n)_ for integer _n_ >= 1.
def estimate(cfrac, prec)
  last_result = nil
  terms = prec

  loop do
    # Estimate continued fraction for _n_ from 1 to _terms_.
    result = cfrac.a(terms)
    (terms - 1).downto(1) do |n|
      a = BigDecimal cfrac.a(n)
      b = BigDecimal cfrac.b(n)
      digits = [b.div(result, 1).exponent + prec, 1].max
      result = a + b.div(result, digits)
    end
    result = result.round(prec)

    if result == last_result
      return result
    else
      # Double _terms_ and try again.
      last_result = result
      terms *= 2
    end
  end
end

puts estimate(sqrt2, 50).to_s('F')
puts estimate(napier, 50).to_s('F')
puts estimate(pi, 10).to_s('F')
