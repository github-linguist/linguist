#
# The flt package (http://flt.rubyforge.org/) is useful for high-precision floating-point math.
# It lets us control 'context' of numbers, individually or collectively -- including precision
# (which adjusts the context's value of epsilon accordingly).

require 'flt'
include Flt

BinNum.Context.precision = 512  # default 53 (bits)

def AGM(a,g)
  new_a = BinNum a
  new_g = BinNum g
  while new_a - new_g > new_a.class.Context.epsilon do
    old_g = new_g
    new_g = (new_a * new_g).sqrt
    new_a = (old_g + new_a) * 0.5
  end
  new_g
end

puts AGM 1, 1 / BinNum(2).sqrt
