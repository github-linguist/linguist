# returns pair [sum, carry]
def four_bit_adder(a, b)
  a_bits = binary_string_to_bits(a,4)
  b_bits = binary_string_to_bits(b,4)

  s0, c0 = full_adder(a_bits[0], b_bits[0],  0)
  s1, c1 = full_adder(a_bits[1], b_bits[1], c0)
  s2, c2 = full_adder(a_bits[2], b_bits[2], c1)
  s3, c3 = full_adder(a_bits[3], b_bits[3], c2)

  [bits_to_binary_string([s0, s1, s2, s3]), c3.to_s]
end

# returns pair [sum, carry]
def full_adder(a, b, c0)
  s, c = half_adder(c0, a)
  s, c1 = half_adder(s, b)
  [s, _or(c,c1)]
end

# returns pair [sum, carry]
def half_adder(a, b)
  [xor(a, b), _and(a,b)]
end

def xor(a, b)
  _or(_and(a, _not(b)), _and(_not(a), b))
end

# "and", "or" and "not" are Ruby keywords
def _and(a, b); a & b; end
def _or(a, b);  a | b; end
def _not(a);   ~a & 1; end

def int_to_binary_string(n, length)
  ("0"*length + n.to_s(2))[-length .. -1]
end
def binary_string_to_bits(s, length)
  (s.reverse + "0"*length)[0..length-1].chars.map(&:to_i)
end
def bits_to_binary_string(bits)
  bits.map(&:to_s).reverse.join("")
end

puts " A    B      A      B   C    S  sum"
0.upto(15) do |a|
  0.upto(15) do |b|
    bin_a = int_to_binary_string(a, 4)
    bin_b = int_to_binary_string(b, 4)
    sum, carry = four_bit_adder(bin_a, bin_b)
    puts "%2d + %2d = %s + %s = %s %s = %2d" % [
      a, b, bin_a, bin_b, carry, sum, (carry + sum).to_i(2)
    ]
  end
end
