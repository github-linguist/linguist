# ATOMIC GATES
not_gate = (bit) ->
  [1, 0][bit]

and_gate = (bit1, bit2) ->
  bit1 and bit2

or_gate = (bit1, bit2) ->
  bit1 or bit2

# COMPOSED GATES
xor_gate = (A, B) ->
  X = and_gate A, not_gate(B)
  Y = and_gate not_gate(A), B
  or_gate X, Y

half_adder = (A, B) ->
  S = xor_gate A, B
  C = and_gate A, B
  [S, C]

full_adder = (C0, A, B) ->
  [SA, CA] = half_adder C0, A
  [SB, CB] = half_adder SA, B
  S = SB
  C = or_gate CA, CB
  [S, C]

n_bit_adder = (n) ->
  (A_bits, B_bits) ->
    s = []
    C = 0
    for i in [0...n]
      [S, C] = full_adder C, A_bits[i], B_bits[i]
      s.push S
    [s, C]

adder = n_bit_adder(4)
console.log adder [1, 0, 1, 0], [0, 1, 1, 0]
