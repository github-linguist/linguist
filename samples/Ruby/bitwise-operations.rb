def bitwise(a, b)
  form = "%1$7s:%2$6d  %2$016b"
  puts form % ["a", a]
  puts form % ["b", b]
  puts form % ["a and b", a & b]
  puts form % ["a or b ", a | b]
  puts form % ["a xor b", a ^ b]
  puts form % ["not a  ", ~a]
  puts form % ["a << b ", a << b]  # left shift
  puts form % ["a >> b ", a >> b]  # arithmetic right shift
end

bitwise(14,3)
