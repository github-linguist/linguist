require 'bigdecimal'
require 'bigdecimal/util'  # String#to_d

def safe_add(a, b, prec)
  a, b = a.to_d, b.to_d
  rm = BigDecimal::ROUND_MODE
  orig = BigDecimal.mode(rm)

  BigDecimal.mode(rm, BigDecimal::ROUND_FLOOR)
  low = a.add(b, prec)

  BigDecimal.mode(rm, BigDecimal::ROUND_CEILING)
  high = a.add(b, prec)

  BigDecimal.mode(rm, orig)
  low..high
end

[["1", "2"],
 ["0.1", "0.2"],
 ["0.1", "0.00002"],
 ["0.1", "-0.00002"],
].each { |a, b| puts "#{a} + #{b} = #{safe_add(a, b, 3)}" }
