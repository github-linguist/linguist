decimal_to_roman = (n) ->
  # This should work for any positive integer, although it
  # gets a bit preposterous for large numbers.
  if n >= 4000
    thousands = decimal_to_roman n / 1000
    ones = decimal_to_roman n % 1000
    return "M(#{thousands})#{ones}"

  s = ''
  translate_each = (min, roman) ->
    while n >= min
      n -= min
      s += roman
  translate_each 1000, "M"
  translate_each  900, "CM"
  translate_each  500, "D"
  translate_each  400, "CD"
  translate_each  100, "C"
  translate_each   90, "XC"
  translate_each   50, "L"
  translate_each   40, "XL"
  translate_each   10, "X"
  translate_each    9, "IX"
  translate_each    5, "V"
  translate_each    4, "IV"
  translate_each    1, "I"
  s

###################
tests =
  IV: 4
  XLII: 42
  MCMXC: 1990
  MMVIII: 2008
  MDCLXVI: 1666
  'M(IV)': 4000
  'M(VI)IX': 6009
  'M(M(CXXIII)CDLVI)DCCLXXXIX': 123456789
  'M(MMMV)I': 3005001

for expected, decimal of tests
  roman = decimal_to_roman(decimal)
  if roman == expected
    console.log "#{decimal} = #{roman}"
  else
    console.log "error for #{decimal}: #{roman} is wrong"
