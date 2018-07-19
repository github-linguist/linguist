roman_to_demical = (s) ->
  # s is well-formed Roman Numeral >= I
  numbers =
    M: 1000
    D: 500
    C: 100
    L: 50
    X: 10
    V: 5
    I: 1

  result = 0
  for c in s
    num = numbers[c]
    result += num
    if old_num < num
      # If old_num exists and is less than num, then
      # we need to subtract it twice, once because we
      # have already added it on the last pass, and twice
      # to conform to the Roman convention that XC = 90,
      # not 110.
      result -= 2 * old_num
    old_num = num
  result

tests =
  IV: 4
  XLII: 42
  MCMXC: 1990
  MMVIII: 2008
  MDCLXVI: 1666

for roman, expected of tests
  dec = roman_to_demical(roman)
  console.log "error" if dec != expected
  console.log "#{roman} = #{dec}"
