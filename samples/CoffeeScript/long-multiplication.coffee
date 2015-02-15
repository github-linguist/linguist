# This very limited BCD-based collection of functions
# allows for long multiplication.  It works for positive
# numbers only.  The assumed data structure is as follows:
# BcdInteger.from_integer(4321) == [1, 2, 3, 4]

BcdInteger =
  from_string: (s) ->
    arr = []
    for c in s
      arr.unshift parseInt(c)
    arr

  from_integer: (n) ->
    result = []
    while n > 0
      result.push n % 10
      n = Math.floor n / 10
    result

  to_string: (arr) ->
    s = ''
    for elem in arr
      s = elem.toString() + s
    s

  sum: (arr1, arr2) ->
    if arr1.length < arr2.length
      return BcdInteger.sum(arr2, arr1)
    carry = 0
    result= []
    for d1, pos in arr1
      d = d1 + (arr2[pos] || 0) + carry
      result.push d % 10
      carry = Math.floor d / 10
    if carry
      result.push 1
    result

  multiply_by_power_of_ten: (arr, power_of_ten) ->
    result = (0 for i in [0...power_of_ten])
    result.concat arr

  product_by_integer: (arr, n) ->
    result = []
    for digit, i in arr
      prod = BcdInteger.from_integer n * digit
      prod = BcdInteger.multiply_by_power_of_ten prod, i
      result = BcdInteger.sum result, prod
    result

  product: (arr1, arr2) ->
    result = []
    for digit, i in arr1
      prod = BcdInteger.product_by_integer arr2, digit
      prod = BcdInteger.multiply_by_power_of_ten prod, i
      result = BcdInteger.sum result, prod
    result

x = BcdInteger.from_integer 1
for i in [1..64]
  x = BcdInteger.product_by_integer x, 2
console.log BcdInteger.to_string x #   18446744073709551616
square = BcdInteger.product x, x
console.log BcdInteger.to_string square # 340282366920938463463374607431768211456
