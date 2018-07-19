eval_poly = (x, coefficients) ->
  # coefficients are for ascending powers
  return 0 if coefficients.length == 0
  ones_place = coefficients.shift()
  x * eval_poly(x, coefficients) + ones_place

console.log eval_poly 3, [-19, 7, -4, 6] # 128
console.log eval_poly 10, [4, 3, 2, 1] # 1234
console.log eval_poly 2, [1, 1, 0, 0, 1] # 19
