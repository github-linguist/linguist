dot_product = (ary1, ary2) ->
  if ary1.length != ary2.length
    throw "can't find dot product: arrays have different lengths"
  dotprod = 0
  for v, i in ary1
    dotprod += v * ary2[i]
  dotprod

console.log dot_product([ 1, 3, -5 ], [ 4, -2, -1 ]) # 3
try
  console.log dot_product([ 1, 3, -5 ], [ 4, -2, -1, 0 ]) # exception
catch e
  console.log e
