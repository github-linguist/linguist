gray_encode = (n) ->
  n ^ (n >> 1)

gray_decode = (g) ->
  n = g
  n ^= g while g >>= 1
  n

for i in [0..32]
  console.log gray_decode gray_encode(i)
