happy = (n) ->
  seen = {}
  while true
    n = sum_digit_squares(n)
    return true if n == 1
    return false if seen[n]
    seen[n] = true

sum_digit_squares = (n) ->
  sum = 0
  for c in n.toString()
    d = parseInt(c)
    sum += d*d
  sum

i = 1
cnt = 0
while cnt < 8
  if happy(i)
    console.log i
    cnt += 1
  i += 1
