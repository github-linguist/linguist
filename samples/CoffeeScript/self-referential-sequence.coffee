sequence = (n) ->
  cnts = {}
  for c in n.toString()
    d = parseInt(c)
    incr cnts, d

  seq = []
  while true
    s = ''
    for i in [9..0]
      s += "#{cnts[i]}#{i}" if cnts[i]
    if s in seq
      break
    seq.push s

    new_cnts = {}
    for digit, cnt of cnts
      incr new_cnts, cnt
      incr new_cnts, digit
    cnts = new_cnts
  seq

incr = (h, k) ->
  h[k] ?= 0
  h[k] += 1

descending = (n) ->
  return true if n < 10
  tens = n / 10
  return false if n % 10 > tens % 10
  descending(tens)

max_len = 0
for i in [1..1000000]
  if descending(i)
    seq = sequence(i)
    if seq.length > max_len
      max_len = seq.length
      max_seq = seq
      max_i = i

console.log max_i, max_seq
