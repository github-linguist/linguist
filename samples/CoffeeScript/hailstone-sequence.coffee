hailstone = (n) ->
  if n is 1
    [n]

  else if n % 2 is 0
    [n].concat hailstone n/2

  else
    [n].concat hailstone (3*n) + 1

h27 = hailstone 27
console.log "hailstone(27) = #{h27[0..3]} ... #{h27[-4..]} (length: #{h27.length})"

maxlength = 0
maxnums = []

for i in [1..100000]
  seq = hailstone i

  if seq.length is maxlength
    maxnums.push i
  else if seq.length > maxlength
    maxlength = seq.length
    maxnums = [i]

console.log "Max length: #{maxlength}; numbers generating sequences of this length: #{maxnums}"
