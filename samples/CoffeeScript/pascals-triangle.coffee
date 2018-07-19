pascal = (n) ->
  width = 6
  for r in [1..n]
    s = ws (width/2) * (n-r) # center row
    output = (n) -> s += pad width, n
    cell = 1
    output cell
    # Compute binomial coefficients as you go
    # across the row.
    for c in [1...r]
      cell *= (r-c) / c
      output cell
    console.log s

ws = (n) ->
  s = ''
  s += ' ' for i in [0...n]
  s

pad = (cnt, n) ->
  s = n.toString()
  # There is probably a better way to do this.
  cnt -= s.length
  right = Math.floor(cnt / 2)
  left = cnt - right
  ws(left) + s + ws(right)

pascal(7)
