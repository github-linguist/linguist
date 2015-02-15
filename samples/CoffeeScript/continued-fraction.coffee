# Compute a continuous fraction of the form
# a0 + b1 / (a1 + b2 / (a2 + b3 / ...
continuous_fraction = (f) ->
  a = f.a
  b = f.b
  c = 1
  for n in [100000..1]
    c = b(n) / (a(n) + c)
  a(0) + c

# A little helper.
p = (a, b) ->
  console.log a
  console.log b
  console.log "---"

do ->
  fsqrt2 =
    a: (n) -> if n is 0 then 1 else 2
    b: (n) -> 1
  p Math.sqrt(2), continuous_fraction(fsqrt2)

  fnapier =
    a: (n) -> if n is 0 then 2 else n
    b: (n) -> if n is 1 then 1 else n - 1
  p Math.E, continuous_fraction(fnapier)

  fpi =
    a: (n) ->
      return 3 if n is 0
      6
    b: (n) ->
      x = 2*n - 1
      x * x
  p Math.PI, continuous_fraction(fpi)
