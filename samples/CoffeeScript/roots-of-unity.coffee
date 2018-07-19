# Find the n nth-roots of 1
nth_roots_of_unity = (n) ->
  (complex_unit_vector(2*Math.PI*i/n) for i in [1..n])

complex_unit_vector = (rad) ->
  new Complex(Math.cos(rad), Math.sin(rad))

class Complex
  constructor: (@real, @imag) ->
  toString: ->
    round_z = (n) ->
      if Math.abs(n) < 0.00005 then 0 else n
    fmt = (n) -> n.toFixed(3)
    real = round_z @real
    imag = round_z @imag
    s = ''
    if real and imag
      "#{fmt real}+#{fmt imag}i"
    else if real or !imag
      "#{fmt real}"
    else
      "#{fmt imag}i"

do ->
  for n in [2..5]
    console.log "---1 to the 1/#{n}"
    for root in nth_roots_of_unity n
      console.log root.toString()
