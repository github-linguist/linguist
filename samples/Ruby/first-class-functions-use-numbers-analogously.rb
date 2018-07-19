multiplier = proc {|n1, n2| proc {|m| n1 * n2 * m}}
numlist = [x=2, y=4, x+y]
invlist = [0.5, 0.25, 1.0/(x+y)]
p numlist.zip(invlist).map {|n, invn| multiplier[invn, n][0.5]}
# => [0.5, 0.5, 0.5]
