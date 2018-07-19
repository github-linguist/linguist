def matrix = [ [ 1, 2, 3, 4 ],
               [ 5, 6, 7, 8 ] ]

matrix.each { println it }
println()
def transpose = matrix.transpose()

transpose.each { println it }
