def array = 1..3

// square via multiplication
def sumSq = array.collect { it * it }.sum()
println sumSq

// square via exponentiation
sumSq = array.collect { it ** 2 }.sum()

println sumSq
