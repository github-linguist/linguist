def factors(number) {
    if (number == 1) {
        return [1]
    }
    def factors = []
    BigInteger value = number
    BigInteger possibleFactor = 2
    while (possibleFactor <= value) {
        if (value % possibleFactor == 0) {
            factors << possibleFactor
            value /= possibleFactor
        } else {
            possibleFactor++
        }
    }
    factors
}
Number.metaClass.factors = { factors(delegate) }

((1..10) + (6351..6359)).each { number ->
    println "$number = ${number.factors().join(' x ')}"
}
