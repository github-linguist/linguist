def genPowers(exponent) {
    var i := -1
    return def powerGenerator() {
        return (i += 1) ** exponent
    }
}

def filtered(source, filter) {
    var fval := filter()
    return def filterGenerator() {
        while (true) {
            def sval := source()
            while (sval > fval) {
                fval := filter()
            }
            if (sval < fval) {
                return sval
            }
        }
    }
}

def drop(n, gen) {
    for _ in 1..n { gen() }
}


def squares := genPowers(2)
def cubes := genPowers(3)
def squaresNotCubes := filtered(squares, cubes)
drop(20, squaresNotCubes)
for _ in 1..10 {
    print(`${squaresNotCubes()} `)
}
println()
