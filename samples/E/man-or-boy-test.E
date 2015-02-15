def a(var k, &x1, &x2, &x3, &x4, &x5) {
    def bS; def &b := bS
    bind bS {
        to get() {
            k -= 1
            return a(k, &b, &x1, &x2, &x3, &x4)
        }
    }
    return if (k <= 0) { x4 + x5 } else { b }
}

def p := 1
def n := -1
def z := 0
println(a(10, &p, &n, &n, &p, &z))
