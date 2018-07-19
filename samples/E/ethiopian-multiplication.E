def halve(&x)  { x //= 2 }
def double(&x) { x *= 2 }
def even(x)    { return x %% 2 <=> 0 }

def multiply(var a, var b) {
    var ab := 0
    while (a > 0) {
        if (!even(a)) { ab += b }
        halve(&a)
        double(&b)
    }
    return ab
}
