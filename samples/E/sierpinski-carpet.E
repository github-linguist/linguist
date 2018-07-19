def inCarpet(var x, var y) {
    while (x > 0 && y > 0) {
        if (x %% 3 <=> 1 && y %% 3 <=> 1) {
            return false
        }
        x //= 3
        y //= 3
    }
    return true
}

def carpet(order) {
    for y in 0..!(3**order) {
        for x in 0..!(3**order) {
            print(inCarpet(x, y).pick("#", " "))
        }
        println()
    }
}
