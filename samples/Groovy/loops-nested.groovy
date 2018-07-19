final random = new Random()
def a = []
(0..<10).each {
    def row = []
    (0..<10).each {
        row << (random.nextInt(20) + 1)
    }
    a << row
}

a.each { println it }
println ()

Outer:
for (i in (0..<a.size())) {
    for (j in (0..<a[i].size())) {
        if (a[i][j] == 20){
            println ([i:i, j:j])
            break Outer
        }
    }
}
