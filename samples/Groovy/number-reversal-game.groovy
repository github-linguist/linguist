sorted = [*(1..9)]
arr    = sorted.clone()

void flipstart(n) { arr[0..<n] = arr[0..<n].reverse() }

int steps = 0
while (arr==sorted) Collections.shuffle(arr)
while (arr!=sorted) {
    println arr.join(' ')
    print 'Reverse how many? '
    def flipcount = System.in.readLine()
    flipstart( flipcount.toInteger() )
    steps += 1
}
println "Done! That took you ${steps} steps"
