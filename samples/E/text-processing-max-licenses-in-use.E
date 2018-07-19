var out := 0
var maxOut := 0
var maxTimes := []

def events := ["OUT " => 1, "IN  " => -1]

for line in <file:mlijobs.txt> {
    def `License @{via (events.fetch) delta}@@ @time for job @num$\n` := line

    out += delta
    if (out > maxOut) {
        maxOut := out
        maxTimes := []
    }
    if (out == maxOut) {
        maxTimes with= time
    }
}

println(`Maximum simultaneous license use is $maxOut at the following times:`)
for time in maxTimes {
    println(` $time`)
}
