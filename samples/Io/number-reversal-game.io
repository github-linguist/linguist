withRange := method( a, z,
    Range clone setRange(a,z)
)
sorted := withRange(1,9) asList
numbers := sorted clone shuffle
while( numbers==sorted, numbers = numbers shuffle)

steps :=0
stdin := File standardInput
while( numbers != sorted,
    writeln(numbers join(" "))
    write("Reverse how many? ")
    flipcount := stdin readLine asNumber
    withRange(0, ((flipcount-1)/2) floor) foreach( i,
        numbers swapIndices(i,flipcount-1-i)
    )
    steps = steps+1
)
writeln("Done! That took you ", steps, " steps")
