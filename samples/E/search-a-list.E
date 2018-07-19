def haystack := ["Zig","Zag","Wally","Ronald","Bush","Krusty","Charlie","Bush","Bozo"]

/** meet the 'raise an exception' requirement */
def find(needle) {
    switch (haystack.indexOf1(needle)) {
        match ==(-1) { throw("an exception") }
        match index { return index }
    }
}

println(find("Ronald")) # prints 3
println(find("McDonald")) # will throw
