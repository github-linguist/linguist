def map = [lastName: "Anderson", firstName: "Thomas", nickname: "Neo", age: 24, address: "everywhere"]

println "Entries:"
map.each { println it }

println()
println "Keys:"
map.keySet().each { println it }

println()
println "Values:"
map.values().each { println it }
