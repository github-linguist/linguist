val keys = List(1, 2, 3)
val values = Array("A", "B", "C") // Array mixed with List
val map = keys.zip(values).toMap  // and other Seq are possible.

// Testing
assert(map == Map(1 ->"A", 2 -> "B", 3 -> "C"))
println("Successfully completed without errors.")
