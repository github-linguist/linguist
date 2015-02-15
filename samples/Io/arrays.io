foo := list("foo", "bar", "baz")
foo at(1) println // bar
foo append("Foobarbaz")
foo println
foo atPut(2, "barbaz") // baz becomes barbaz
