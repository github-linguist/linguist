? def symmDiff(a, b) { return (a &! b) | (b &! a) }
# value: <symmDiff>

? symmDiff(["John", "Bob", "Mary", "Serena"].asSet(), ["Jim", "Mary", "John", "Bob"].asSet())
# value: ["Jim", "Serena"].asSet()
