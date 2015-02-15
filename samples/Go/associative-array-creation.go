// declare a nil map variable, for maps from string to int
var x map[string] int

// make an empty map
x = make(map[string] int)

// make an empty map with an initial capacity
x = make(map[string] int, 42)

// set a value
x["foo"] = 3

// make a map with a literal
x = map[string] int {
  "foo": 2, "bar": 42, "baz": -1,
}
