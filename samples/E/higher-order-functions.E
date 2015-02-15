def map(f, list) {
  var out := []
  for x in list {
    out with= f(x)
  }
  return out
}

? map(fn x { x + x }, [1, "two"])
# value: [2, "twotwo"]

? map(1.add, [5, 10, 20])
# value: [6, 11, 21]

? def foo(x) { return -(x.size()) }
> map(foo, ["", "a", "bc"])
# value: [0, -1, -2]
