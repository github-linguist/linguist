def compareBy(keyfn) { # This ought to be in the standard library
  return def comparer(a, b) {
    return keyfn(a).op__cmp(keyfn(b))
  }
}

def x := [
  ["Joe",3],
  ["Bill",4],
  ["Alice",20],
  ["Harry",3],
]

println(x.sort(compareBy(fn [name,_] { name })))
