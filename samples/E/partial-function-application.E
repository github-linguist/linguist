def pa(f, args1) {
  return def partial {
    match [`run`, args2] {
      E.call(f, "run", args1 + args2)
    }
  }
}

def fs(f, s) {
  var r := []
  for n in s {
    r with= f(n)
  }
  return r
}

def f1(n) { return n * 2 }
def f2(n) { return n ** 2 }

def fsf1 := pa(fs, [f1])
def fsf2 := pa(fs, [f2])
for s in [0..3, [2, 4, 6, 8]] {
  for f in [fsf1, fsf2] {
    println(f(s))
  }
}
