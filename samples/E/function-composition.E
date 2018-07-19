def compose(f, g) {
  return fn x { return f(g(x)) }
}
