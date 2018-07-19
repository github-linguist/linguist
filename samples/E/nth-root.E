def nthroot(n, x) {
  require(n > 1 && x > 0)
  def np := n - 1
  def iter(g) { return (np*g + x/g**np) / n }
  var g1 := x
  var g2 := iter(g1)
  while (!(g1 <=> g2)) {
    g1 := iter(g1)
    g2 := iter(iter(g2))
  }
  return g1
}
