def zigZag(n) {
  def move(&i, &j) {
      if (j < (n - 1)) {
          i := 0.max(i - 1)
          j += 1
      } else {
          i += 1
      }
  }

  def array := makeFlex2DArray(n, n)
  var x := 0
  var y := 0

  for i in 1..n**2 {
      array[y, x] := i
      if ((x + y) % 2 == 0) {
          move(&x, &y)
      } else {
          move(&y, &x)
      }
  }
  return array
}
