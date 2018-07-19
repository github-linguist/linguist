def selectionSort := {
  def cswap(c, a, b) {
    def t := c[a]
    c[a]  := c[b]
    c[b]  := t
    println(c)
  }

  def indexOfMin(array, first, last) {
    var min := array[first]
    var mini := first
    for i in (first+1)..last {
      if (array[i] < min) {
        min := array[i]
        mini := i
      }
    }
    return mini
  }

  /** Selection sort (in-place). */
  def selectionSort(array) {
    def last := (array.size()-1)
    for i in 0..(last - 1) {
      cswap(array, i, indexOfMin(array, i + 1, last))
    }
  }
}
