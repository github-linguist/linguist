def heapsort := {
  def cswap(c, a, b) {
    def t := c[a]
    c[a]  := c[b]
    c[b]  := t
    # println(c)
  }

  def siftDown(array, start, finish) {
    var root := start
    while (var child := root * 2 + 1
           child <= finish) {
      if (child + 1 <= finish && array[child] < array[child + 1]) {
        child += 1
      }
      if (array[root] < array[child]) {
        cswap(array, root, child)
        root := child
      } else {
        break
      }
    }
  }

  /** Heapsort (in-place). */
  def heapsort(array) {
    # in pseudo-code, heapify only called once, so inline it here
    for start in (0..((array.size()-2)//2)).descending() {
      siftDown(array, start, array.size()-1)
    }

    for finish in (0..(array.size()-1)).descending() {
      cswap(array, 0, finish)
      siftDown(array, 0, finish - 1)
    }
  }
}
