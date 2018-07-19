def heapSort[T](a: Array[T])(implicit ord: Ordering[T]) {
  import scala.annotation.tailrec // Ensure functions are tail-recursive
  import ord._

  val indexOrdering = Ordering by a.apply

  def numberOfLeaves(heapSize: Int) = (heapSize + 1) / 2

  def children(i: Int, heapSize: Int) = {
    val leftChild = i * 2 + 1
    leftChild to leftChild + 1 takeWhile (_ < heapSize)
  }

  def swap(i: Int, j: Int) = {
    val tmp = a(i)
    a(i) = a(j)
    a(j) = tmp
  }

  // Maintain partial ordering by bubbling down elements
  @tailrec
  def siftDown(i: Int, heapSize: Int) {
    val childrenOfI = children(i, heapSize)
    if (childrenOfI nonEmpty) {
      val biggestChild = childrenOfI max indexOrdering
      if (a(i) < a(biggestChild)) {
        swap(i, biggestChild)
        siftDown(biggestChild, heapSize)
      }
    }
  }

  // Prepare heap by sifting down all non-leaf elements
  for (i <- a.indices.reverse drop numberOfLeaves(a.size)) siftDown(i, a.size)

  // Sort from the end of the array forward, by swapping the highest element,
  // which is always the top of the heap, to the end of the unsorted array
  for (i <- a.indices.reverse) {
    swap(0, i)
    siftDown(0, i)
  }
}
