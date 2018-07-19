def bubbleSort[T](arr: Array[T])(implicit o: Ordering[T]) {
  import o._
  val consecutiveIndices = (arr.indices, arr.indices drop 1).zipped
  var hasChanged = true
  do {
    hasChanged = false
    consecutiveIndices foreach { (i1, i2) =>
      if (arr(i1) > arr(i2)) {
        hasChanged = true
        val tmp = arr(i1)
        arr(i1) = arr(i2)
        arr(i2) = tmp
      }
    }
  } while(hasChanged)
}
