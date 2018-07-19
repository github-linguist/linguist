def binarySearch[A <% Ordered[A]](a: IndexedSeq[A], v: A) = {
  def recurse(low: Int, high: Int): Option[Int] = (low + high) / 2 match {
    case _ if high < low => None
    case mid if a(mid) > v => recurse(low, mid - 1)
    case mid if a(mid) < v => recurse(mid + 1, high)
    case mid => Some(mid)
  }
  recurse(0, a.size - 1)
}
