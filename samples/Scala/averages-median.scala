def median[T](s: Seq[T])(implicit n: Fractional[T]) = {
  import n._
  val (lower, upper) = s.sortWith(_<_).splitAt(s.size / 2)
  if (s.size % 2 == 0) (lower.last + upper.head) / fromInt(2) else upper.head
}
