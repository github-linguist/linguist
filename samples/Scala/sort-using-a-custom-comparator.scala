List("Here", "are", "some", "sample", "strings", "to", "be", "sorted").sortWith{(a,b) =>
  val cmp=a.size-b.size
  (if (cmp==0) -a.compareTo(b) else cmp) > 0
}
