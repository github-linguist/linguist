def flatList(l: List[_]): List[Any] = l match {
  case Nil => Nil
  case (head: List[_]) :: tail => flatList(head) ::: flatList(tail)
  case head :: tail => head :: flatList(tail)
}
