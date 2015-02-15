def insert(list: List[Int], value: Int) = list.span(_ < value) match {
  case (lower, upper) => lower ::: value :: upper
}
def insertSort(list: List[Int]) = list.foldLeft(List[Int]())(insert)
