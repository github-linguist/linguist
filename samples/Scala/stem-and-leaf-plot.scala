def stemAndLeaf(numbers: List[Int]) = {
  val lineFormat = "%" + (numbers map (_.toString.length) max) + "d | %s"
  val map = numbers groupBy (_ / 10)
  for (stem <- numbers.min / 10 to numbers.max / 10) {
    println(lineFormat format (stem, map.getOrElse(stem, Nil) map (_ % 10) sortBy identity mkString " "))
  }
}
