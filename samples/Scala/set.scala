object sets {
  val set1 = Set(1,2,3,4,5)
  val set2 = Set(3,5,7,9)
  println(set1 contains 3)
  println(set1 | set2)
  println(set1 & set2)
  println(set1 diff set2)
  println(set1 subsetOf set2)
  println(set1 == set2)
}
