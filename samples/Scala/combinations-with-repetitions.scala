object CombinationsWithRepetition {

  def multi[A](as: List[A], k: Int): List[List[A]] =
    (List.fill(k)(as)).flatten.combinations(k).toList

  def main(args: Array[String]): Unit = {
    val doughnuts = multi(List("iced", "jam", "plain"), 2)
    for (combo <- doughnuts) println(combo.mkString(","))

    val bonus = multi(List(0,1,2,3,4,5,6,7,8,9), 3).size
    println("There are "+bonus+" ways to choose 3 items from 10 choices")
  }
}
