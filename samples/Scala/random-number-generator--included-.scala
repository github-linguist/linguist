import scala.util.Random

/**
 * Histogram of 200 throws with two dices.
 */
object Throws extends App {
  Stream.continually(Random.nextInt(6) + Random.nextInt(6) + 2)
    .take(200).groupBy(identity).toList.sortBy(_._1)
    .foreach {
      case (a, b) => println(f"$a%2d:" + "X" * b.size)
    }
}
