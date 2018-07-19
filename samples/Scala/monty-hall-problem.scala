import scala.util.Random

object MontyHallSimulation {
  def main(args: Array[String]) {
    val samples = if (args.size == 1 && (args(0) matches "\\d+")) args(0).toInt else 1000
    val doors = Set(0, 1, 2)
    var stayStrategyWins = 0
    var switchStrategyWins = 0

    1 to samples foreach { _ =>
      val prizeDoor = Random shuffle doors head;
      val choosenDoor = Random shuffle doors head;
      val hostDoor = Random shuffle (doors - choosenDoor - prizeDoor) head;
      val switchDoor = doors - choosenDoor - hostDoor head;

      (choosenDoor, switchDoor) match {
        case (`prizeDoor`, _) => stayStrategyWins += 1
        case (_, `prizeDoor`) => switchStrategyWins += 1
      }
    }

    def percent(n: Int) = n * 100 / samples

    val report = """|%d simulations were ran.
                    |Staying won %d times (%d %%)
                    |Switching won %d times (%d %%)""".stripMargin

    println(report
            format (samples,
                    stayStrategyWins, percent(stayStrategyWins),
                    switchStrategyWins, percent(switchStrategyWins)))
  }
}
