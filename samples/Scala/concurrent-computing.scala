import scala.actors.Futures
List("Enjoy", "Rosetta", "Code").map { x =>
    Futures.future {
      Thread.sleep((Math.random * 1000).toInt)
       println(x)
    }
}.foreach(_())
