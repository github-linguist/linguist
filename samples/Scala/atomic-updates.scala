object AtomicUpdates {

  class Buckets(ns: Int*) {

    import scala.actors.Actor._

    val buckets = ns.toArray

    case class Get(index: Int)
    case class Transfer(fromIndex: Int, toIndex: Int, amount: Int)
    case object GetAll

    val handler = actor {
      loop {
        react {
          case Get(index) => reply(buckets(index))
          case Transfer(fromIndex, toIndex, amount) =>
            assert(amount >= 0)
            val actualAmount = Math.min(amount, buckets(fromIndex))
            buckets(fromIndex) -= actualAmount
            buckets(toIndex) += actualAmount
          case GetAll => reply(buckets.toList)
        }
      }
    }

    def get(index: Int): Int = (handler !? Get(index)).asInstanceOf[Int]
    def transfer(fromIndex: Int, toIndex: Int, amount: Int) = handler ! Transfer(fromIndex, toIndex, amount)
    def getAll: List[Int] = (handler !? GetAll).asInstanceOf[List[Int]]
  }

  def randomPair(n: Int): (Int, Int) = {
    import scala.util.Random._
    val pair = (nextInt(n), nextInt(n))
    if (pair._1 == pair._2) randomPair(n) else pair
  }

  def main(args: Array[String]) {
    import scala.actors.Scheduler._
    val buckets = new Buckets(List.range(1, 11): _*)
    val stop = new java.util.concurrent.atomic.AtomicBoolean(false)
    val latch = new java.util.concurrent.CountDownLatch(3)
    execute {
      while (!stop.get) {
        val (i1, i2) = randomPair(10)
        val (n1, n2) = (buckets.get(i1), buckets.get(i2))
        val m = (n1 + n2) / 2
        if (n1 < n2)
          buckets.transfer(i2, i1, n2 - m)
        else
          buckets.transfer(i1, i2, n1 - m)
      }
      latch.countDown
    }
    execute {
      while (!stop.get) {
        val (i1, i2) = randomPair(10)
        val n = buckets.get(i1)
        buckets.transfer(i1, i2, if (n == 0) 0 else scala.util.Random.nextInt(n))
      }
      latch.countDown
    }
    execute {
      for (i <- 1 to 20) {
        val all = buckets.getAll
        println(all.sum + ":" + all)
        Thread.sleep(500)
      }
      stop.set(true)
      latch.countDown
    }
    latch.await
    shutdown
  }
}
