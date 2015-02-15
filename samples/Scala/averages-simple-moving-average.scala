class MovingAverage(period: Int) {
  private var queue = new scala.collection.mutable.Queue[Double]()
  def apply(n: Double) = {
    queue.enqueue(n)
    if (queue.size > period)
      queue.dequeue
    queue.sum / queue.size
  }
  override def toString = queue.mkString("(", ", ", ")")+", period "+period+", average "+(queue.sum / queue.size)
  def clear = queue.clear
}
