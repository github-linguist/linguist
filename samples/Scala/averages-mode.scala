import scala.collection.breakOut
import scala.collection.generic.CanBuildFrom
def mode
  [T, CC[X] <: Seq[X]](coll: CC[T])
  (implicit o: T => Ordered[T], cbf: CanBuildFrom[Nothing, T, CC[T]])
  : CC[T] = {
  val grouped = coll.groupBy(x => x).mapValues(_.size).toSeq
  val max = grouped.map(_._2).max
  grouped.filter(_._2 == max).map(_._1)(breakOut)
}
