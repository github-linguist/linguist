object Harshad extends App {
  import Stream._
  val fh: Stream[Int] => Stream[Int] = ints => ints.head#::fh((ints.tail) filter {i=>i%(i.toString.map(_.asDigit):\0)(_+_)==0})
  val harshads = fh(from(1))

  println(harshads take 20 toList)
  println(harshads filter (_>1000) head)
}
