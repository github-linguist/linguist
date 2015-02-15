def AccumulatorFactory[N](n: N)(implicit num: Numeric[N]) = {
  import num._
  var acc = n
  (inc: N) => {
    acc = acc + inc
    acc
  }
}
