def maxSubseq(l: List[Int]) = l.scanRight(Nil : List[Int]) {
  case (el, acc) if acc.sum + el < 0 => Nil
  case (el, acc)                     => el :: acc
} max Ordering.by((_: List[Int]).sum)

def biggestMaxSubseq(l: List[Int]) = l.scanRight(Nil : List[Int]) {
  case (el, acc) if acc.sum + el < 0 => Nil
  case (el, acc)                     => el :: acc
} max Ordering.by((ss: List[Int]) => (ss.sum, ss.length))

def biggestMaxSubseq[N](l: List[N])(implicit n: Numeric[N]) = {
  import n._
  l.scanRight(Nil : List[N]) {
    case (el, acc) if acc.sum + el < zero => Nil
    case (el, acc)                        => el :: acc
  } max Ordering.by((ss: List[N]) => (ss.sum, ss.length))
}

def linearBiggestMaxSubseq[N](l: List[N])(implicit n: Numeric[N]) = {
  import n._
  l.scanRight((zero, Nil : List[N])) {
    case (el, (acc, _)) if acc + el < zero => (zero, Nil)
    case (el, (acc, ss))                   => (acc + el, el :: ss)
  } max Ordering.by((t: (N, List[N])) => (t._1, t._2.length)) _2
}
