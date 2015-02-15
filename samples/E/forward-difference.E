pragma.enable("accumulator")
/** Single step. */
def forwardDifference(seq :List) {
    return accum [] for i in 0..(seq.size() - 2) {
        _.with(seq[i + 1] - seq[i])
    }
}

/** Iterative implementation of the goal. */
def nthForwardDifference1(var seq :List, n :(int >= 0)) {
    for _ in 1..n { seq := forwardDifference(seq) }
    return seq
}

/** Imperative implementation of the goal. */
def nthForwardDifference2(seq :List, n :(int >= 0)) {
  def buf := seq.diverge()
  def finalSize := seq.size() - n
  for lim in (finalSize..!seq.size()).descending() {
    for i in 0..!lim {
      buf[i] := buf[i + 1] - buf[i]
    }
  }
  return buf.run(0, finalSize)
}

? def sampleData := [90, 47, 58, 29, 22, 32, 55, 5, 55, 73]
> for n in 0..10 {
>   def r1 := nthForwardDifference1(sampleData, n)
>   require(r1 == nthForwardDifference2(sampleData, n))
>   println(r1)
> }
