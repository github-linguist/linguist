object ExtremeFloatingPoint extends App {
  val negInf = -1.0 / 0.0 //also Double.NegativeInfinity
  val inf = 1.0 / 0.0 //  //also Double.PositiveInfinity
  val nan = 0.0 / 0.0 //  //also Double.NaN
  val negZero = -2.0 / inf

  println("Value:         Result:      Infinity? Whole?")
  println(f"Negative inf: ${negInf}%9s ${negInf.isInfinity}%9s ${negInf.isWhole}%9s")
  println(f"Positive inf: ${inf}%9s ${inf.isInfinity}%9s ${inf.isWhole}%9s")
  println(f"NaN:          ${nan}%9s ${nan.isInfinity}%9s ${nan.isWhole}%9s")
  println(f"Negative 0:   ${negZero}%9s ${negZero.isInfinity}%9s ${negZero.isWhole}%9s")
  println(f"inf + -inf:   ${inf + negInf}%9s ${(inf + negInf).isInfinity}%9s ${(inf + negInf).isWhole}%9s")
  println(f"0 * NaN:      ${0 * nan}%9s ${(inf + negInf).isInfinity}%9s ${(inf + negInf).isWhole}%9s")
  println(f"NaN == NaN:   ${nan == nan}%9s")
}
