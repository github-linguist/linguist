import java.util.Locale._

object Gamma {
  def stGamma(x:Double):Double=math.sqrt(2*math.Pi/x)*math.pow((x/math.E), x)

  def laGamma(x:Double):Double={
    val p=Seq(676.5203681218851, -1259.1392167224028, 771.32342877765313,
             -176.61502916214059, 12.507343278686905, -0.13857109526572012,
                9.9843695780195716e-6, 1.5056327351493116e-7)

    if(x < 0.5) {
      math.Pi/(math.sin(math.Pi*x)*laGamma(1-x))
    } else {
      val x2=x-1
      val t=x2+7+0.5
      val a=p.zipWithIndex.foldLeft(0.99999999999980993)((r,v) => r+v._1/(x2+v._2+1))
      math.sqrt(2*math.Pi)*math.pow(t, x2+0.5)*math.exp(-t)*a
    }
  }

  def main(args: Array[String]): Unit = {
    println("Gamma    Stirling             Lanczos")
    for(x <- 0.1 to 2.0 by 0.1)
      println("%.1f  ->  %.16f   %.16f".formatLocal(ENGLISH, x, stGamma(x), laGamma(x)))
  }
}
