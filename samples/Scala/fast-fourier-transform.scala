object FFT extends App {
  import scala.math._

  case class Complex(re: Double, im: Double = 0.0) {
    def +(x: Complex): Complex = Complex((this.re+x.re),(this.im+x.im))
    def -(x: Complex): Complex = Complex((this.re-x.re),(this.im-x.im))
    def *(x: Complex): Complex = Complex(this.re*x.re-this.im*x.im,this.re*x.im+this.im*x.re)
  }

  def fft(f: List[Complex]): List[Complex] = {
    import Stream._
    require((f.size==0)||(from(0) map {x=>pow(2,x).toInt}).takeWhile(_<2*f.size).toList.exists(_==f.size)==true,"list size "+f.size+" not allowed!")
    f.size match {
      case 0 => Nil
      case 1 => f
      case n => {
        val cis: Double => Complex = phi => Complex(cos(phi),sin(phi))
        val e = fft(f.zipWithIndex.filter(_._2%2==0).map(_._1))
        val o  = fft(f.zipWithIndex.filter(_._2%2!=0).map(_._1))
        import scala.collection.mutable.ListBuffer
        val lb = new ListBuffer[Pair[Int, Complex]]()
        for (k <- 0 to n/2-1) {
          lb += Pair(k,e(k)+o(k)*cis(-2*Pi*k/n))
          lb += Pair(k+n/2,e(k)-o(k)*cis(-2*Pi*k/n))
        }
        lb.toList.sortWith((x,y)=>x._1<y._1).map(_._2)
      }
    }
  }

  fft(List(Complex(1),Complex(1),Complex(1),Complex(1),Complex(0),Complex(0),Complex(0),Complex(0))).foreach(println)
}
