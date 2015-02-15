object VlqCode {
  def encode(x:Long)={
    val result=scala.collection.mutable.Stack[Byte]()
    result push (x&0x7f).toByte
    var l = x >>> 7
    while(l>0){
      result push ((l&0x7f)|0x80).toByte
      l >>>= 7
    }
    result.toArray
  }

  def decode(a:Array[Byte])=a.foldLeft(0L)((r, b) => r<<7|b&0x7f)

  def toString(a:Array[Byte])=a map("%02x".format(_)) mkString("[", ", ", "]")

  def test(x:Long)={
    val enc=encode(x)
    println("0x%x => %s => 0x%x".format(x, toString(enc), decode(enc)))
  }

  def main(args: Array[String]): Unit = {
    val xs=Seq(0, 0x7f, 0x80, 0x2000, 0x3fff, 0x4000, 0x1FFFFF, 0x200000, 0x8000000,
               0xFFFFFFF, 0xFFFFFFFFL, 0x842FFFFFFFFL, 0x0FFFFFFFFFFFFFFFL)
    xs foreach test
  }
}
