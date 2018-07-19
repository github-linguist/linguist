object LinearCongruentialGenerator {
  def bsdRandom(rseed:Int):Iterator[Int]=new Iterator[Int]{
    var seed=rseed
    override def hasNext:Boolean=true
    override def next:Int={seed=(seed * 1103515245 + 12345) & Int.MaxValue; seed}
  }

  def msRandom(rseed:Int):Iterator[Int]=new Iterator[Int]{
    var seed=rseed
    override def hasNext:Boolean=true
    override def next:Int={seed=(seed * 214013 + 2531011) & Int.MaxValue; seed >> 16}
  }
		
  def toString(it:Iterator[Int], n:Int=20)=it take n mkString ", "
		
  def main(args:Array[String]){
    println("-- seed 0 --")
    println("BSD: "+ toString(bsdRandom(0)))
    println("MS : "+ toString(msRandom(0)))
						
    println("-- seed 1 --")
    println("BSD: "+ toString(bsdRandom(1)))
    println("MS : "+ toString( msRandom(1)))
  }
}
