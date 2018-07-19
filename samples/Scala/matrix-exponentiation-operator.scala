class Matrix[T](matrix:Array[Array[T]])(implicit n: Numeric[T], m: ClassManifest[T])
{
  import n._
  val rows=matrix.size
  val cols=matrix(0).size
  def row(i:Int)=matrix(i)
  def col(i:Int)=matrix map (_(i))

  def *(other: Matrix[T]):Matrix[T] = new Matrix(
    Array.tabulate(rows, other.cols)((row, col) =>
      (this.row(row), other.col(col)).zipped.map(_*_) reduceLeft (_+_)
  ))

  def **(x: Int)=x match {
    case 0 => createIdentityMatrix
    case 1 => this
    case 2 => this * this
    case _ => List.fill(x)(this) reduceLeft (_*_)
  }
	
  def createIdentityMatrix=new Matrix(Array.tabulate(rows, cols)((row,col) =>
    if (row == col) one else zero)
  )

  override def toString = matrix map (_.mkString("[", ", ", "]")) mkString "\n"
}

object MatrixTest {
  def main(args:Array[String])={
    val m=new Matrix[BigInt](Array(Array(3,2), Array(2,1)))
    println("-- m --\n"+m)
				
    Seq(0,1,2,3,4,10,20,50) foreach {x =>
      println("-- m**"+x+" --")
      println(m**x)
    }
  }
}
