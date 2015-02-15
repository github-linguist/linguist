case class Matrix( val matrix:Array[Array[Double]] ) {

  // Assuming matrix is positive-definite, symmetric and not empty...

  val rows,cols = matrix.size

  def getOption( r:Int, c:Int ) : Option[Double] = Pair(r,c) match {
    case (r,c) if r < rows && c < rows => Some(matrix(r)(c))
    case _ => None
  }

  def isLowerTriangle( r:Int, c:Int ) : Boolean = { c <= r }
  def isDiagonal( r:Int, c:Int ) : Boolean = { r == c}

  override def toString = matrix.map(_.mkString(", ")).mkString("\n")

  /**
   * Perform Cholesky Decomposition of this matrix
   */
  lazy val cholesky : Matrix = {

    val l = Array.ofDim[Double](rows*cols)

    for( i <- (0 until rows); j <- (0 until cols) ) yield {
	
      val s = (for( k <- (0 until j) ) yield { l(i*rows+k) * l(j*rows+k) }).sum
	
      l(i*rows+j) = (i,j) match {
        case (r,c) if isDiagonal(r,c) => scala.math.sqrt(matrix(i)(i) - s)
        case (r,c) if isLowerTriangle(r,c) => (1.0 / l(j*rows+j) * (matrix(i)(j) - s))
        case _ => 0
      }
    }

    val m = Array.ofDim[Double](rows,cols)
    for( i <- (0 until rows); j <- (0 until cols) ) m(i)(j) = l(i*rows+j)
    Matrix(m)
  }
}

// A little test...
val a1 = Matrix(Array[Array[Double]](Array(25,15,-5),Array(15,18,0),Array(-5,0,11)))
val a2 = Matrix(Array[Array[Double]](Array(18,22,54,42), Array(22,70,86,62), Array(54,86,174,134), Array(42,62,134,106)))

val l1 = a1.cholesky
val l2 = a2.cholesky


// Given test results
val r1 = Array[Double](5,0,0,3,3,0,-1,1,3)
val r2 = Array[Double](4.24264,0.00000,0.00000,0.00000,5.18545,6.56591,0.00000,0.00000,
                        12.72792,3.04604,1.64974,0.00000,9.89949,1.62455,1.84971,1.39262)

// Verify assertions						
(l1.matrix.flatten.zip(r1)).foreach{ case (result,test) =>
  assert(math.round( result * 100000 ) * 0.00001 == math.round( test * 100000 ) * 0.00001)
}

(l2.matrix.flatten.zip(r2)).foreach{ case (result,test) =>
  assert(math.round( result * 100000 ) * 0.00001 == math.round( test * 100000 ) * 0.00001)
}
