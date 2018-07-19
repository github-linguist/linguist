class Folder(){
  var dir = (1,0)
  var pos = (-1,0)
  def apply(l:List[Int], a:Array[Array[Int]]) = {
    var (x,y) = pos  //start position
    var (dx,dy) = dir //direction
    l.foreach {e => x = x + dx; y = y + dy; a(y)(x) = e }  //copy l elements to array using current direction
    pos = (x,y)
    dir = (-dy, dx) //turn
  }
}
def spiral(n:Int) = {
  def dup(n:Int) = (1 to n).flatMap(i=>List(i,i)).toList
  val folds = n :: dup(n-1).reverse  //define fold part lengths
	
  var array = new Array[Array[Int]](n,n)
  val fold = new Folder()

  var seq = (0 until n*n).toList  //sequence to fold
  folds.foreach {len => fold(seq.take(len),array); seq = seq.drop(len)}
  array
}
