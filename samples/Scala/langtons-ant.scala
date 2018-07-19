class Langton(matrix:Array[Array[Char]], ant:Ant) {
  import Langton._
  val rows=matrix.size
  val cols=matrix(0).size
	
  def isValid = 0 <= ant.row && ant.row < cols && 0 <= ant.col  && ant.col < rows
  def isBlack=matrix(ant.row)(ant.col)==BLACK
  def changeColor(c:Char)={matrix(ant.row)(ant.col)=c; matrix}
	
  def evolve():Langton={
    val (newCol, newAnt)=if(isBlack) (WHITE, ant.turnLeft) else (BLACK, ant.turnRight)
    new Langton(changeColor(newCol), newAnt.move)
  }
  override def toString()=matrix map (_.mkString("")) mkString "\n"	
}

case class Ant(row:Int, col:Int, d:Int=0) {
  def turnLeft=Ant(row,col,(d-1)&3)
  def turnRight=Ant(row,col,(d+1)&3)
  def move=d match {
    case 0 => Ant(row-1,col,d)	// north
    case 1 => Ant(row,col+1,d)	// east
    case 2 => Ant(row+1,col,d)	// south
    case 3 => Ant(row,col-1,d)	// west
  }
}

object Langton {
  val BLACK='#'
  val WHITE='.'
  def apply(x:Int=100, y:Int=100)=new Langton(Array.fill(y, x)(WHITE), Ant(x>>>1, y>>>1, 0))
	
  def main(args: Array[String]): Unit = {
    var l=Langton(100,100)
    var moves=0
    while (l.isValid) {
      moves += 1
      l=l.evolve
    }
    println("Out of bounds after "+moves+" moves")
    println(l)
  }
}
