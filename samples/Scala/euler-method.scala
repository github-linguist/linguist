object App{

  def main(args : Array[String]) = {

    def cooling( step : Int ) = {
      eulerStep( (step , y) => {-0.07 * (y - 20)} ,
        100.0,0,100,step)
    }
    cooling(10)
    cooling(5)
    cooling(2)
  }
  def eulerStep( func : (Int,Double) => Double,y0 : Double,
    begin : Int, end : Int , step : Int) = {

    println("Step size: %s".format(step))

    var current : Int = begin
    var y : Double = y0
    while( current <= end){
      println( "%d %.5f".format(current,y))
      current += step
      y += step * func(current,y)
    }

    println("DONE")
  }

}
