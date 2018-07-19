object DivideByZero extends Application {

  def check(x: Int, y: Int): Boolean = {
    try {
      val result = x / y
      println(result)
      return false
    } catch {
      case x: ArithmeticException => {
        return true
      }
    }
  }

  println("divided by zero = " + check(1, 0))

}
