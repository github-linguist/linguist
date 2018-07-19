object NumericalIntegration {
  def leftRect(f:Double=>Double, a:Double, b:Double)=f(a)
  def midRect(f:Double=>Double, a:Double, b:Double)=f((a+b)/2)
  def rightRect(f:Double=>Double, a:Double, b:Double)=f(b)
  def trapezoid(f:Double=>Double, a:Double, b:Double)=(f(a)+f(b))/2
  def simpson(f:Double=>Double, a:Double, b:Double)=(f(a)+4*f((a+b)/2)+f(b))/6;

  def fn1(x:Double)=x*x*x
  def fn2(x:Double)=1/x
  def fn3(x:Double)=x

  type Method = (Double=>Double, Double, Double) => Double
  def integrate(f:Double=>Double, a:Double, b:Double, steps:Double, m:Method)={
    val delta:Double=(b-a)/steps
    delta*(a until b by delta).foldLeft(0.0)((s,x) => s+m(f, x, x+delta))
  }

  def print(f:Double=>Double, a:Double, b:Double, steps:Double)={
    println("rectangular left   : %f".format(integrate(f, a, b, steps, leftRect)))
    println("rectangular middle : %f".format(integrate(f, a, b, steps, midRect)))
    println("rectangular right  : %f".format(integrate(f, a, b, steps, rightRect)))
    println("trapezoid          : %f".format(integrate(f, a, b, steps, trapezoid)))
    println("simpson            : %f".format(integrate(f, a, b, steps, simpson)))
  }

  def main(args: Array[String]): Unit = {
    print(fn1, 0, 1, 100)
    println("------")
    print(fn2, 1, 100, 1000)
    println("------")
    print(fn3, 0, 5000, 5000000)
    println("------")
    print(fn3, 0, 6000, 6000000)
  }
}
