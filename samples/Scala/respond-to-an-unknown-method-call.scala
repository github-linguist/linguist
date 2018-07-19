class DynamicTest extends Dynamic
{
  def foo()=println("this is foo")
  def bar()=println("this is bar")
  def applyDynamic(name: String)(args: Any*)={
    println("tried to handle unknown method "+name)
    if(!args.isEmpty)
      println("  it had arguments: "+args.mkString(","))
  }
}

object DynamicTest {
  def main(args: Array[String]): Unit = {
    val d=new DynamicTest()
    d.foo()
    d.bar()
    d.grill()
    d.ding("dong")
  }
}
