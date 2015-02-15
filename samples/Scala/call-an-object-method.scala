/* This class implicitly includes a constructor which accepts an Int and
 *  creates "val variable1: Int" with that value.
 */
class MyClass(val memberVal: Int) { // Acts like a getter, getter automatically generated.
  var variable2 = "asdf" // Another instance variable; a public mutable this time
  def this() = this(0) // An auxilliary constructor that instantiates with a default value
}

object HelloObject {
  val s = "Hello" // Not private, so getter auto-generated
}

/** Demonstrate use of our example class.
 */
object Call_an_object_method extends App {
  val s = "Hello"
  val m = new MyClass
  val n = new MyClass(3)

  assert(HelloObject.s == "Hello") // "Hello" by object getterHelloObject
  assert(m.memberVal == 0)
  assert(n.memberVal == 3)
  println("Successfully completed without error.")
}
