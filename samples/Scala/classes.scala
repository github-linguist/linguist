/** This class implicitly includes a constructor which accepts an Int and
 *  creates "val variable1: Int" with that value.
 */
class MyClass(val myMethod: Int) { // Acts like a getter, getter automatically generated.
  var variable2 = "asdf" // Another instance variable; a public var this time
  def this() = this(0) // An auxilliary constructor that instantiates with a default value
}

object HelloObject {
  val s = "Hello" // Not private, so getter auto-generated
}

/** Demonstrate use of our example class.
 */
object Call_an_object_method extends App {
  val s = "Hello"
  val m = new MyClass()
  val n = new MyClass(3)

  println(HelloObject.s) // prints "Hello" by object getterHelloObject

  println(m.myMethod) // prints 0
  println(n.myMethod) // prints 3
}
