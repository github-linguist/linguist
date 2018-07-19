object VersCheck extends App {
  val minimalVersion = 1.7

  val vers = System.getProperty("java.specification.version");
  println(if (vers.toDouble >= minimalVersion) "YAY!" else s"Must use Java >= $minimalVersion");

  val bloop = Option(-42)
  if (bloop.isDefined) bloop.get.abs
}
