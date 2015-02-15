def bitwise(a: Int, b: Int) {
  println("a and b: " + (a & b))
  println("a or b: " + (a | b))
  println("a xor b: " + (a ^ b))
  println("not a: " + (~a))
  println("a << b: " + (a << b)) // left shift
  println("a >> b: " + (a >> b)) // arithmetic right shift
  println("a >>> b: " + (a >>> b)) // unsigned right shift
  println("a rot b: " + Integer.rotateLeft(a, b)) // Rotate Left
  println("a rol b: " + Integer.rotateRight(a, b)) // Rotate Right
}
