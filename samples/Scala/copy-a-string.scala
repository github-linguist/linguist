  val src = "Hello"
  // Its actually not a copy but a reference
  // That is not a problem because String is immutable
  // In fact its a feature
  val des = src
  assert(src eq des) // Proves the same reference is used.
  // To make a real copy makes no sense.
  // Actually its hard to make a copy, the compiler is too smart.
  // mkString, toString makes also not a real copy
  val cop = src.mkString.toString
  assert((src eq cop))                 // Still no copyed image
  val copy = src.reverse.reverse       // Finally double reverse makes a copy
  assert(src == copy && !(src eq copy))// Prove, but it really makes no sense.
