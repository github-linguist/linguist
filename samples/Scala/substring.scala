object Substring {
  // Ruler             1         2         3         4         5         6
  //         012345678901234567890123456789012345678901234567890123456789012
  val str = "The good life is one inspired by love and guided by knowledge."
  val (n, m) = (21, 16) // An one-liner to set n = 21, m = 16

  // Starting from n characters in and of m length
  assert("inspired by love" == str.slice(n, n + m))
  // Starting from n characters in, up to the end of the string
  assert("inspired by love and guided by knowledge." == str.drop(n))
  // Whole string minus last character
  assert("The good life is one inspired by love and guided by knowledge" == str.init)
  // Starting from a known character within the string and of m length
  assert("life is one insp" == { val i = str.indexOf('l'); str.slice(i, i + m) })
  // Alternatively
  assert("life is one insp" == str.drop(str.indexOf('l')).take(m))

  // Starting from a known substring within the string and of m length
  assert("good life is one" == { val i = str.indexOf("good"); str.slice(i, i + m) })
  // Alternatively
  assert("good life is one" == str.drop(str.indexOf("good")).take(m))
}
