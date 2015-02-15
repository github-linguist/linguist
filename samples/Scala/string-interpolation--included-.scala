object StringInterpolation extends App {

  import util.matching.Regex._
  val size = "little"

  { // Method I (preferred)
    // Scala 2.10.0 supports direct string interpolation
    // by putting "s" at the beginning of the string.
    println("V2.10+  : " + s"Mary had a $size lamb,")
  }

  { // Method II
    // Pre Scala 2.10 indirect use of Java Class Formatter
    val originalFormatter = "Mary had a %s lamb,"
    println("V2.10- 1: " + originalFormatter format size)
    // Above mentioned is Scala's postfix notation and equivalent for:
    println("V2.10- 2: " + originalFormatter.format(size))
    // Also possible
    printf(("V2.10- 3: " + originalFormatter + '\n').format(size))
    // All will be expanded to
    print(("V2.10- 3: " + originalFormatter + '\n').format(size))
    print((new java.util.Formatter).format("V2.10- 4: " + originalFormatter + '\n', size))
  }

  { // Method III
    // Regular expressions, only for demonstration
    val extractor = """\$\{([^}]+)\}""".r
    println((extractor.replaceAllIn("Regex  1: Mary had a ${x} lamb,", "snow white")))

    // RegEx freaking
    def interpolate(text: String, vars: (String, String)*) =
      extractor.replaceAllIn(text,
        _ match { case Groups(v) => vars.toMap.getOrElse(v, "" /*in case nothing provided*/ ) })

    println(interpolate("Regex  2: ${who} had a ${size} ${pet}, ${unknown}",
      ("pet", "lamb"), ("size", "fat"), ("size", "humongous"), ("who", "Mary")))
  }

  { // Method IV, not recommended.
    // Standard API method, search argument (1st ones) supposed to be a regular expression
    println("Replace1: " + "Mary had a ${x} lamb".replaceAll("""\$\{x\}""", size))
    // Standard API method, literally, on regular expression
    println("Replace2: " + "Mary had a ${x} lamb".replaceAllLiterally("${x}", size))
  }

  { // Method IV, not recommended.
    println("Split   : " + "Mary had a ${x} lamb.".split("""\$\{([^}]+)\}""").mkString(size))
  }
}
