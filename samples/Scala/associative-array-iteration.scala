  val m = Map("Amsterdam" -> "Netherlands", "New York" -> "USA", "Heemstede" -> "Netherlands")

  println(f"Key->Value: ${m.mkString(", ")}%s")
  println(f"Pairs: ${m.toList.mkString(", ")}%s")
  println(f"Keys: ${m.keys.mkString(", ")}%s")
  println(f"Values: ${m.values.mkString(", ")}%s")
  println(f"Unique values: ${m.values.toSet.mkString(", ")}%s")
