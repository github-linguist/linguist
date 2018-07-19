scala> val s1 = Set("John", "Serena", "Bob", "Mary", "Serena")
s1: scala.collection.immutable.Set[java.lang.String] = Set(John, Serena, Bob, Mary)

scala> val s2 = Set("Jim", "Mary", "John", "Jim", "Bob")
s2: scala.collection.immutable.Set[java.lang.String] = Set(Jim, Mary, John, Bob)

scala> (s1 diff s2) union (s2 diff s1)
res46: scala.collection.immutable.Set[java.lang.String] = Set(Serena, Jim)
