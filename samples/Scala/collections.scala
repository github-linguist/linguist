Windows PowerShell
Copyright (C) 2012 Microsoft Corporation. All rights reserved.

PS C:\Users\FransAdm> scala
Welcome to Scala version 2.10.1 (Java HotSpot(TM) 64-Bit Server VM, Java 1.7.0_25).
Type in expressions to have them evaluated.
Type :help for more information.

scala> // Immutable collections do not and cannot change the instantiated object

scala> // Lets start with Lists

scala> val list = Nil // Empty List
list: scala.collection.immutable.Nil.type = List()

scala> val list2 = List("one", "two") // List with two elements (Strings)
list2: List[String] = List(one, two)

scala> val list3 = 3 :: list2 // prepend 3 to list2, using a special operator
list3: List[Any] = List(3, one, two)

scala> // The result was a mixture with a Int and Strings, so the common superclass Any is used.

scala> // Let test the Set collection

scala> val set = Set.empty[Char] // Empty Set of Char type
set: scala.collection.immutable.Set[Char] = Set()

scala> val set1 = set + 'c' // add an element
set1: scala.collection.immutable.Set[Char] = Set(c)

scala> val set2 = set + 'a' + 'c' + 'c' // try to add another and  the same element twice
set2: scala.collection.immutable.Set[Char] = Set(a, c)

scala> // Let's look at the most universal map: TrieMap (Cache-aware lock-free concurrent hash trie)

scala> val capital = collection.concurrent.TrieMap("US" -> "Washington", "France" -> "Paris") // This map is mutable
capital: scala.collection.concurrent.TrieMap[String,String] = TrieMap(US -> Washington, France -> Paris)

scala> capital - "France" // This is only an expression, does not modify the map itself
res0: scala.collection.concurrent.TrieMap[String,String] = TrieMap(US -> Washington)

scala> capital += ("Tokio" -> "Japan") // Adding an element, object is changed - not the val capital
res1: capital.type = TrieMap(US -> Washington, Tokio -> Japan, France -> Paris)

scala> capital // Check what we have sofar
res2: scala.collection.concurrent.TrieMap[String,String] = TrieMap(US -> Washington, Tokio -> Japan, France -> Paris)

scala>  val queue = new scala.collection.mutable.Queue[String]
queue: scala.collection.mutable.Queue[String] = Queue()

scala> queue += "first"
res17: queue.type = Queue("first")

scala> queue += "second"
res19: queue.type = Queue("first", "second")

scala>
