case class Pair(val name:String, val value:Double)
val a=Array(new Pair("Krypton", 83.798), new Pair("Beryllium", 9.012182), new Pair("Silicon", 28.0855))
a.sortWith((a,b) => a.name.compareTo(b.name) < 0)

//Result:
//  Array(Pair(Beryllium,9.012182), Pair(Krypton,83.798), Pair(Silicon,28.0855))
