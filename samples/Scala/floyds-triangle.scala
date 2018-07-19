def floydstriangle( n:Int ) {
  val s = (1 to n)
  val t = s map {i => (s take(i-1) sum) + 1}

  (s zip t) foreach { n =>
    var m = n._2;
	
    for( i <- 0 until n._1 ) {
      val w = (t.last + i).toString.length + 1  // Column width from last row
      print("           " + m takeRight w )
      m+=1
    }
	
    print("\n")
  }
}

// Test
floydstriangle(5)
floydstriangle(14)
