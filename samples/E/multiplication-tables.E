  def size := 12
  println(`{|style="border-collapse: collapse; text-align: right;"`)
  println(`|`)
  for x in 1..size {
    println(`|style="border-bottom: 1px solid black; " | $x`)
  }
  for y in 1..size {
    println(`|-`)
      println(`|style="border-right: 1px solid black;" | $y`)
    for x in 1..size {
      println(`| &nbsp;${if (x >= y) { x*y } else {""}}`)
    }
  }
  println("|}")
