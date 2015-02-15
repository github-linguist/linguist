def mapRange(a1:Double, a2:Double, b1:Double, b2:Double, x:Double):Double=b1+(x-a1)*(b2-b1)/(a2-a1)

for(i <- 0 to 10)
  println("%2d in [0, 10] maps to %5.2f in [-1, 0]".format(i, mapRange(0,10, -1,0, i)))
