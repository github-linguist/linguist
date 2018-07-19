def priceFraction(x:Double)=x match {
   case n if n>=0 && n<0.06 => 0.10
   case n if n<0.11 => 0.18
   case n if n<0.36 => ((((n*100).toInt-11)/5)*6+26)/100.toDouble
   case n if n<0.96 => ((((n*100).toInt-31)/5)*4+50)/100.toDouble
   case _ => 1.00
}

def testPriceFraction()=
   for(n <- 0.00 to (1.00, 0.01)) println("%.2f  %.2f".format(n, priceFraction(n)))
