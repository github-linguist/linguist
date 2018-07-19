def countChange(amount: Int, coins:List[Int]) = {
	  val ways = Array.fill(amount + 1)(0)
	  ways(0) = 1
	  coins.foreach (coin =>
	  for (j<-coin to amount)
		  ways(j) =  ways(j) + ways(j - coin)
		  )
	ways(amount)
  }

countChange (15, List(1, 5, 10, 25))
