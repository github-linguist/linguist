object SleepSort {
  def sort(nums:Seq[Int])=nums foreach {n =>
    scala.concurrent.ops.spawn{
      Thread.sleep(500*n)
      print(n+" ")
    }
  }
		
  def main(args:Array[String])={
    sort(args map (_.toInt))
  }
}
