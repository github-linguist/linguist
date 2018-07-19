def rms(nums: Seq[Int]) = math.sqrt(nums.map(math.pow(_, 2)).sum / nums.size)
println(rms(1 to 10))
