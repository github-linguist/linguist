def rootsOfUnity(n:Int)=for(k <- 0 until n) yield Complex.fromPolar(1.0, 2*math.Pi*k/n)
