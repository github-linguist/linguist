def nonsqr(n:Int)=n+math.round(math.sqrt(n)).toInt
		
for(n<-1 to 22) println(n + "  "+ nonsqr(n))

val test=(1 to 1000000).exists{n =>
   val j=math.sqrt(nonsqr(n))
   j==math.floor(j)
}
println("squares up to one million="+test)
