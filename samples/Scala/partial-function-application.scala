def fs(f:Int=>Int, s:List[Int])=s map f
def f1(x:Int)=x*2
def f2(x:Int)=x*x
		
def fsf1=fs(f1,_:List[Int])
def fsf2=fs(f2,_:List[Int])

println(fsf1(List(0,1,2,3)))
println(fsf1(List(2,4,6,8)))
println(fsf2(List(0,1,2,3)))
println(fsf2(List(2,4,6,8)))
