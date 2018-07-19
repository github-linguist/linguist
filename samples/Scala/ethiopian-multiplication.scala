def ethiopian(i:Int, j:Int):Int=
   pairIterator(i,j).filter(x=> !isEven(x._1)).map(x=>x._2).foldLeft(0){(x,y)=>x+y}

def ethiopian2(i:Int, j:Int):Int=
   pairIterator(i,j).map(x=>if(isEven(x._1)) 0 else x._2).foldLeft(0){(x,y)=>x+y}

def ethiopian3(i:Int, j:Int):Int=
{
   var res=0;
   for((h,d) <- pairIterator(i,j) if !isEven(h)) res+=d;
   res
}

def isEven(x:Int)=(x&1)==0
def halve(x:Int)=x>>>1
def double(x:Int)=x<<1

// generates pairs of values (halve,double)
def pairIterator(x:Int, y:Int)=new Iterator[(Int, Int)]
{
   var i=(x, y)
   def hasNext=i._1>0
   def next={val r=i; i=(halve(i._1), double(i._2)); r}
}
