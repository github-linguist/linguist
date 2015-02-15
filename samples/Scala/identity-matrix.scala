def identityMatrix(n:Int)=Array.tabulate(n,n)((x,y) => if(x==y) 1 else 0)
def printMatrix[T](m:Array[Array[T]])=m map (_.mkString("[", ", ", "]")) mkString "\n"

printMatrix(identityMatrix(5))
