def permute(l: List[Double]): List[List[Double]] = l match {
  case Nil => List(Nil)
  case x :: xs =>
    for {
      ys <- permute(xs)
      position <- 0 to ys.length
      (left, right) = ys splitAt position
    } yield left ::: (x :: right)
}

def computeAllOperations(l: List[Double]): List[(Double,String)] = l match {
  case Nil => Nil
  case x :: Nil => List((x, "%1.0f" format x))
  case x :: xs =>
    for {
      (y, ops) <- computeAllOperations(xs)
      (z, op) <-
        if (y == 0)
          List((x*y, "*"), (x+y, "+"), (x-y, "-"))
        else
          List((x*y, "*"), (x/y, "/"), (x+y, "+"), (x-y, "-"))
    } yield (z, "(%1.0f%s%s)" format (x,op,ops))
}

def hasSolution(l: List[Double]) = permute(l) flatMap computeAllOperations filter (_._1 == 24) map (_._2)
