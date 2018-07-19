import scala.annotation.tailrec

case class LearnerParams(target:String,rate:Double,C:Int)

val chars =  ('A' to 'Z') ++ List(' ')
val randgen = new scala.util.Random
def randchar = {
   val charnum = randgen.nextInt(chars.size)
   chars(charnum)
}

class RichTraversable[T](t: Traversable[T]) {
    def maxBy[B](fn: T => B)(implicit ord: Ordering[B]) = t.max(ord on fn)
    def minBy[B](fn: T => B)(implicit ord: Ordering[B]) = t.min(ord on fn)
}

implicit def toRichTraversable[T](t: Traversable[T]) = new RichTraversable(t)

def fitness(candidate:String)(implicit params:LearnerParams) =
   (candidate zip params.target).map { case (a,b) => if (a==b) 1 else 0 }.sum

def mutate(initial:String)(implicit params:LearnerParams) =
   initial.map{ samechar => if(randgen.nextDouble < params.rate) randchar else samechar }

@tailrec
def evolve(generation:Int, initial:String)(implicit params:LearnerParams){
   import params._
   printf("Generation: %3d  %s\n",generation, initial)
   if(initial == target) return ()
   val candidates = for (number <- 1 to C) yield mutate(initial)
   val next = candidates.maxBy(fitness)
   evolve(generation+1,next)
}

implicit val params = LearnerParams("METHINKS IT IS LIKE A WEASEL",0.01,100)
val initial = (1 to params.target.size) map(x => randchar) mkString
evolve(0,initial)
