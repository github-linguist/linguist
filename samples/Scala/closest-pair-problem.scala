import scala.util._

object ClosestPair{
   class Point(x:Double, y:Double) extends Pair(x,y){
      def distance(p:Point)=math.hypot(_1-p._1, _2-p._2)
   }

   def closestPairBF(a:Array[Point])={
      var minDist=a(0) distance a(1)
      var minPoints=(a(0), a(1))

      for(p1<-a; p2<-a if p1.ne(p2); dist=p1 distance p2 if(dist<minDist)){
         minDist=dist;
         minPoints=(p1, p2)
      }
      (minPoints, minDist)
   }

   def main(args: Array[String]): Unit = {
      val a=Array.fill(1000)(new Point(Random.nextDouble, Random.nextDouble))
      val (points, dist)=closestPairBF(a)
      println("min: "+points._1+" - "+points._2+"   ->"+dist)
   }
}
