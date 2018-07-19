object ShortCircuit {
   def a(b:Boolean)={print("Called A=%5b".format(b));b}
   def b(b:Boolean)={print(" -> B=%5b".format(b));b}

   def main(args: Array[String]): Unit = {
      val boolVals=List(false,true)
      for(aa<-boolVals; bb<-boolVals){
         print("\nTesting A=%5b AND B=%5b   -> ".format(aa, bb))
         a(aa) && b(bb)
      }
      for(aa<-boolVals; bb<-boolVals){
         print("\nTesting A=%5b  OR B=%5b   -> ".format(aa, bb))
         a(aa) || b(bb)
      }
      println
   }
}
