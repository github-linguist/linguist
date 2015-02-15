object Array2D{
   def main(args: Array[String]): Unit = {
      val x = Console.readInt
      val y = Console.readInt

      val a=Array.fill(x, y)(0)
      a(0)(0)=42
      println("The number at (0, 0) is "+a(0)(0))
   }
}
