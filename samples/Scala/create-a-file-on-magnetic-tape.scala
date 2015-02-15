object LinePrinter extends App {
  import java.io.{ FileWriter, IOException }
  {
    val lp0 = new FileWriter("/dev/tape")
    lp0.write("Hello, world!")
    lp0.close()
  }
}
