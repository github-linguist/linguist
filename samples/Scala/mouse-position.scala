import java.awt.MouseInfo

object MousePosition extends App {
  val mouseLocation = MouseInfo.getPointerInfo().getLocation()
  println (mouseLocation)
}
