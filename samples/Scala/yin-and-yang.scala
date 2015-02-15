import scala.swing.Swing.pair2Dimension
import scala.swing.{ MainFrame, Panel }
import java.awt.{ Color, Graphics2D }

object YinYang extends scala.swing.SimpleSwingApplication {
  var preferedSize = 500

  /** Draw a Taijitu symbol on the given graphics context.
   */
  def drawTaijitu(g: Graphics2D, size: Int) {
    val sizeMinsOne = size - 1
    // Preserve the color for the caller
    val colorSave = g.getColor()

    g.setColor(Color.WHITE)
    // Use fillOval to draw a filled in circle
    g.fillOval(0, 0, sizeMinsOne, sizeMinsOne)

    g.setColor(Color.BLACK)
    // Use fillArc to draw part of a filled in circle
    g.fillArc(0, 0, sizeMinsOne, sizeMinsOne, 270, 180)
    g.fillOval(size / 4, size / 2, size / 2, size / 2)

    g.setColor(Color.WHITE)
    g.fillOval(size / 4, 0, size / 2, size / 2)
    g.fillOval(7 * size / 16, 11 * size / 16, size / 8, size / 8)

    g.setColor(Color.BLACK)
    g.fillOval(7 * size / 16, 3 * size / 16, size / 8, size / 8)
    // Use drawOval to draw an empty circle for the outside border
    g.drawOval(0, 0, sizeMinsOne, sizeMinsOne)

    // Restore the color for the caller
    g.setColor(colorSave)
  }

  def top = new MainFrame {
    title = "Rosetta Code >>> Yin Yang Generator | Language: Scala"
    contents = gui(preferedSize)

    def gui(sizeInterior: Int) = new Panel() {
      preferredSize = (sizeInterior, sizeInterior)

      /** Draw a Taijitu symbol in this graphics context.
       */
      override def paintComponent(graphics: Graphics2D) = {
        super.paintComponent(graphics)

        // Color in the background of the image
        background = Color.RED
        drawTaijitu(graphics, sizeInterior)
      }
    } // def gui(
  }

  override def main(args: Array[String]) = {
    preferedSize = args.headOption.map(_.toInt).getOrElse(preferedSize)
    super.main(args)
  }
}
