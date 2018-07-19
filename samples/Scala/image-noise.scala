import java.awt.event.{ActionEvent, ActionListener}
import swing.{Panel, MainFrame, SimpleSwingApplication}
import javax.swing.Timer
import java.awt.{Font, Color, Graphics2D, Dimension}

object ImageNoise extends SimpleSwingApplication {
  var delay_ms = 2
  var framecount = 0
  var fps = 0

  def top = new MainFrame {
    contents = panel
  }

  val panel = new Panel {
    preferredSize = new Dimension(320, 240)

    override def paintComponent(g: Graphics2D) {
      for (x <- 0 to size.width; y <- 0 to size.height) {
        val c = if (math.random > 0.5) Color.BLACK else Color.WHITE
        g.setColor(c)
        g.fillRect(x, y, 1, 1)
      }
      g.setColor(Color.RED)
      g.setFont(new Font("Monospaced", Font.BOLD, 20))
      g.drawString("FPS: " + fps, size.width - 100, size.height - 10)
      framecount += 1
    }
  }

  val repainter = new Timer(delay_ms, new ActionListener {
    def actionPerformed(e: ActionEvent) {
      panel.repaint
    }
  })

  val framerateChecker = new Timer(1000, new ActionListener {
    def actionPerformed(e: ActionEvent) {
      fps = framecount
      framecount = 0
    }
  })

  repainter.start()
  framerateChecker.start()
}
