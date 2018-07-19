import scala.swing._
import scala.swing.Swing._
import scala.actors._
import scala.actors.Actor._

import java.awt.{Color, Graphics}

object Pendulum extends SimpleSwingApplication {

  val length    = 100

  val prefSizeX = 2*length+50
  val prefSizeY = length/2*3

  lazy val ui = new Panel {
    import math._
    background = Color.white
    preferredSize = (prefSizeX, prefSizeY)
    peer.setDoubleBuffered(true)

    var angle: Double = Pi/2;

    def pendular = new Actor {
      var angleAccel, angleVelocity = 0.0;
      var dt = 0.1

      def act() {
        while (true) {
          angleAccel = -9.81 / length * sin(angle)
          angleVelocity += angleAccel * dt
          angle += angleVelocity * dt
          repaint()
          Thread.sleep(15)
        }
      }
    }

    override def paintComponent(g: Graphics2D) = {
      super.paintComponent(g)

      g.setColor(Color.white);
      g.fillRect(0, 0, size.width, size.height);
      val anchorX = size.width / 2
      val anchorY = size.height / 4
      val ballX = anchorX + (sin(angle) * length).toInt
      val ballY = anchorY + (cos(angle) * length).toInt
      g.setColor(Color.lightGray)
      g.drawLine(anchorX-2*length, anchorY, anchorX+2*length, anchorY)
      g.setColor(Color.black)
      g.drawLine(anchorX, anchorY, ballX, ballY)
      g.fillOval(anchorX - 3, anchorY - 4, 7, 7)
      g.drawOval(ballX - 7, ballY - 7, 14, 14)
      g.setColor(Color.yellow)
      g.fillOval(ballX - 7, ballY - 7, 14, 14)
    }
  }

  def top = new MainFrame {
    title = "Rosetta Code >>> Task: Animate a pendulum | Language: Scala"
    contents = ui
    ui.pendular.start
  }

}
