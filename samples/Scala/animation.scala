import scala.actors.Actor.{actor, loop, reactWithin, exit}
import scala.actors.TIMEOUT
import scala.swing.{SimpleSwingApplication, MainFrame, Label}
import scala.swing.event.MouseClicked

case object Revert

object BasicAnimation extends SimpleSwingApplication {
  val label = new Label("Hello World! ")
  val rotator = actor {
    var goingRight = true
    loop {
      reactWithin(250 /*ms*/) {
        case Revert => goingRight = !goingRight
        case TIMEOUT =>
          if (goingRight)
            label.text = label.text.last + label.text.init
          else
            label.text = label.text.tail + label.text.head
        case unknown => println("Unknown message "+unknown); exit()
      }
    }
  }
  def top = new MainFrame {
    title = "Basic Animation"
    contents = label
  }
  listenTo(label.mouse.clicks) // use "Mouse" instead of "mouse" on Scala 2.7
  reactions += {
    case _ : MouseClicked => rotator ! Revert
  }
}
