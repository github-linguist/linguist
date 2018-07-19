import scala.swing._
import scala.swing.event._
import scala.swing.Swing._

object SimpleApp extends SimpleSwingApplication {
  def top = new MainFrame {
    var nClicks = 0

    val button = new Button {
      text = "click me"
    }
    val label = new Label {
      text = "There have been no clicks yet"
    }
    contents = new BorderPanel {
      layout(button) = BorderPanel.Position.South
      layout(label) = BorderPanel.Position.Center
    }

    preferredSize = ((300, 200): Dimension)

    listenTo(button)
    reactions += {
      case ButtonClicked(_) =>
        nClicks += 1
        label.text = "There have been %d clicks" format nClicks
    }
  }
}
