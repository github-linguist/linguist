import scala.swing._
import scala.swing.Swing._
import scala.swing.event._

object Interact extends SimpleSwingApplication {
  def top = new MainFrame {
    title = "Rosetta Code >>> Task: component interaction | Language: Scala"

    val numberField = new TextField {
      text = "0" // start at 0
      horizontalAlignment = Alignment.Right
    }

    val incButton = new Button { text = "Increment" }
    val randButton = new Button { text = "Random" }

    // arrange buttons in a grid with 1 row, 2 columns
    val buttonPanel = new GridPanel(1, 2) {
      contents ++= List(incButton, randButton)
    }

    // arrange text field and button panel in a grid with 2 row, 1 column
    contents = new GridPanel(2, 1) { contents ++= List(numberField, buttonPanel) }

    // listen for keys pressed in numberField and button clicks
    listenTo(numberField.keys, incButton, randButton)
    reactions += {
      case kt: KeyTyped if (!kt.char.isDigit) => // if the entered char isn't a digit …
        kt.consume // … eat the event (i.e. stop it from being processed)
      case ButtonClicked(`incButton`) =>
        if (numberField.text.isEmpty) numberField.text = "0"
        // we use BigInt to avoid long overflow/number format exception
        numberField.text = (BigInt(numberField.text) + 1).toString
      case ButtonClicked(`randButton`) =>
        import Dialog._
        numberField.text = showOptions(buttonPanel, message = "Are you sure?",
          title = "Choose an option!", entries = List("Yes", "No", "Cancel"),
          initial = 2) match {
            case Result.Yes => (Long.MaxValue * math.random).toLong.toString
            case _          => numberField.text
          }
    }
    centerOnScreen()
  }
}
