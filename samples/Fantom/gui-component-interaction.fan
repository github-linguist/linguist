using fwt
using gfx

class GuiComponent
{
  public static Void main ()
  {
    Window
    {
      title = "Rosetta Code: Gui component"
      size = Size(350, 200)
      textField := Text
      {
        onModify.add |Event e|
        {
          Text thisText := e.widget
          if (thisText.text != "") // if nonempty string
          {
            try (thisText.text.toInt) // try converting to int
            catch thisText.text = ""  // clear field if does not work
          }
        }
      }
      GridPane
      {
        numCols = 1
        textField,
        Button
        {
          text = "increment"
          onAction.add |Event e|
          { // make sure there is a number to increment, else set field to 0
            if (textField.text == "")
            {
              textField.text = "0"
            }
            else
            {
              try
              {
                Int x := textField.text.toInt
                textField.text = (x+1).toStr
              }
              catch
              {
                textField.text = "0"
              }
            }
          }
        },
        Button
        {
          text = "random"
          onAction.add |Event e|
          {
            if (Dialog.openQuestion(e.window, "Make number random?", null, Dialog.yesNo) == Dialog.yes)
            {
              textField.text = Int.random(1..10000).toStr
            }
          }
        },
      },
    }.open
  }
}
