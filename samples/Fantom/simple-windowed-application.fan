using fwt
using gfx

class SimpleApplication
{
  public static Void main ()
  {
    Window
    {
      title = "Simple Window Application"
      size = Size(350, 50)
      clicked := 0
      label := Label
      {
        text = "There have been no clicks yet"
      }
      Button
      {
        text = "Click me"
        onAction.add |Event e|
        {
          clicked += 1
          label.text = "There have been $clicked clicks"
        }
      },
      label,
    }.open
  }
}
