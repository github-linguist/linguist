using concurrent
using fwt
using gfx

const class RotateString : Actor
{
  new make (Label label) : super (ActorPool ())
  {
    Actor.locals["rotate-label"] = label
    Actor.locals["rotate-string"] = label.text
    Actor.locals["direction"] = "forward"
    sendLater (1sec, "update")
  }

  // responsible for calling appropriate methods to process each message
  override Obj? receive (Obj? msg)
  {
    switch (msg)
    {
      case "update":
        Desktop.callAsync |->| { update }  // make sure we update GUI in event thread
        sendLater (1sec, "update")
      case "reverse":
        Desktop.callAsync |->| { reverse }
    }

    return null
  }

  // change the stored string indicating the direction to rotate
  Void reverse ()
  {
    Actor.locals["direction"] =
        (Actor.locals["direction"] == "forward" ? "backward" : "forward")
  }

  // update the text on the label according to the stored direction
  Void update ()
  {
    label := Actor.locals["rotate-label"] as Label
    str := Actor.locals["rotate-string"] as Str
    if (label != null)
    {
      newStr := ""
      if (Actor.locals["direction"] == "forward")
        newStr = str[1..-1] + str[0].toChar
      else
        newStr = str[-1].toChar + str[0..<-1]
      label.text = newStr
      Actor.locals["rotate-string"] = newStr
    }
  }
}

class Animate
{
  public static Void main ()
  {
    label := Label
    {
      text = "Hello world! "
      halign = Halign.center
    }
    ticker := RotateString (label)
    label.onMouseDown.add |Event e|
    {
      ticker.send ("reverse")
    }
    Window
    {
      title = "Animate"
      label,
    }.open
  }
}
